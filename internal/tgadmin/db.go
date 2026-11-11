package tgadmin

import (
	"bufio"
	"bytes"
	"database/sql"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"time"

	_ "modernc.org/sqlite"
)

// everything the bot remembers lives in one SQLite file in the data dir.
// Default rollback journal: between writes the file is complete on its own
// and can be read or copied by anyone, e.g. sqlite3 or tgadmin dump
const dbFile = "tgadmin.sqlite"

var db *sql.DB

const timeFormat = "2006-01-02 15:04:05" // UTC, sorts as text, understood by SQLite

func now() string {
	return time.Now().UTC().Format(timeFormat)
}

// user statuses
const (
	statusKnown  = "known"
	statusBanned = "banned"
)

// outcomes of checked messages
const (
	outcomeLolsSpam   = "lols_spam"   // banned at lols.bot
	outcomeJevSpam    = "jev_spam"    // banned by Jev
	outcomeJevTrusted = "jev_trusted" // whitelisted by Jev without a vote
	outcomeVoteSpam   = "vote_spam"   // banned by vote or for repeated messages
	outcomeNotSpam    = "not_spam"    // whitelisted by vote
	outcomeRetracted  = "retracted"   // user deleted their messages during the vote
	outcomeExpired    = "expired"     // vote ran out of time
)

func openDB(dir string) (*sql.DB, error) {
	dsn := "file:" + filepath.Join(dir, dbFile) +
		"?_pragma=busy_timeout(10000)&_pragma=foreign_keys(1)&_pragma=synchronous(FULL)&_txlock=immediate"
	d, err := sql.Open("sqlite", dsn)
	if err != nil {
		return nil, err
	}
	// a single connection serializes the update loop and the timers.
	// Never call db.* while holding a transaction or open rows
	d.SetMaxOpenConns(1)
	if err := d.Ping(); err != nil {
		d.Close()
		return nil, err
	}
	return d, nil
}

// inTx runs f in a transaction, committing if it returns nil
func inTx(f func(tx *sql.Tx) error) error {
	tx, err := db.Begin()
	if err != nil {
		return err
	}
	if err := f(tx); err != nil {
		return errors.Join(err, tx.Rollback())
	}
	return tx.Commit()
}

func logErr(err error) {
	if err != nil {
		fmt.Println("[ ERROR ]", err)
	}
}

// migrations

// migrations[i] brings the schema from version i to i+1 (PRAGMA user_version).
// Never change one that has shipped, add a new one instead
var migrations = []func(tx *sql.Tx, dir string) error{
	migrateV1,
}

func migrate(dir string) error {
	var version int
	if err := db.QueryRow("PRAGMA user_version").Scan(&version); err != nil {
		return err
	}
	for ; version < len(migrations); version++ {
		err := inTx(func(tx *sql.Tx) error {
			if err := migrations[version](tx, dir); err != nil {
				return err
			}
			_, err := tx.Exec(fmt.Sprintf("PRAGMA user_version = %d", version+1))
			return err
		})
		if err != nil {
			return fmt.Errorf("migration to version %d: %w", version+1, err)
		}
		fmt.Println("[ MIGRATED ] to version", version+1)
	}
	return nil
}

const schemaV1 = `
CREATE TABLE users (
	id       INTEGER PRIMARY KEY, -- Telegram user ID
	username TEXT,
	name     TEXT,                -- first and last name
	status   TEXT NOT NULL DEFAULT 'unknown', -- unknown, known or banned
	reason   TEXT,                -- why: user vote, jev trusted, banned at lols.bot...
	since    TEXT NOT NULL        -- when status was set or user first seen
);

-- messages that went through the checks: first messages of unknown users
-- and whatever they posted while their vote was open
CREATE TABLE messages (
	chat_id    INTEGER NOT NULL,
	message_id INTEGER NOT NULL,
	user_id    INTEGER NOT NULL REFERENCES users(id),
	date       TEXT NOT NULL,
	raw        TEXT NOT NULL, -- JSON as Telegram sent it
	jev        REAL,          -- spam probability, if asked
	outcome    TEXT,          -- see outcome* constants, NULL while the vote is open
	PRIMARY KEY (chat_id, message_id)
);
CREATE INDEX messages_user ON messages(user_id);

CREATE TABLE votes (
	user_id         INTEGER PRIMARY KEY REFERENCES users(id),
	chat_id         INTEGER NOT NULL,
	vote_message_id INTEGER, -- NULL if posting it failed
	started         TEXT NOT NULL
);

CREATE TABLE ballots (
	user_id  INTEGER NOT NULL REFERENCES votes(user_id) ON DELETE CASCADE,
	voter_id INTEGER NOT NULL, -- voters are mostly never checked, so not in users
	side     TEXT NOT NULL,    -- approve or ban
	PRIMARY KEY (user_id, voter_id)
);
`

// migrateV1 creates the schema and imports known_users and the *.jsonl
// records the bot kept before the database, leaving those files in place
func migrateV1(tx *sql.Tx, dir string) error {
	if _, err := tx.Exec(schemaV1); err != nil {
		return err
	}

	// known_users: "id @username First Last", see userStr
	users := 0
	err := scanLines(filepath.Join(dir, "known_users"), func(line string) error {
		fields := strings.Fields(line)
		if len(fields) == 0 {
			return nil
		}
		id, err := strconv.ParseInt(fields[0], 10, 64)
		if err != nil {
			return nil
		}
		var username string
		if len(fields) > 1 && strings.HasPrefix(fields[1], "@") {
			username, fields = fields[1][1:], fields[1:]
		}
		_, err = tx.Exec(`INSERT INTO users (id, username, name, status, reason, since)
			VALUES (?, ?, ?, ?, 'known_users', ?) ON CONFLICT DO NOTHING`,
			id, nullable(username), nullable(strings.Join(fields[1:], " ")), statusKnown, now())
		users++
		return err
	})
	if err != nil {
		return err
	}

	messages := 0
	for _, file := range []struct{ name, outcome, status, reason string }{
		{"lols_spam.jsonl", outcomeLolsSpam, statusBanned, "banned at lols.bot"},
		{"jev_spam.jsonl", outcomeJevSpam, statusBanned, "spam by jev"},
		{"vote_spam.jsonl", outcomeVoteSpam, statusBanned, "vote"},
		{"jev_trusted.jsonl", outcomeJevTrusted, statusKnown, "jev trusted"},
		{"not_spam.jsonl", outcomeNotSpam, statusKnown, "vote"},
	} {
		err := scanLines(filepath.Join(dir, file.name), func(line string) error {
			var msg Message
			if err := json.Unmarshal([]byte(line), &msg); err != nil || msg.From == nil {
				return nil
			}
			messages++
			if err := saveUser(tx, msg.From, file.status, file.reason); err != nil {
				return err
			}
			return saveMessage(tx, &msg, nil, file.outcome)
		})
		if err != nil {
			return err
		}
	}
	if users+messages > 0 {
		fmt.Println("[ IMPORTED ]", users, "known users,", messages, "messages")
	}
	return nil
}

// scanLines calls f for every line of file, if it exists
func scanLines(path string, f func(line string) error) error {
	file, err := os.Open(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil
	}
	if err != nil {
		return err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	scanner.Buffer(nil, 16<<20) // replies to channel posts carry the whole post
	for scanner.Scan() {
		if err := f(scanner.Text()); err != nil {
			return err
		}
	}
	return scanner.Err()
}

// writes

func nullable(s string) any {
	if s == "" {
		return nil
	}
	return s
}

// saveUser adds user or refreshes their names. Status changes only if given
func saveUser(tx *sql.Tx, user *User, status, reason string) error {
	username, name := nullable(user.Username), nullable(strings.TrimSpace(user.FirstName+" "+user.LastName))
	if status == "" {
		_, err := tx.Exec(`INSERT INTO users (id, username, name, since) VALUES (?, ?, ?, ?)
			ON CONFLICT DO UPDATE SET username = excluded.username, name = excluded.name`,
			user.ID, username, name, now())
		return err
	}
	_, err := tx.Exec(`INSERT INTO users (id, username, name, status, reason, since) VALUES (?, ?, ?, ?, ?, ?)
		ON CONFLICT DO UPDATE SET username = excluded.username, name = excluded.name,
			status = excluded.status, reason = excluded.reason, since = excluded.since`,
		user.ID, username, name, status, reason, now())
	return err
}

// saveMessage stores msg exactly as Telegram sent it. Outcome "" means
// the vote is still open, and saving again only fills in what’s missing
func saveMessage(tx *sql.Tx, msg *Message, jev *float64, outcome string) error {
	var raw bytes.Buffer
	if err := json.Compact(&raw, msg.rawJSON()); err != nil {
		return err
	}
	_, err := tx.Exec(`INSERT INTO messages (chat_id, message_id, user_id, date, raw, jev, outcome)
		VALUES (?, ?, ?, ?, ?, ?, ?)
		ON CONFLICT DO UPDATE SET jev = coalesce(jev, excluded.jev), outcome = coalesce(excluded.outcome, outcome)`,
		msg.Chat.ID, msg.MessageID, msg.From.ID, time.Unix(msg.Date, 0).UTC().Format(timeFormat),
		raw.String(), jev, nullable(outcome))
	return err
}

// record stores how the check of messages ended, first one asked Jev with jev
func record(outcome string, jev *float64, messages []*Message) {
	logErr(inTx(func(tx *sql.Tx) error {
		for i, msg := range messages {
			if err := saveUser(tx, msg.From, "", ""); err != nil {
				return err
			}
			if i > 0 {
				jev = nil
			}
			if err := saveMessage(tx, msg, jev, outcome); err != nil {
				return err
			}
		}
		return nil
	}))
}

// setStatus marks user known or banned, but only logs it in dev mode
func setStatus(user *User, status, reason string) {
	if config.Dev {
		return
	}
	logErr(inTx(func(tx *sql.Tx) error {
		return saveUser(tx, user, status, reason)
	}))
	if status == statusKnown {
		knownUsers[user.ID] = true
	}
}

// reads

func loadKnownUsers() (map[int64]bool, error) {
	rows, err := db.Query("SELECT id FROM users WHERE status = ?", statusKnown)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	users := map[int64]bool{}
	for rows.Next() {
		var id int64
		if err := rows.Scan(&id); err != nil {
			return nil, err
		}
		users[id] = true
	}
	return users, rows.Err()
}

type querier interface {
	Query(query string, args ...any) (*sql.Rows, error)
}

// openMessages returns messages of user’s open vote, oldest first
func openMessages(q querier, userID int64) ([]*Message, error) {
	rows, err := q.Query("SELECT raw FROM messages WHERE user_id = ? AND outcome IS NULL ORDER BY rowid", userID)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var messages []*Message
	for rows.Next() {
		var raw string
		if err := rows.Scan(&raw); err != nil {
			return nil, err
		}
		var msg Message
		if err := json.Unmarshal([]byte(raw), &msg); err != nil {
			return nil, err
		}
		messages = append(messages, &msg)
	}
	return messages, rows.Err()
}

// openVotes returns user IDs with an open vote started by cutoff
func openVotes(cutoff time.Time) ([]int64, error) {
	rows, err := db.Query("SELECT user_id FROM votes WHERE started <= ? ORDER BY started", cutoff.UTC().Format(timeFormat))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var ids []int64
	for rows.Next() {
		var id int64
		if err := rows.Scan(&id); err != nil {
			return nil, err
		}
		ids = append(ids, id)
	}
	return ids, rows.Err()
}

// dump

// dump prints a table as JSON lines, columns in table order, raw messages
// as JSON. Without a table it lists them. Rows are read in small batches,
// each its own short read, so the running bot is never kept waiting
func dump(args []string, getenv func(string) string, out io.Writer) error {
	fs := flag.NewFlagSet("tgadmin dump", flag.ContinueOnError)
	dir := fs.String("data-dir", ".", "directory with "+dbFile+" (env "+envName("data-dir")+")")
	fs.Usage = func() {
		fmt.Fprintln(fs.Output(), "Usage: tgadmin dump [-data-dir dir] [table]")
		fs.PrintDefaults()
	}
	if v := getenv(envName("data-dir")); v != "" {
		*dir = v
	}
	if err := fs.Parse(args); err != nil {
		return err
	}
	if fs.NArg() > 1 {
		return fmt.Errorf("unexpected arguments: %q", fs.Args()[1:])
	}
	if _, err := os.Stat(filepath.Join(*dir, dbFile)); err != nil {
		return err
	}
	var err error
	if db, err = openDB(*dir); err != nil {
		return err
	}
	defer db.Close()

	var tables []string
	rows, err := db.Query("SELECT name FROM sqlite_schema WHERE type = 'table' ORDER BY rowid")
	if err != nil {
		return err
	}
	for rows.Next() {
		var name string
		rows.Scan(&name)
		tables = append(tables, name)
	}
	rows.Close()
	if fs.NArg() == 0 {
		_, err := fmt.Fprintln(out, strings.Join(tables, "\n"))
		return err
	}
	table := fs.Arg(0)
	if !slices.Contains(tables, table) {
		return fmt.Errorf("no table %q, there are: %s", table, strings.Join(tables, ", "))
	}

	w := bufio.NewWriter(out)
	defer w.Flush()
	for after := int64(-1 << 63); ; {
		n, last, err := dumpBatch(w, table, after)
		if err != nil {
			return err
		}
		if n == 0 {
			return nil
		}
		after = last
	}
}

func dumpBatch(w io.Writer, table string, after int64) (n int, last int64, err error) {
	rows, err := db.Query(`SELECT rowid AS "_rowid_", * FROM "`+table+`" WHERE rowid > ? ORDER BY rowid LIMIT 1000`, after)
	if err != nil {
		return 0, 0, err
	}
	defer rows.Close()
	columns, err := rows.Columns()
	if err != nil {
		return 0, 0, err
	}
	values := make([]any, len(columns))
	ptrs := make([]any, len(columns))
	for i := range values {
		ptrs[i] = &values[i]
	}
	for rows.Next() {
		if err := rows.Scan(ptrs...); err != nil {
			return 0, 0, err
		}
		last = values[0].(int64)
		var line bytes.Buffer
		line.WriteByte('{')
		for i, column := range columns[1:] {
			value := values[i+1]
			if b, ok := value.([]byte); ok {
				value = string(b)
			}
			if i > 0 {
				line.WriteByte(',')
			}
			writeJSON(&line, column)
			line.WriteByte(':')
			// raw messages are stored as compact JSON already
			if s, ok := value.(string); ok && column == "raw" && json.Valid([]byte(s)) {
				line.WriteString(s)
			} else if err := writeJSON(&line, value); err != nil {
				return 0, 0, err
			}
		}
		line.WriteString("}\n")
		if _, err := w.Write(line.Bytes()); err != nil {
			return 0, 0, err
		}
		n++
	}
	return n, last, rows.Err()
}

// writeJSON writes v like json.Marshal, but keeps <, > and & as they are
func writeJSON(buf *bytes.Buffer, v any) error {
	encoder := json.NewEncoder(buf)
	encoder.SetEscapeHTML(false)
	if err := encoder.Encode(v); err != nil {
		return err
	}
	buf.Truncate(buf.Len() - 1) // Encode adds a newline
	return nil
}
