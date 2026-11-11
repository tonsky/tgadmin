package tgadmin

import (
	"bytes"
	"database/sql"
	_ "embed"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"os"
	"runtime/debug"
	"slices"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

var config *Config

// state, besides the database (see db.go)

// whitelisted user IDs, a cache of known users in the database,
// only touched by the update loop
var knownUsers map[int64]bool

type adminList struct {
	admins  map[int64]bool
	fetched time.Time
}

// chat admins by chat ID, only touched by the update loop
var adminCache = map[int64]adminList{}

// utils

// safely keeps the bot running if f panics
func safely(f func()) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Printf("[ ERROR ] %v\n%s", r, debug.Stack())
		}
	}()
	f()
}

func trim(s string) string {
	if r := []rune(s); len(r) > 80 {
		return string(r[:80]) + "..."
	}
	return s
}

func userStr(user *User) string {
	s := strconv.FormatInt(user.ID, 10)
	if user.Username != "" {
		s += " @" + user.Username
	}
	if user.FirstName != "" {
		s += " " + user.FirstName
	}
	if user.LastName != "" {
		s += " " + user.LastName
	}
	return s
}

func messageStr(msg *Message) string {
	s := fmt.Sprintf("%s/%d by %d", msg.Chat.Username, msg.MessageID, msg.From.ID)
	if msg.From.Username != "" {
		s += " (" + msg.From.Username + ")"
	}
	return s
}

// app

var lolsURL = "https://api.lols.bot"

func checkExternal(userID int64) string {
	status, data, err := request(fmt.Sprintf("%s/account?id=%d", lolsURL, userID), nil, nil, 10*time.Second)
	if err != nil {
		fmt.Println("[ ERROR ] lols.bot", err)
		return ""
	}
	if status != 200 {
		return ""
	}
	var body struct {
		Banned bool `json:"banned"`
	}
	if err := json.Unmarshal(data, &body); err != nil {
		fmt.Println("[ ERROR ] lols.bot", err)
		return ""
	}
	if body.Banned {
		return "banned at lols.bot"
	}
	return ""
}

// messageExists probes if msg is still there. There’s no getMessage in Bot API,
// so we forward message to a dump group and see if it fails. Deleted messages
// give “message to forward not found” in basic groups but “MESSAGE_ID_INVALID”
// in supergroups. Any other error counts as existing
func messageExists(msg *Message) bool {
	resp, err := call("/forwardMessage", params{
		"chat_id":              config.DumpGroupID,
		"from_chat_id":         msg.Chat.ID,
		"message_id":           msg.MessageID,
		"disable_notification": true,
	}, apiTimeout)
	if err != nil {
		fmt.Println("[ ERROR ] /forwardMessage", err)
		return true
	}
	if !resp.OK {
		return !strings.Contains(resp.Description, "not found") &&
			!strings.Contains(resp.Description, "MESSAGE_ID_INVALID")
	}
	var forwarded Message
	if err := json.Unmarshal(resp.Result, &forwarded); err == nil {
		post("/deleteMessage", params{"chat_id": config.DumpGroupID, "message_id": forwarded.MessageID}, nil)
	}
	return true
}

//go:embed jev_prompt.txt
var jevPrompt string

var jevURL = "https://api.typesafe.ai/v1/systemone"

// what Jev gets to see of a message
type jevMessage struct {
	Text           string      `json:"text,omitempty"`
	Caption        string      `json:"caption,omitempty"`
	Links          []string    `json:"links,omitempty"`
	ForwardedFrom  string      `json:"forwarded_from,omitempty"`
	From           jevUser     `json:"from"`
	ReplyToMessage *jevMessage `json:"reply_to_message,omitempty"`
}

type jevUser struct {
	FirstName string `json:"first_name,omitempty"`
	LastName  string `json:"last_name,omitempty"`
	Username  string `json:"username,omitempty"`
}

func jevSelect(msg *Message) *jevMessage {
	res := &jevMessage{Text: msg.Text, Caption: msg.Caption}
	// URLs hidden behind link text
	for _, entity := range append(msg.Entities, msg.CaptionEntities...) {
		if entity.Type == "text_link" {
			res.Links = append(res.Links, entity.URL)
		}
	}
	if origin := msg.ForwardOrigin; origin != nil {
		switch {
		case origin.Chat != nil:
			res.ForwardedFrom = chatStr(origin.Chat)
		case origin.SenderChat != nil:
			res.ForwardedFrom = chatStr(origin.SenderChat)
		case origin.SenderUser != nil:
			res.ForwardedFrom = strings.TrimSpace(origin.SenderUser.FirstName + " " + origin.SenderUser.LastName)
		default:
			res.ForwardedFrom = origin.SenderUserName
		}
	}
	if from := msg.From; from != nil {
		res.From = jevUser{from.FirstName, from.LastName, from.Username}
	}
	return res
}

func chatStr(chat *Chat) string {
	if chat.Username == "" {
		return chat.Title
	}
	return strings.TrimSpace(chat.Title + " @" + chat.Username)
}

// checkJev returns spam probability of msg, or nil if Jev is off or failed
func checkJev(msg *Message) *float64 {
	if config.JevAPIKey == "" {
		return nil
	}
	state := jevSelect(msg)
	if msg.ReplyToMessage != nil {
		state.ReplyToMessage = jevSelect(msg.ReplyToMessage)
	}
	_, data, err := request(jevURL,
		map[string]string{"Authorization": "Bearer " + config.JevAPIKey},
		params{
			"model":     "jev-latest",
			"state":     state,
			"questions": params{"spam": params{"type": "noul", "instructions": jevPrompt}},
		},
		15*time.Second)
	if err != nil {
		fmt.Println("[ ERROR ] jev", err)
		return nil
	}
	var body struct {
		Answers struct {
			Spam struct {
				Noul *float64 `json:"noul"`
			} `json:"spam"`
		} `json:"answers"`
	}
	err = json.Unmarshal(data, &body)
	noul := body.Answers.Spam.Noul
	if err != nil || noul == nil {
		fmt.Println("[ ERROR ] jev", string(data))
		return nil
	}
	fmt.Println("[ JEV ]", messageStr(msg), "spam probability", *noul)
	return noul
}

func whitelistUser(user *User, reason string) {
	fmt.Println("[ WHITELIST ]", userStr(user), "for", reason)
	setStatus(user, statusKnown, reason)
}

func banUser(user *User, reason string, messages []*Message) {
	chatID := messages[0].Chat.ID
	for _, msg := range messages {
		fmt.Println("[ DELETING ]", messageStr(msg), "for", reason)
		post("/deleteMessage", params{"chat_id": chatID, "message_id": msg.MessageID}, nil)
	}
	fmt.Println("[ BAN ]", userStr(user), "for", reason)
	if !config.Dev {
		post("/banChatMember", params{"chat_id": chatID, "user_id": user.ID}, nil)
	}
	setStatus(user, statusBanned, reason)
}

func getAdmins(chatID int64) map[int64]bool {
	cached, ok := adminCache[chatID]
	if ok && time.Since(cached.fetched) < config.AdminCacheTTL {
		return cached.admins
	}
	var members []struct {
		User User `json:"user"`
	}
	post("/getChatAdministrators", params{"chat_id": chatID}, &members)
	admins := map[int64]bool{}
	var usernames []string
	for _, member := range members {
		admins[member.User.ID] = true
		usernames = append(usernames, member.User.Username)
	}
	fmt.Println("[ ADMINS ] for", chatID, strings.Join(usernames, ", "))
	adminCache[chatID] = adminList{admins, time.Now()}
	return admins
}

func voteKeyboard(userID int64, approveCount, banCount int) params {
	return params{"inline_keyboard": [][]params{{
		{"text": fmt.Sprintf("🤖 Бот (%d)", banCount), "callback_data": fmt.Sprintf("ban:%d", userID)},
		{"text": fmt.Sprintf("🧑 Не бот (%d)", approveCount), "callback_data": fmt.Sprintf("approve:%d", userID)},
	}}}
}

// startVote posts the vote on msg, the first message of an unknown user
func startVote(msg *Message, jev *float64) {
	user := msg.From
	fmt.Println("[ VOTE ]", messageStr(msg))
	mention := strings.TrimSpace(user.FirstName + " " + user.LastName)
	if mention == "" {
		mention = user.Username
	}
	if mention == "" {
		mention = strconv.FormatInt(user.ID, 10)
	}
	var voteMessage *Message
	post("/sendMessage", params{
		"chat_id":          msg.Chat.ID,
		"reply_parameters": params{"message_id": msg.MessageID},
		"text":             "Голосуем! " + mention + " — бот или нет?",
		"reply_markup":     voteKeyboard(user.ID, 0, 0),
	}, &voteMessage)

	var voteMessageID any
	if voteMessage != nil {
		voteMessageID = voteMessage.MessageID
	}
	logErr(inTx(func(tx *sql.Tx) error {
		if err := saveUser(tx, user, "", ""); err != nil {
			return err
		}
		if err := saveMessage(tx, msg, jev, ""); err != nil {
			return err
		}
		_, err := tx.Exec("INSERT INTO votes (user_id, chat_id, vote_message_id, started) VALUES (?, ?, ?, ?)",
			user.ID, msg.Chat.ID, voteMessageID, now())
		return err
	}))
}

// a vote taken out of the database by closeVote
type closedVote struct {
	messages      []*Message
	chatID        int64
	voteMessageID *int64 // nil if posting it failed
}

// closeVote atomically ends user’s vote, settling its messages with outcome.
// Returns the vote, or nil if there’s none: someone else has closed it.
// Every vote ends here, so only one of those racing to close it acts on it
func closeVote(userID int64, outcome string) *closedVote {
	var vote *closedVote
	logErr(inTx(func(tx *sql.Tx) error {
		var v closedVote
		err := tx.QueryRow("DELETE FROM votes WHERE user_id = ? RETURNING chat_id, vote_message_id", userID).
			Scan(&v.chatID, &v.voteMessageID)
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		if v.messages, err = openMessages(tx, userID); err != nil {
			return err
		}
		if _, err := tx.Exec("UPDATE messages SET outcome = ? WHERE user_id = ? AND outcome IS NULL", outcome, userID); err != nil {
			return err
		}
		if len(v.messages) > 0 {
			vote = &v
		}
		return nil
	}))
	return vote
}

// resolveVote closes user’s vote and deletes the vote message
func resolveVote(userID int64, outcome string) *closedVote {
	vote := closeVote(userID, outcome)
	if vote != nil && vote.voteMessageID != nil {
		post("/deleteMessage", params{"chat_id": vote.chatID, "message_id": *vote.voteMessageID}, nil)
	}
	return vote
}

func handleCallbackQuery(query *CallbackQuery) {
	answer := func(text string) {
		opts := params{"callback_query_id": query.ID}
		if text != "" {
			opts["text"] = text
		}
		post("/answerCallbackQuery", opts, nil)
	}

	action, target, _ := strings.Cut(query.Data, ":")
	targetID, err := strconv.ParseInt(target, 10, 64)
	if err != nil || !(action == "approve" || action == "ban") || query.Message == nil {
		answer("")
		return
	}
	voterID := query.From.ID
	chatID := query.Message.Chat.ID
	fmt.Println("[ VOTED ]", userStr(&query.From), action, targetID)

	// the voter’s current side, if any
	var side sql.NullString
	err = db.QueryRow("SELECT (SELECT side FROM ballots WHERE user_id = ? AND voter_id = ?) FROM votes WHERE user_id = ?",
		targetID, voterID, targetID).Scan(&side)
	switch {
	// not in progress
	case errors.Is(err, sql.ErrNoRows):
		answer("")
		return

	case err != nil:
		logErr(err)
		answer("")
		return

	// repeated vote
	case side.String == action:
		answer("Вы уже проголосовали")
		return
	}

	answer("")
	admins := getAdmins(chatID)

	var approveCount, banCount int
	var adminApproved, adminBanned, open bool
	logErr(inTx(func(tx *sql.Tx) error {
		// might have been resolved while we were talking to Telegram
		_, err := tx.Exec(`INSERT INTO ballots (user_id, voter_id, side) SELECT ?, ?, ? WHERE EXISTS (SELECT 1 FROM votes WHERE user_id = ?)
			ON CONFLICT DO UPDATE SET side = excluded.side`, targetID, voterID, action, targetID)
		if err != nil {
			return err
		}
		rows, err := tx.Query("SELECT voter_id, side FROM ballots WHERE user_id = ?", targetID)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var voter int64
			var side string
			if err := rows.Scan(&voter, &side); err != nil {
				return err
			}
			open = true
			if side == "approve" {
				approveCount++
				adminApproved = adminApproved || admins[voter]
			} else {
				banCount++
				adminBanned = adminBanned || admins[voter]
			}
		}
		return rows.Err()
	}))
	if !open {
		return
	}

	reason := "user vote"
	if adminApproved || adminBanned {
		reason = "admin vote"
	}
	switch {
	case approveCount >= config.VoteLimit || adminApproved:
		if vote := resolveVote(targetID, outcomeNotSpam); vote != nil {
			whitelistUser(vote.messages[0].From, reason)
		}

	case banCount >= config.VoteLimit || adminBanned:
		if vote := resolveVote(targetID, outcomeVoteSpam); vote != nil {
			banUser(vote.messages[0].From, reason, vote.messages)
		}

	// update counts
	default:
		post("/editMessageReplyMarkup", params{
			"chat_id":      chatID,
			"message_id":   query.Message.MessageID,
			"reply_markup": voteKeyboard(targetID, approveCount, banCount),
		}, nil)
	}
}

// collectMessage adds msg to its author’s open vote, if there is one,
// and returns how many times their most repeated message was posted
func collectMessage(msg *Message) (maxFreq int, open bool) {
	logErr(inTx(func(tx *sql.Tx) error {
		err := tx.QueryRow("SELECT 1 FROM votes WHERE user_id = ?", msg.From.ID).Scan(new(int))
		if errors.Is(err, sql.ErrNoRows) {
			return nil
		}
		if err != nil {
			return err
		}
		open = true
		if err := saveMessage(tx, msg, nil, ""); err != nil {
			return err
		}
		messages, err := openMessages(tx, msg.From.ID)
		if err != nil {
			return err
		}
		freqs := map[string]int{}
		for _, m := range messages {
			if utf8.RuneCountInString(m.Text) > 10 {
				freqs[m.Text]++
				maxFreq = max(maxFreq, freqs[m.Text])
			}
		}
		return nil
	}))
	return maxFreq, open
}

func handleMessage(msg *Message) {
	user := msg.From
	switch {
	// service messages (joins, etc.)
	case user == nil || len(msg.NewChatMembers) > 0:
		return

	// known
	case knownUsers[user.ID] && !(config.Dev && user.Username == "nikitonsky"):
		return
	}

	// open vote -- collect messages
	if maxFreq, open := collectMessage(msg); open {
		if maxFreq >= config.RepeatedMessagesLimit {
			if vote := resolveVote(user.ID, outcomeVoteSpam); vote != nil {
				banUser(user, "repeated messages", vote.messages)
			}
		}
		return
	}

	// unknown -- first message goes through the checks
	logMessage(msg)

	// banned by lols
	if reason := checkExternal(user.ID); reason != "" {
		record(outcomeLolsSpam, nil, []*Message{msg})
		banUser(user, reason, []*Message{msg})
		return
	}

	// spam by jev
	jev := checkJev(msg)
	if jev != nil && *jev >= config.JevThreshold {
		record(outcomeJevSpam, jev, []*Message{msg})
		banUser(user, "spam by jev", []*Message{msg})
		return
	}

	// trusted by jev: an old account writing something Jev is sure is not spam
	if jev != nil && *jev < config.JevTrustThreshold && trusted(msg) {
		fmt.Println("[ TRUSTED ]", messageStr(msg))
		record(outcomeJevTrusted, jev, []*Message{msg})
		whitelistUser(user, "jev trusted")
		return
	}

	// start community vote
	startVote(msg, jev)
}

// trusted tells if msg can skip the vote when Jev is sure it’s not spam.
// Spam comes mostly from new accounts, and Telegram IDs grow with account
// age. Short messages give Jev too little to judge
func trusted(msg *Message) bool {
	letters := 0
	for _, r := range msg.Text + msg.Caption {
		if unicode.IsLetter(r) || unicode.IsDigit(r) {
			letters++
		}
	}
	return msg.From.ID < config.TrustMaxUserID && letters >= 10
}

// checkPendingVotes re-checks every user with an open vote: bans if lols.bot
// now reports them, retracts the vote if user deleted all their messages
func checkPendingVotes() {
	userIDs, err := openVotes(time.Now())
	if err != nil {
		logErr(err)
		return
	}
	for _, userID := range userIDs {
		messages, err := openMessages(db, userID)
		if err != nil || len(messages) == 0 {
			logErr(err)
			continue
		}
		user := messages[0].From
		if reason := checkExternal(userID); reason != "" {
			// reported as spammer while vote was in progress
			if vote := resolveVote(userID, outcomeLolsSpam); vote != nil {
				banUser(user, reason, vote.messages)
			}
		} else if !slices.ContainsFunc(messages, messageExists) {
			// user deleted all their messages -- retract vote, forget user
			if resolveVote(userID, outcomeRetracted) != nil {
				fmt.Println("[ RETRACTED ]", userStr(user))
			}
		}
	}
}

// cleanupPendingVotes drops votes older than vote-ttl, leaving
// the vote message in place and the user unknown
func cleanupPendingVotes() {
	userIDs, err := openVotes(time.Now().Add(-config.VoteTTL))
	logErr(err)
	for _, userID := range userIDs {
		if closeVote(userID, outcomeExpired) != nil {
			fmt.Println("[ CLEANUP ]", userID)
		}
	}
}

func runTimers() {
	minute := time.NewTicker(time.Minute)
	hour := time.NewTicker(time.Hour)
	for {
		select {
		case <-minute.C:
			safely(checkPendingVotes)
		case <-hour.C:
			safely(cleanupPendingVotes)
		}
	}
}

// logMessage prints a message that goes through the spam checks
func logMessage(msg *Message) {
	raw := msg.rawJSON()
	var message map[string]any
	decoder := json.NewDecoder(bytes.NewReader(raw))
	decoder.UseNumber()
	if err := decoder.Decode(&message); err != nil {
		fmt.Println("[ CHECK ]", string(raw))
		return
	}
	// replies to channel posts carry the whole post
	if reply, ok := message["reply_to_message"].(map[string]any); ok {
		if text, ok := reply["text"].(string); ok {
			reply["text"] = trim(text)
		}
	}
	var buf bytes.Buffer
	encoder := json.NewEncoder(&buf)
	encoder.SetEscapeHTML(false)
	encoder.Encode(message)
	fmt.Print("[ CHECK ] ", buf.String())
}

var retryDelay = 5 * time.Second

func handleUpdate(update *Update) {
	switch {
	case update.Message != nil:
		handleMessage(update.Message)
	case update.CallbackQuery != nil:
		handleCallbackQuery(update.CallbackQuery)
	}
}

// poll handles the next batch of updates and returns the offset to continue from
func poll(offset int64) int64 {
	updates, ok := getUpdates(offset)
	if !ok {
		// Telegram is down or another instance is polling, don’t spin
		time.Sleep(retryDelay)
		return offset
	}
	for _, raw := range updates {
		var update Update
		if err := json.Unmarshal(raw, &update); err != nil {
			fmt.Println("[ ERROR ]", err, string(raw))
		}
		offset = max(offset, update.UpdateID+1)
		safely(func() { handleUpdate(&update) })
	}
	return offset
}

// start opens the database in the data dir, bringing it up to date
func start() error {
	if err := os.MkdirAll(config.DataDir, 0o755); err != nil {
		return err
	}
	var err error
	if db, err = openDB(config.DataDir); err != nil {
		return err
	}
	if err := migrate(config.DataDir); err != nil {
		return err
	}
	knownUsers, err = loadKnownUsers()
	return err
}

func Main() {
	if len(os.Args) > 1 && os.Args[1] == "dump" {
		err := dump(os.Args[2:], os.Getenv, os.Stdout)
		if errors.Is(err, flag.ErrHelp) {
			os.Exit(0)
		}
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		return
	}

	var err error
	if config, err = loadConfig(os.Args[1:], os.Getenv); err == nil {
		err = start()
	}
	if errors.Is(err, flag.ErrHelp) {
		os.Exit(0)
	}
	if err != nil {
		fmt.Println("[ ERROR ]", err)
		os.Exit(1)
	}

	fmt.Println("[ STARTED ]")
	go runTimers()

	var offset int64
	for {
		offset = poll(offset)
	}
}
