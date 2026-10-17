package tgadmin

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path"
	"path/filepath"
	"reflect"
	"slices"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"
)

const (
	chatID  = -100
	voteID  = 1000 // message_id of every vote message
	adminID = 1
)

// fake stands in for Telegram, lols.bot and Jev
type fake struct {
	mu       sync.Mutex
	calls    []string       // Telegram calls so far, see describe
	spammers map[int64]bool // banned at lols.bot
	deleted  map[int64]bool // message IDs that are gone
	noul     float64        // Jev’s answer
	updates  string         // next getUpdates result
	jevState any            // what Jev was asked about
}

func describe(method string, opts map[string]any) string {
	switch method {
	case "deleteMessage":
		return fmt.Sprintf("delete %v/%v", opts["chat_id"], opts["message_id"])
	case "banChatMember":
		return fmt.Sprintf("ban %v", opts["user_id"])
	case "sendMessage":
		return fmt.Sprintf("send %v", opts["text"])
	case "answerCallbackQuery":
		text, _ := opts["text"].(string)
		return strings.TrimSpace("answer " + text)
	case "getUpdates":
		return fmt.Sprintf("getUpdates %v %v %v", opts["offset"], opts["timeout"], opts["allowed_updates"])
	case "editMessageReplyMarkup":
		row := opts["reply_markup"].(map[string]any)["inline_keyboard"].([]any)[0].([]any)
		return fmt.Sprintf("edit %v / %v", row[0].(map[string]any)["text"], row[1].(map[string]any)["text"])
	}
	return method
}

func (f *fake) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	defer f.mu.Unlock()

	var body map[string]any
	if r.Method == http.MethodPost {
		decoder := json.NewDecoder(r.Body)
		decoder.UseNumber()
		if err := decoder.Decode(&body); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
	}

	switch r.URL.Path {
	case "/lols/":
		id, _ := strconv.ParseInt(r.URL.Query().Get("a"), 10, 64)
		fmt.Fprintf(w, `{"banned":%v}`, f.spammers[id])
		return
	case "/jev":
		f.jevState = body["state"]
		fmt.Fprintf(w, `{"answers":{"spam":{"noul":%v}}}`, f.noul)
		return
	}

	method := path.Base(r.URL.Path)
	f.calls = append(f.calls, describe(method, body))
	result := "true"
	switch method {
	case "getUpdates":
		result, f.updates = f.updates, "[]"
	case "sendMessage":
		result = fmt.Sprintf(`{"message_id":%d,"chat":{"id":%d}}`, voteID, chatID)
	case "getChatAdministrators":
		result = fmt.Sprintf(`[{"user":{"id":%d,"username":"admin"}}]`, adminID)
	case "forwardMessage":
		if id, _ := body["message_id"].(json.Number).Int64(); f.deleted[id] {
			fmt.Fprint(w, `{"ok":false,"error_code":400,"description":"Bad Request: message to forward not found"}`)
			return
		}
		result = `{"message_id":2000,"chat":{"id":-1}}`
	}
	fmt.Fprintf(w, `{"ok":true,"result":%s}`, result)
}

// expect checks Telegram calls made since the last expect
func (f *fake) expect(t *testing.T, want ...string) {
	t.Helper()
	f.mu.Lock()
	defer f.mu.Unlock()
	if !reflect.DeepEqual(f.calls, want) {
		t.Errorf("calls:\n got %q\nwant %q", f.calls, want)
	}
	f.calls = nil
}

func setup(t *testing.T) *fake {
	f := &fake{spammers: map[int64]bool{}, deleted: map[int64]bool{}, updates: "[]"}
	server := httptest.NewServer(f)
	t.Cleanup(server.Close)
	telegramURL, lolsURL, jevURL = server.URL, server.URL+"/lols", server.URL+"/jev"

	t.Chdir(t.TempDir())
	var err error
	if config, err = loadConfig([]string{"-token", "token", "-dump-group-id", "-1", "-data-dir", t.TempDir()}, noEnv); err != nil {
		t.Fatal(err)
	}
	knownUsers = map[int64]bool{}
	pendingVotes = map[int64]*pendingVote{}
	adminCache = map[int64]adminList{}
	return f
}

func message(id, userID int64, text string) *Message {
	return &Message{
		MessageID: id,
		From:      &User{ID: userID, FirstName: "User"},
		Chat:      Chat{ID: chatID, Username: "chat"},
		Text:      text,
	}
}

func vote(action string, voterID, targetID int64) *CallbackQuery {
	return &CallbackQuery{
		ID:      "callback",
		From:    User{ID: voterID},
		Message: &Message{MessageID: voteID, Chat: Chat{ID: chatID}},
		Data:    fmt.Sprintf("%s:%d", action, targetID),
	}
}

const voteText = "send Голосуем! User — бот или нет?"

// expectRecords checks message texts in file, one message per line
func expectRecords(t *testing.T, file string, want ...string) {
	t.Helper()
	data, _ := os.ReadFile(filepath.Join(config.DataDir, file))
	got := []string{}
	for _, line := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		if line == "" {
			continue
		}
		var msg Message
		if err := json.Unmarshal([]byte(line), &msg); err != nil || msg.MessageID == 0 {
			t.Fatalf("%s: not a message: %s, %v", file, line, err)
		}
		got = append(got, msg.Text)
	}
	if !slices.Equal(got, append([]string{}, want...)) {
		t.Errorf("%s:\n got %q\nwant %q", file, got, want)
	}
}

func TestKnownUser(t *testing.T) {
	f := setup(t)
	knownUsers[5] = true
	handleMessage(message(10, 5, "hi"))
	handleMessage(&Message{MessageID: 11, Chat: Chat{ID: chatID}, NewChatMembers: []User{{ID: 6}}, From: &User{ID: 6}})
	f.expect(t)
}

func TestBannedAtLols(t *testing.T) {
	f := setup(t)
	f.spammers[5] = true
	handleMessage(message(10, 5, "hi"))
	f.expect(t, "delete -100/10", "ban 5")
	if len(pendingVotes) != 0 {
		t.Error("vote started for a banned user")
	}
	expectRecords(t, lolsSpamFile, "hi")
}

func TestJev(t *testing.T) {
	f := setup(t)
	config.JevAPIKey = "key"

	f.noul = 0.7
	msg := message(10, 5, "hi")
	msg.From.Username = "user"
	msg.ReplyToMessage = message(9, 4, "post")
	msg.ReplyToMessage.ReplyToMessage = message(8, 3, "older post")
	handleMessage(msg)
	f.expect(t, "delete -100/10", "ban 5")
	want := map[string]any{
		"text": "hi",
		"from": map[string]any{"first_name": "User", "username": "user"},
		"reply_to_message": map[string]any{
			"text": "post",
			"from": map[string]any{"first_name": "User"},
		},
	}
	if !reflect.DeepEqual(f.jevState, want) {
		t.Errorf("jev state:\n got %v\nwant %v", f.jevState, want)
	}

	expectRecords(t, jevSpamFile, "hi")

	f.noul = 0.69
	handleMessage(message(11, 6, "hi"))
	f.expect(t, voteText)
}

func TestApprove(t *testing.T) {
	f := setup(t)
	handleMessage(message(10, 5, "hi"))
	f.expect(t, voteText)

	handleCallbackQuery(vote("approve", 11, 5))
	f.expect(t, "answer", "getChatAdministrators", "edit 🤖 Бот (0) / 🧑 Не бот (1)")

	// repeated vote
	handleCallbackQuery(vote("approve", 11, 5))
	f.expect(t, "answer Вы уже проголосовали")

	// changed their mind, twice
	handleCallbackQuery(vote("ban", 11, 5))
	f.expect(t, "answer", "edit 🤖 Бот (1) / 🧑 Не бот (0)")
	handleCallbackQuery(vote("approve", 11, 5))
	f.expect(t, "answer", "edit 🤖 Бот (0) / 🧑 Не бот (1)")

	handleCallbackQuery(vote("approve", 12, 5))
	f.expect(t, "answer", "edit 🤖 Бот (0) / 🧑 Не бот (2)")
	handleCallbackQuery(vote("approve", 13, 5))
	f.expect(t, "answer", "delete -100/1000")

	if data, _ := os.ReadFile(knownUsersPath()); string(data) != "5 User\n" {
		t.Errorf("known_users: %q", data)
	}
	expectRecords(t, notSpamFile, "hi")

	// late vote, garbage, and the user is known now
	handleCallbackQuery(vote("ban", 14, 5))
	handleCallbackQuery(vote("nope", 14, 5))
	f.expect(t, "answer", "answer")
	handleMessage(message(11, 5, "hi again"))
	f.expect(t)
}

func TestAdminBan(t *testing.T) {
	f := setup(t)
	handleMessage(message(10, 5, "hi"))
	handleMessage(message(11, 5, "hi again"))
	f.expect(t, voteText)

	handleCallbackQuery(vote("ban", adminID, 5))
	f.expect(t, "answer", "getChatAdministrators", "delete -100/1000", "delete -100/10", "delete -100/11", "ban 5")
	if knownUsers[5] || len(pendingVotes) != 0 {
		t.Error("banned user is still around")
	}
	expectRecords(t, voteSpamFile, "hi", "hi again")
}

func TestRepeatedMessages(t *testing.T) {
	f := setup(t)
	// short messages don’t count
	for id := int64(10); id < 14; id++ {
		handleMessage(message(id, 5, "hi"))
	}
	handleMessage(message(14, 5, "buy my stuff"))
	handleMessage(message(15, 5, "buy my stuff"))
	f.expect(t, voteText)

	handleMessage(message(16, 5, "buy my stuff"))
	f.expect(t, "delete -100/1000",
		"delete -100/10", "delete -100/11", "delete -100/12", "delete -100/13",
		"delete -100/14", "delete -100/15", "delete -100/16", "ban 5")
	expectRecords(t, voteSpamFile, "hi", "hi", "hi", "hi", "buy my stuff", "buy my stuff", "buy my stuff")
}

func TestCheckPendingVotes(t *testing.T) {
	f := setup(t)
	handleMessage(message(10, 5, "hi"))
	f.expect(t, voteText)

	// nothing changed, only the probe
	checkPendingVotes()
	f.expect(t, "forwardMessage", "delete -1/2000")

	// reported as spammer while vote was in progress
	f.spammers[5] = true
	checkPendingVotes()
	f.expect(t, "delete -100/1000", "delete -100/10", "ban 5")
	expectRecords(t, lolsSpamFile, "hi")

	// user deleted one message, then all of them
	handleMessage(message(20, 6, "hi"))
	handleMessage(message(21, 6, "hi again"))
	f.expect(t, voteText)
	f.deleted[20] = true
	checkPendingVotes()
	f.expect(t, "forwardMessage", "forwardMessage", "delete -1/2000")
	f.deleted[21] = true
	checkPendingVotes()
	f.expect(t, "forwardMessage", "forwardMessage", "delete -100/1000")
	if knownUsers[6] || len(pendingVotes) != 0 {
		t.Error("vote was not retracted")
	}
	expectRecords(t, notSpamFile)
	expectRecords(t, voteSpamFile)
}

func TestCleanupPendingVotes(t *testing.T) {
	f := setup(t)
	handleMessage(message(10, 5, "hi"))
	handleMessage(message(20, 6, "hi"))
	pendingVotes[5].added = time.Now().Add(-25 * time.Hour)
	cleanupPendingVotes()
	f.expect(t, voteText, voteText)
	if pendingVotes[5] != nil || pendingVotes[6] == nil {
		t.Errorf("pending votes: %v", pendingVotes)
	}
}

func TestDevMode(t *testing.T) {
	f := setup(t)
	config.Dev = true
	knownUsers[5] = true
	msg := message(10, 5, "hi")
	msg.From.Username = "nikitonsky"
	handleMessage(msg)
	f.expect(t, voteText)

	handleCallbackQuery(vote("approve", adminID, 5))
	f.expect(t, "answer", "getChatAdministrators", "delete -100/1000")
	if _, err := os.Stat(knownUsersPath()); err == nil {
		t.Error("known_users written in dev mode")
	}

	handleMessage(msg)
	handleCallbackQuery(vote("ban", adminID, 5))
	f.expect(t, voteText, "answer", "delete -100/1000", "delete -100/10")
}

func TestPoll(t *testing.T) {
	f := setup(t)
	f.updates = `[
		{"update_id": 558841693, "message": {"message_id": 13, "from": {"id": 232806939, "is_bot": false, "first_name": "Nikita", "last_name": "Prokopov", "username": "nikitonsky"}, "chat": {"id": -100, "title": "Grumpy <Queue>", "username": "grumpy_queue", "type": "supergroup"}, "date": 1698933272, "message_thread_id": 8, "reply_to_message": {"message_id": 8, "from": {"id": 777000, "is_bot": false, "first_name": "Telegram"}, "chat": {"id": -100, "type": "supergroup"}, "date": 1698933181, "text": "` + strings.Repeat("пост ", 100) + `"}, "text": "reply", "entities": [{"offset": 0, "length": 5, "type": "bold"}]}},
		{"update_id": 558841694, "edited_message": {"message_id": 13}},
		{"update_id": 558841695, "callback_query": {"id": "4382bfdwdsb323b2d9", "from": {"id": 1, "is_bot": false, "first_name": "Admin"}, "message": {"message_id": 1000, "chat": {"id": -100, "type": "supergroup"}, "date": 1698933300, "text": "vote"}, "chat_instance": "1", "data": "approve:232806939"}}
	]`
	offset := poll(0)
	if offset != 558841696 {
		t.Errorf("offset: %d", offset)
	}
	f.expect(t, "getUpdates 0 50 [message callback_query]",
		"send Голосуем! Nikita Prokopov — бот или нет?",
		"answer", "getChatAdministrators", "delete -100/1000")
	if data, _ := os.ReadFile(knownUsersPath()); string(data) != "232806939 @nikitonsky Nikita Prokopov\n" {
		t.Errorf("known_users: %q", data)
	}
	// messages are recorded as Telegram sent them
	expectRecords(t, notSpamFile, "reply")
	data, _ := os.ReadFile(filepath.Join(config.DataDir, notSpamFile))
	for _, want := range []string{`{"message_id":13,"from":{"id":232806939,`, `"title":"Grumpy <Queue>"`, `"message_thread_id":8`, `"entities":[{"offset":0,"length":5,"type":"bold"}]}` + "\n"} {
		if !strings.Contains(string(data), want) {
			t.Errorf("%s: no %s in %s", notSpamFile, want, data)
		}
	}

	if offset = poll(offset); offset != 558841696 {
		t.Errorf("offset: %d", offset)
	}
	f.expect(t, "getUpdates 558841696 50 [message callback_query]")
}

func noEnv(string) string { return "" }

func TestLoadConfig(t *testing.T) {
	env := map[string]string{
		"TGADMIN_TOKEN":         "123:abc",
		"TGADMIN_DEV":           "true",
		"TGADMIN_VOTE_TTL":      "1m",
		"TGADMIN_JEV_THRESHOLD": "0.5",
		"TGADMIN_VOTE_LIMIT":    "5",
		"DATA_DIR":              "/data",
		"OTHER":                 "ignored",
	}
	getenv := func(name string) string { return env[name] }

	// flags win over env vars, env vars win over defaults
	config, err := loadConfig([]string{"-vote-limit", "2", "-token=456:def"}, getenv)
	want := &Config{
		Dev:                   true,
		Token:                 "456:def",
		VoteLimit:             2,
		VoteTTL:               time.Minute,
		RepeatedMessagesLimit: 3,
		AdminCacheTTL:         time.Hour,
		JevThreshold:          0.5,
		DumpGroupID:           -5016389519,
		DataDir:               "/data",
	}
	if err != nil || !reflect.DeepEqual(config, want) {
		t.Errorf("config:\n got %+v, %v\nwant %+v", config, err, want)
	}

	config, err = loadConfig([]string{"-data-dir", "/other"}, getenv)
	if err != nil || config.DataDir != "/other" {
		t.Errorf("-data-dir: %+v, %v", config, err)
	}
	config, err = loadConfig([]string{"-token", "abc"}, noEnv)
	if err != nil || config.DataDir != "." {
		t.Errorf("default data dir: %+v, %v", config, err)
	}

	for _, tc := range []struct {
		args []string
		env  map[string]string
	}{
		{nil, nil},
		{[]string{"-dev"}, nil},
		{[]string{"-token", ""}, nil},
		{nil, map[string]string{"TGADMIN_DEV": "true"}},
		{[]string{"-token", "abc", "-vote-limit", "three"}, nil},
		{[]string{"-token", "abc", "-vote-ttl", "60000"}, nil},
		{[]string{"-token", "abc", "-unknown", "1"}, nil},
		{[]string{"-token", "abc", "extra"}, nil},
		{[]string{"-token", "abc"}, map[string]string{"TGADMIN_DEV": "yes"}},
		{[]string{"-token", "abc"}, map[string]string{"TGADMIN_DUMP_GROUP_ID": "group"}},
	} {
		getenv := func(name string) string { return tc.env[name] }
		if config, err := loadConfig(tc.args, getenv); err == nil {
			t.Errorf("%q %v: expected an error, got %+v", tc.args, tc.env, config)
		}
	}
}

func TestLoadKnownUsers(t *testing.T) {
	t.Chdir(t.TempDir())
	users, err := loadKnownUsers("known_users")
	if err != nil || len(users) != 0 {
		t.Errorf("missing file: %v, %v", users, err)
	}

	os.WriteFile("known_users", []byte("1 @first First\n-2\n\nnobody\n3"), 0o644)
	users, err = loadKnownUsers("known_users")
	want := map[int64]bool{1: true, -2: true, 3: true}
	if err != nil || !reflect.DeepEqual(users, want) {
		t.Errorf("known users: %v, %v", users, err)
	}
}
