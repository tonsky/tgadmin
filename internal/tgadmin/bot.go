package tgadmin

import (
	"bufio"
	"bytes"
	_ "embed"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime/debug"
	"slices"
	"strconv"
	"strings"
	"sync"
	"time"
	"unicode/utf8"
)

var config *Config

// state

// whitelisted user IDs, only touched by the update loop
var knownUsers map[int64]bool

type pendingVote struct {
	messages    []*Message
	voteMessage *Message       // nil if posting it failed
	approve     map[int64]bool // voter IDs
	ban         map[int64]bool
	added       time.Time
}

// open votes by user ID, shared between the update loop and the timers.
// A vote taken out of the map by resolveVote belongs to whoever took it
var (
	votesMu      sync.Mutex
	pendingVotes = map[int64]*pendingVote{}
)

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

var lolsURL = "https://lols.bot"

func checkExternal(userID int64) string {
	status, data, err := request(fmt.Sprintf("%s/?a=%d", lolsURL, userID), nil, nil, 10*time.Second)
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
	if from := msg.From; from != nil {
		res.From = jevUser{from.FirstName, from.LastName, from.Username}
	}
	return res
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

func knownUsersPath() string {
	return filepath.Join(config.DataDir, "known_users")
}

func loadKnownUsers(path string) (map[int64]bool, error) {
	users := map[int64]bool{}
	f, err := os.Open(path)
	if errors.Is(err, fs.ErrNotExist) {
		return users, nil
	}
	if err != nil {
		return nil, err
	}
	defer f.Close()
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		// lines are user IDs optionally followed by names, see userStr
		var id int64
		if n, _ := fmt.Sscanf(scanner.Text(), "%d", &id); n == 1 {
			users[id] = true
		}
	}
	return users, scanner.Err()
}

func whitelistUser(user *User) {
	fmt.Println("[ WHITELIST ]", userStr(user))
	if config.Dev {
		return
	}
	knownUsers[user.ID] = true
	if err := appendLine(knownUsersPath(), []byte(userStr(user)+"\n")); err != nil {
		fmt.Println("[ ERROR ]", err)
	}
}

func appendLine(path string, line []byte) error {
	f, err := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0o644)
	if err != nil {
		return err
	}
	_, err = f.Write(line)
	return errors.Join(err, f.Close())
}

// files in the data dir with checked messages by outcome, one message per line
// as Telegram sent it, to tune the Jev prompt later
const (
	lolsSpamFile = "lols_spam.jsonl"
	jevSpamFile  = "jev_spam.jsonl"
	voteSpamFile = "vote_spam.jsonl"
	notSpamFile  = "not_spam.jsonl"
)

func record(file string, messages []*Message) {
	var buf bytes.Buffer
	for _, msg := range messages {
		if err := json.Compact(&buf, msg.rawJSON()); err != nil {
			fmt.Println("[ ERROR ]", err)
			return
		}
		buf.WriteByte('\n')
	}
	if err := appendLine(filepath.Join(config.DataDir, file), buf.Bytes()); err != nil {
		fmt.Println("[ ERROR ]", err)
	}
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

func startVote(msg *Message) {
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

	votesMu.Lock()
	defer votesMu.Unlock()
	pendingVotes[user.ID] = &pendingVote{
		messages:    []*Message{msg},
		voteMessage: voteMessage,
		approve:     map[int64]bool{},
		ban:         map[int64]bool{},
		added:       time.Now(),
	}
}

// resolveVote removes user from pending votes and deletes vote message.
// Returns pending vote or nil if already resolved by someone else
func resolveVote(userID int64) *pendingVote {
	votesMu.Lock()
	pending := pendingVotes[userID]
	delete(pendingVotes, userID)
	votesMu.Unlock()

	if pending == nil {
		return nil
	}
	if voteMessage := pending.voteMessage; voteMessage != nil {
		post("/deleteMessage", params{"chat_id": voteMessage.Chat.ID, "message_id": voteMessage.MessageID}, nil)
	}
	return pending
}

func anyOf(voters, admins map[int64]bool) bool {
	for id := range voters {
		if admins[id] {
			return true
		}
	}
	return false
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
	approve := action == "approve"
	if err != nil || !(approve || action == "ban") || query.Message == nil {
		answer("")
		return
	}
	voterID := query.From.ID
	chatID := query.Message.Chat.ID
	fmt.Println("[ VOTED ]", userStr(&query.From), action, targetID)

	votesMu.Lock()
	pending := pendingVotes[targetID]
	repeated := pending != nil && (approve && pending.approve[voterID] || !approve && pending.ban[voterID])
	votesMu.Unlock()

	switch {
	// not in progress
	case pending == nil:
		answer("")
		return

	// repeated vote
	case repeated:
		answer("Вы уже проголосовали")
		return
	}

	answer("")
	admins := getAdmins(chatID)

	votesMu.Lock()
	pending = pendingVotes[targetID]
	if pending == nil {
		// resolved while we were talking to Telegram
		votesMu.Unlock()
		return
	}
	if approve {
		pending.approve[voterID] = true
		delete(pending.ban, voterID)
	} else {
		pending.ban[voterID] = true
		delete(pending.approve, voterID)
	}
	approveCount, banCount := len(pending.approve), len(pending.ban)
	adminApproved, adminBanned := anyOf(pending.approve, admins), anyOf(pending.ban, admins)
	approved := approveCount >= config.VoteLimit || adminApproved
	banned := banCount >= config.VoteLimit || adminBanned
	targetUser := pending.messages[0].From
	votesMu.Unlock()

	reason := "user vote"
	if adminApproved || adminBanned {
		reason = "admin vote"
	}
	switch {
	case approved:
		if pending := resolveVote(targetID); pending != nil {
			whitelistUser(targetUser)
			record(notSpamFile, pending.messages)
		}

	case banned:
		if pending := resolveVote(targetID); pending != nil {
			banUser(targetUser, reason, pending.messages)
			record(voteSpamFile, pending.messages)
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

// collectMessage adds msg to its author’s pending vote, if there is one,
// and returns how many times their most repeated message was posted
func collectMessage(msg *Message) (maxFreq int, pending bool) {
	votesMu.Lock()
	defer votesMu.Unlock()
	vote := pendingVotes[msg.From.ID]
	if vote == nil {
		return 0, false
	}
	vote.messages = append(vote.messages, msg)
	freqs := map[string]int{}
	for _, m := range vote.messages {
		if utf8.RuneCountInString(m.Text) > 10 {
			freqs[m.Text]++
			maxFreq = max(maxFreq, freqs[m.Text])
		}
	}
	return maxFreq, true
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

	// pending -- collect messages
	if maxFreq, pending := collectMessage(msg); pending {
		if maxFreq >= config.RepeatedMessagesLimit {
			if pending := resolveVote(user.ID); pending != nil {
				banUser(user, "repeated messages", pending.messages)
				record(voteSpamFile, pending.messages)
			}
		}
		return
	}

	// unknown -- first message goes through the checks
	logMessage(msg)

	// banned by lols
	if reason := checkExternal(user.ID); reason != "" {
		banUser(user, reason, []*Message{msg})
		record(lolsSpamFile, []*Message{msg})
		return
	}

	// spam by jev
	jev := checkJev(msg)
	if jev != nil && *jev >= config.JevThreshold {
		banUser(user, "spam by jev", []*Message{msg})
		record(jevSpamFile, []*Message{msg})
		return
	}

	// start community vote
	startVote(msg)
}

// checkPendingVotes re-checks every user with an open vote: bans if lols.bot
// now reports them, retracts the vote if user deleted all their messages
func checkPendingVotes() {
	votesMu.Lock()
	snapshot := make(map[int64][]*Message, len(pendingVotes))
	for userID, pending := range pendingVotes {
		snapshot[userID] = slices.Clone(pending.messages)
	}
	votesMu.Unlock()

	for userID, messages := range snapshot {
		user := messages[0].From
		if reason := checkExternal(userID); reason != "" {
			// reported as spammer while vote was in progress
			if pending := resolveVote(userID); pending != nil {
				banUser(user, reason, pending.messages)
				record(lolsSpamFile, pending.messages)
			}
		} else if !slices.ContainsFunc(messages, messageExists) {
			// user deleted all their messages -- retract vote, forget user
			if resolveVote(userID) != nil {
				fmt.Println("[ RETRACTED ]", userStr(user))
			}
		}
	}
}

func cleanupPendingVotes() {
	votesMu.Lock()
	defer votesMu.Unlock()
	for userID, pending := range pendingVotes {
		if time.Since(pending.added) > config.VoteTTL {
			fmt.Println("[ CLEANUP ]", userID)
			delete(pendingVotes, userID)
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

func Main() {
	var err error
	if config, err = loadConfig(os.Args[1:], os.Getenv); err == nil {
		err = os.MkdirAll(config.DataDir, 0o755)
	}
	if err == nil {
		knownUsers, err = loadKnownUsers(knownUsersPath())
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
