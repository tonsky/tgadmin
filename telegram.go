package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"time"
)

// Only the fields the bot uses are declared, the rest is dropped while decoding

type User struct {
	ID        int64  `json:"id"`
	Username  string `json:"username"`
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
}

type Chat struct {
	ID       int64  `json:"id"`
	Username string `json:"username"`
}

type Message struct {
	MessageID      int64    `json:"message_id"`
	From           *User    `json:"from"`
	Chat           Chat     `json:"chat"`
	Text           string   `json:"text"`
	Caption        string   `json:"caption"`
	ReplyToMessage *Message `json:"reply_to_message"`
	NewChatMembers []User   `json:"new_chat_members"`
}

type CallbackQuery struct {
	ID      string   `json:"id"`
	From    User     `json:"from"`
	Message *Message `json:"message"`
	Data    string   `json:"data"`
}

type Update struct {
	UpdateID      int64          `json:"update_id"`
	Message       *Message       `json:"message"`
	CallbackQuery *CallbackQuery `json:"callback_query"`
}

type params map[string]any

type apiResponse struct {
	OK          bool            `json:"ok"`
	Result      json.RawMessage `json:"result"`
	ErrorCode   int             `json:"error_code"`
	Description string          `json:"description"`
}

const (
	// how long getUpdates waits for new updates
	pollTimeout = 50 * time.Second
	apiTimeout  = 30 * time.Second
)

var (
	telegramURL = "https://api.telegram.org"
	httpClient  = &http.Client{}
)

// request sends body as JSON, or a GET if body is nil,
// and returns response status and body
func request(target string, headers map[string]string, body any, timeout time.Duration) (int, []byte, error) {
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	method, payload := http.MethodGet, io.Reader(nil)
	if body != nil {
		data, err := json.Marshal(body)
		if err != nil {
			return 0, nil, err
		}
		method, payload = http.MethodPost, bytes.NewReader(data)
	}
	req, err := http.NewRequestWithContext(ctx, method, target, payload)
	if err != nil {
		return 0, nil, err
	}
	if body != nil {
		req.Header.Set("Content-Type", "application/json")
	}
	for k, v := range headers {
		req.Header.Set(k, v)
	}

	resp, err := httpClient.Do(req)
	if err != nil {
		// url.Error prints the URL, which has bot token in it
		var urlErr *url.Error
		if errors.As(err, &urlErr) {
			err = urlErr.Err
		}
		return 0, nil, err
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(resp.Body)
	return resp.StatusCode, data, err
}

// call sends a Bot API request and returns its response, successful or not
func call(method string, opts params, timeout time.Duration) (*apiResponse, error) {
	_, data, err := request(telegramURL+"/bot"+config.Token+method, nil, opts, timeout)
	if err != nil {
		return nil, err
	}
	var resp apiResponse
	if err := json.Unmarshal(data, &resp); err != nil {
		return nil, err
	}
	return &resp, nil
}

// post calls a Bot API method and decodes the result into out, if given.
// Errors are logged and reported as false
func post(method string, opts params, out any) bool {
	return postTimeout(method, opts, out, apiTimeout)
}

func postTimeout(method string, opts params, out any, timeout time.Duration) bool {
	resp, err := call(method, opts, timeout)
	if err == nil && resp.OK && out != nil {
		err = json.Unmarshal(resp.Result, out)
	}
	switch {
	case err != nil:
		fmt.Println("[ ERROR ]", method, err)
		return false
	case !resp.OK:
		fmt.Println("[ ERROR ]", method, resp.ErrorCode, resp.Description)
		return false
	}
	return true
}

// getUpdates long-polls for updates starting from offset
func getUpdates(offset int64) ([]json.RawMessage, bool) {
	var updates []json.RawMessage
	ok := postTimeout("/getUpdates", params{
		"offset":          offset,
		"timeout":         int(pollTimeout.Seconds()),
		"allowed_updates": []string{"message", "callback_query"},
	}, &updates, pollTimeout+10*time.Second)
	return updates, ok
}
