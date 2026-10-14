package main

import (
	"errors"
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"
)

type Config struct {
	Dev                   bool
	Token                 string
	VoteLimit             int
	VoteTTL               time.Duration
	RepeatedMessagesLimit int
	AdminCacheTTL         time.Duration
	// https://docs.typesafe.ai, skipped if not set
	JevAPIKey    string
	JevThreshold float64
	// private group used to probe if messages still exist, bot must be a member
	DumpGroupID int64
}

func loadConfig(path string) (*Config, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	m, err := readEDN(string(data))
	if err != nil {
		return nil, fmt.Errorf("%s: %w", path, err)
	}

	c := &Config{
		VoteLimit:             3,
		VoteTTL:               24 * time.Hour,
		RepeatedMessagesLimit: 3,
		AdminCacheTTL:         time.Hour,
		JevThreshold:          0.7,
		DumpGroupID:           -5016389519,
	}
	for key, val := range m {
		if val == nil {
			continue
		}
		// most settings are numbers, the rest overwrite ok
		num, ok := val.(float64)
		switch key {
		case "dev?":
			c.Dev, ok = val.(bool)
		case "token":
			c.Token, ok = val.(string)
		case "jev-api-key":
			c.JevAPIKey, ok = val.(string)
		case "vote-limit":
			c.VoteLimit = int(num)
		case "repeated-messages-limit":
			c.RepeatedMessagesLimit = int(num)
		case "vote-ttl":
			c.VoteTTL = time.Duration(num) * time.Millisecond
		case "admin-cache-ttl":
			c.AdminCacheTTL = time.Duration(num) * time.Millisecond
		case "jev-threshold":
			c.JevThreshold = num
		case "dump-group-id":
			c.DumpGroupID = int64(num)
		default:
			// unknown keys are ignored
			ok = true
		}
		if !ok {
			return nil, fmt.Errorf("%s: unexpected value for :%s: %v", path, key, val)
		}
	}
	if c.Token == "" {
		return nil, fmt.Errorf("%s: :token is required", path)
	}
	return c, nil
}

// readEDN reads a flat EDN map of keywords to strings, numbers, booleans
// and nil, which is all config.edn ever contains. Keys lose their colon
func readEDN(src string) (map[string]any, error) {
	r := &ednReader{src: src}
	r.skip()
	if !r.eat('{') {
		return nil, errors.New("expected a map")
	}
	m := map[string]any{}
	for {
		r.skip()
		if r.eat('}') {
			return m, nil
		}
		key := r.token()
		if !strings.HasPrefix(key, ":") {
			return nil, fmt.Errorf("expected a keyword, got %q", key)
		}
		r.skip()
		val, err := r.value()
		if err != nil {
			return nil, fmt.Errorf("%s: %w", key, err)
		}
		m[key[1:]] = val
	}
}

type ednReader struct {
	src string
	pos int
}

func (r *ednReader) eat(c byte) bool {
	if r.pos < len(r.src) && r.src[r.pos] == c {
		r.pos++
		return true
	}
	return false
}

// skip advances past whitespace, commas and comments
func (r *ednReader) skip() {
	for r.pos < len(r.src) {
		switch r.src[r.pos] {
		case ';':
			for r.pos < len(r.src) && r.src[r.pos] != '\n' {
				r.pos++
			}
		case ' ', '\t', '\r', '\n', ',':
			r.pos++
		default:
			return
		}
	}
}

// token reads up to the next delimiter
func (r *ednReader) token() string {
	start := r.pos
	for r.pos < len(r.src) && !strings.ContainsRune(" \t\r\n,;{}\"", rune(r.src[r.pos])) {
		r.pos++
	}
	return r.src[start:r.pos]
}

func (r *ednReader) value() (any, error) {
	if r.pos < len(r.src) && r.src[r.pos] == '"' {
		end := r.pos + 1
		for end < len(r.src) && r.src[end] != '"' {
			if r.src[end] == '\\' {
				end++
			}
			end++
		}
		if end >= len(r.src) {
			return nil, errors.New("unterminated string")
		}
		s, err := strconv.Unquote(r.src[r.pos : end+1])
		r.pos = end + 1
		return s, err
	}
	switch tok := r.token(); tok {
	case "true":
		return true, nil
	case "false":
		return false, nil
	case "nil":
		return nil, nil
	default:
		n, err := strconv.ParseFloat(tok, 64)
		if err != nil {
			return nil, fmt.Errorf("unsupported value %q", tok)
		}
		return n, nil
	}
}
