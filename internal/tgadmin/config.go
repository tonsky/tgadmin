package tgadmin

import (
	"flag"
	"fmt"
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
	// where known_users lives
	DataDir string
}

// envName maps a flag name to its env var: vote-ttl -> TGADMIN_VOTE_TTL.
// DATA_DIR is set by Houston, so it has no prefix
func envName(flag string) string {
	if flag == "data-dir" {
		return "DATA_DIR"
	}
	return "TGADMIN_" + strings.ToUpper(strings.ReplaceAll(flag, "-", "_"))
}

// loadConfig reads settings from env vars, then from command-line flags,
// so flags win over env vars, and env vars win over defaults
func loadConfig(args []string, getenv func(string) string) (*Config, error) {
	c := &Config{}
	fs := flag.NewFlagSet("tgadmin", flag.ContinueOnError)
	fs.StringVar(&c.Token, "token", "", "Telegram bot token, required")
	fs.BoolVar(&c.Dev, "dev", false, "dev mode: only log bans and whitelisting, treat @nikitonsky as unknown")
	fs.IntVar(&c.VoteLimit, "vote-limit", 3, "votes on either side that decide the outcome")
	fs.DurationVar(&c.VoteTTL, "vote-ttl", 24*time.Hour, "how long a vote stays open")
	fs.IntVar(&c.RepeatedMessagesLimit, "repeated-messages-limit", 3, "same message posted this many times bans the user")
	fs.DurationVar(&c.AdminCacheTTL, "admin-cache-ttl", time.Hour, "how long chat admin lists are cached")
	fs.StringVar(&c.JevAPIKey, "jev-api-key", "", "Jev API key, enables the Jev check")
	fs.Float64Var(&c.JevThreshold, "jev-threshold", 0.7, "Jev spam probability that bans the user")
	fs.Int64Var(&c.DumpGroupID, "dump-group-id", -5016389519, "private group to probe if messages still exist")
	fs.StringVar(&c.DataDir, "data-dir", ".", "directory for known_users")

	var err error
	fs.VisitAll(func(f *flag.Flag) {
		name := envName(f.Name)
		f.Usage += " (env " + name + ")"
		if val := getenv(name); val != "" && err == nil {
			if e := fs.Set(f.Name, val); e != nil {
				err = fmt.Errorf("%s=%q: %w", name, val, e)
			}
		}
	})
	if err != nil {
		return nil, err
	}
	if err := fs.Parse(args); err != nil {
		return nil, err
	}
	if fs.NArg() > 0 {
		return nil, fmt.Errorf("unexpected arguments: %q", fs.Args())
	}
	if c.Token == "" {
		return nil, fmt.Errorf("-token or %s is required", envName("token"))
	}
	return c, nil
}
