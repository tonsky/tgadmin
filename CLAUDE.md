# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Telegram moderation bot written in Go that detects and bans spam accounts in Telegram groups. New users are checked against an external spam database and (optionally) the Jev spam classifier, and their first message triggers a community vote ("bot or not?") that decides whether to whitelist or ban them.

Besides the standard library it has one dependency: `modernc.org/sqlite`, a pure-Go SQLite (no cgo, so `go build` works anywhere).

## Key Commands

- **Run the bot**: `./script/run.sh` or `go run ./cmd/tgadmin` (flags are passed through)
- **Run tests**: `./script/test.sh` (`go vet` + `go test -race`)
- **Build binary**: `./script/package.sh` (creates `target/tgadmin`)
- **Dump the database**: `tgadmin dump [-data-dir dir]` lists tables, `tgadmin dump [-data-dir dir] <table>` prints one as JSON lines (`raw` messages as JSON). Reads in small batches, safe to run next to the live bot. On the server: `cd ~/tgadmin && current/tgadmin dump -data-dir data messages`; `sqlite3 data/tgadmin.sqlite` works too

Pushing to `main` runs the tests and a linux/amd64 build in CI (`.github/workflows/build.yml`). Deploys are done by Houston, which builds on the server with `go build -o tgadmin ./cmd/tgadmin`.

## Architecture

### Core Components

1. **Code** (standard Go layout):
   - `cmd/tgadmin/main.go` — the `main` package, just calls `tgadmin.Main`
   - `internal/tgadmin/` — everything else, one package:
   - `bot.go` — all bot logic:
     - Long-polling loop over Telegram `getUpdates` (`Main`, `poll`)
     - Community vote system for first-time posters
     - External spam-database checks (lols.bot)
     - Jev (TypeSafe) spam classification of first messages
     - User management (whitelisting, banning)
   - `telegram.go` — Bot API types (only the fields the bot uses) and the HTTP wrapper
   - `db.go` — SQLite storage: schema, migrations, reads/writes, `tgadmin dump`
   - `config.go` — config loading from env vars and command-line flags (stdlib `flag`)
   - `bot_test.go` — tests that run the handlers against a fake Telegram/lols.bot/Jev server

2. **Configuration**:
   - Settings come from command-line flags and `TGADMIN_*` env vars; a flag wins over its env var, which wins over the default. Each flag `-some-name` has env var `TGADMIN_SOME_NAME`: `-token` (required), `-dev`, `-vote-limit` (default 3), `-vote-ttl` (Go duration, default `24h`), `-repeated-messages-limit` (default 3), `-admin-cache-ttl` (Go duration, default `1h`), `-dump-group-id`, `-jev-api-key` (optional, enables the Jev check), `-jev-threshold` (default 0.6), `-jev-trust-threshold` (default 0.05), `-trust-max-user-id` (default 5000000000), and `-data-dir` (default `.`), whose env var is `DATA_DIR` without the prefix, because Houston sets it. `tgadmin -h` lists them
   - `mise.local.toml` — local env vars for development (token, keys, `TGADMIN_DEV`), gitignored
   - `tgadmin.sqlite` in the data dir — SQLite database with everything the bot remembers, see State below. Default rollback journal (no WAL), `synchronous=FULL`, one connection per process
   - `known_users`, `*.jsonl` in the data dir — what the bot kept before the database; imported by the first migration and left in place, no longer written
   - `internal/tgadmin/jev_prompt.txt` — the Jev prompt, embedded into the binary

3. **State** — tables in `tgadmin.sqlite`, schema in `db.go`:
   - `users` — everyone the checks have seen: `status` (`unknown`, `known` = whitelisted, `banned`), `reason`, `since`, display `username` and `name`
   - `messages` — every checked message, `raw` exactly as Telegram sent it, Jev's `jev` score if asked, and `outcome` (`lols_spam`, `jev_spam`, `jev_trusted`, `vote_spam`, `not_spam`, `retracted`, `expired`; NULL while the user's vote is open). Written in dev mode too. Meant for tuning the Jev prompt
   - `votes` (open votes) and `ballots` (who voted which side) — votes survive restarts

   Migrations: `migrations` in `db.go`, one function per schema version, tracked in `PRAGMA user_version`; never edit one that has shipped, append a new one.

   In memory: `knownUsers` (cache of known user IDs, loaded at start) and `adminCache` (chat admin lists), both only touched by the update loop.

   Two goroutines run: the update loop and the timers (`runTimers`). They share only the database; `db` has a single connection, so all access is serialized and transactions (`inTx`) make each change atomic. Never call `db.*` while holding a transaction or open rows (it would deadlock), and never hold a transaction across network calls.

### Bot Algorithm

1. Known users pass through untouched
2. Unknown users are checked against the https://lols.bot spam database; if banned there, they are banned immediately
3. If `jev-api-key` is set, their first message is sent to Jev (https://docs.typesafe.ai) as a yes/no “is this spam?” question with the prompt from `jev_prompt.txt`; a spam probability of `jev-threshold` or higher bans them immediately. Without the key, or on any API error, the step is skipped
   - If the spam probability is below `jev-trust-threshold`, the user ID is below `trust-max-user-id` (IDs grow with account age, spam comes from new accounts) and the message has at least 10 letters or digits, the user is whitelisted without a vote (`trusted`). Evaluated on ~2000 labelled messages: no spam passed, ~38% of human votes skipped
4. Otherwise the bot replies to their first message with a vote: inline keyboard "🤖 Бот / 🧑 Не бот"
   - `vote-limit` votes on either side decide the outcome; a single admin vote decides immediately
   - Approve → whitelist; ban → delete all their messages and ban
5. While a vote is pending, further messages from the user are collected; posting the same message `repeated-messages-limit` times gets them banned outright
6. A timer re-checks every user with an open vote once a minute:
   - re-queries lols.bot — if now reported as spammer, closes the vote and bans
   - probes whether their messages still exist — if the user deleted all of them, the vote is retracted and the user stays unknown
7. Pending votes older than `vote-ttl` are cleaned up hourly (vote message stays, user stays unknown)

Note: the Bot API sends no update when messages are deleted, so deletion is detected by probing with `forwardMessage` into a private dump group (`dump-group-id` setting; the forward is deleted right away). A "message to forward not found" (or "MESSAGE_ID_INVALID" in supergroups) error means the message is gone; any other error fails open (`messageExists`).

### Key Functions

- `handleMessage` — main entry point for incoming messages
- `handleCallbackQuery` — processes vote button presses
- `checkExternal` — queries lols.bot by user ID
- `checkJev` — asks Jev whether a first message is spam (only text/caption, hidden link URLs, the forward source, sender name/username and the replied-to message are sent)
- `trusted` — whether a message Jev is sure about can skip the vote (old account, enough text)
- `startVote` — posts the vote message for a first-time poster
- `closeVote` — atomically ends a vote and sets its messages' outcome; whoever gets the vote back acts on it. `resolveVote` also deletes the vote message; every vote outcome goes through one of them
- `checkPendingVotes` — the per-minute re-check (lols.bot + deleted-messages probe)
- `banUser` — deletes user's messages and bans them
- `whitelistUser` — marks user known (`setStatus`)
- `record` — stores checked messages with their outcome
- `post` — Telegram API call wrapper (decodes the result, returns false on error)

### Dev Mode

With `-dev` (or `TGADMIN_DEV=true`), the actual ban call and user status changes are skipped (logged only; message deletion and recording messages still happen), and messages from `nikitonsky` are treated as coming from an unknown user for testing.

### Logging

Only the first message of an unknown user, the one that goes through the checks, is printed in full (`[ CHECK ] <json>`). Everything else is one-line events: `[ VOTE ]`, `[ VOTED ]`, `[ TRUSTED ]`, `[ BAN ]`, `[ WHITELIST ]` and so on.
