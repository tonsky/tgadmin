# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Telegram moderation bot written in Go that detects and bans spam accounts in Telegram groups. New users are checked against an external spam database and (optionally) the Jev spam classifier, and their first message triggers a community vote ("bot or not?") that decides whether to whitelist or ban them.

It only uses the standard library, there are no dependencies.

## Key Commands

- **Run the bot**: `./script/run.sh` or `go run .`
- **Run tests**: `./script/test.sh` (`go vet` + `go test -race`)
- **Build binary**: `./script/package.sh` (creates `target/tgadmin`)

Pushing to `main` builds a linux/amd64 binary, copies it to the server and restarts the service (`.github/workflows/build.yml`).

## Architecture

### Core Components

1. **Code** (single `main` package in the repo root):
   - `main.go` — all bot logic:
     - Long-polling loop over Telegram `getUpdates` (`main`, `poll`)
     - Community vote system for first-time posters
     - External spam-database checks (lols.bot)
     - Jev (TypeSafe) spam classification of first messages
     - User management (whitelisting, banning)
   - `telegram.go` — Bot API types (only the fields the bot uses) and the HTTP wrapper
   - `config.go` — config loading from env vars and command-line flags (stdlib `flag`)
   - `main_test.go` — tests that run the handlers against a fake Telegram/lols.bot/Jev server

2. **Configuration**:
   - Settings come from command-line flags and `TGADMIN_*` env vars; a flag wins over its env var, which wins over the default. Each flag `-some-name` has env var `TGADMIN_SOME_NAME`: `-token` (required), `-dev`, `-vote-limit` (default 3), `-vote-ttl` (Go duration, default `24h`), `-repeated-messages-limit` (default 3), `-admin-cache-ttl` (Go duration, default `1h`), `-dump-group-id`, `-jev-api-key` (optional, enables the Jev check), `-jev-threshold` (default 0.7). `tgadmin -h` lists them
   - `mise.local.toml` — local env vars for development (token, keys, `TGADMIN_DEV`), gitignored
   - `known_users` — persistent append-only storage of whitelisted user IDs
   - `jev_prompt.txt` — the Jev prompt, embedded into the binary

3. **State Management** (package-level variables in `main.go`):
   - `knownUsers` — whitelisted user IDs
   - `pendingVotes` — open votes by user ID: `messages`, `voteMessage`, `approve`/`ban` voter sets, `added`
   - `adminCache` — cached chat admin lists

   Two goroutines run: the update loop and the timers (`runTimers`). `pendingVotes` is the only state they share and is guarded by `votesMu`; the mutex is never held across network calls. `knownUsers` and `adminCache` are only touched by the update loop.

### Bot Algorithm

1. Known users pass through untouched
2. Unknown users are checked against the https://lols.bot spam database; if banned there, they are banned immediately
3. If `jev-api-key` is set, their first message is sent to Jev (https://docs.typesafe.ai) as a yes/no “is this spam?” question with the prompt from `jev_prompt.txt`; a spam probability of `jev-threshold` or higher bans them immediately. Without the key, or on any API error, the step is skipped
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
- `checkJev` — asks Jev whether a first message is spam (only text/caption, sender name/username and the replied-to message are sent)
- `startVote` — posts the vote message for a first-time poster
- `resolveVote` — atomically removes a pending vote and deletes the vote message; every vote outcome goes through it
- `checkPendingVotes` — the per-minute re-check (lols.bot + deleted-messages probe)
- `banUser` — deletes user's messages and bans them
- `whitelistUser` — adds user to `known_users` file
- `post` — Telegram API call wrapper (decodes the result, returns false on error)

### Dev Mode

With `-dev` (or `TGADMIN_DEV=true`), the actual ban call and `known_users` writes are skipped (logged only; message deletion still happens), and messages from `nikitonsky` are treated as coming from an unknown user for testing.
