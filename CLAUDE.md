# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Telegram moderation bot written in Clojure that detects and bans spam accounts in Telegram groups. New users are checked against an external spam database, and their first message triggers a community vote ("bot or not?") that decides whether to whitelist or ban them.

## Key Commands

- **Run the bot**: `./script/run.sh` or `clojure -M -m tgadmin.core`
- **Start REPL**: `./script/repl.sh` (starts a socket server via `clojure+.core.server/start-server`)
- **Build JAR**: `./script/package.sh` (creates `target/tgadmin.jar`)
- **Reload code in REPL**: `(reload)` (via clj-reload, defined in `dev/user.clj`)

There is no test suite.

## Architecture

### Core Components

1. **Main namespace**: `tgadmin.core` — single namespace containing all bot logic:
   - Long-polling loop over Telegram `getUpdates` (`-main`)
   - Community vote system for first-time posters
   - External spam-database checks (lols.bot)
   - User management (whitelisting, banning)
   - "Time to first 🤡 reaction" channel monitoring (unrelated to moderation)

2. **Configuration**:
   - `config.edn` — bot token and settings: `:dev?`, `:vote-limit` (default 3), `:vote-ttl` (default 24h), `:repeated-messages-limit` (default 3), `:admin-cache-ttl` (default 1h), `:reaction-channel-id`, `:reaction-group-id`
   - `known_users` — persistent append-only storage of whitelisted user IDs

3. **State Management** (all atoms in `tgadmin.core`):
   - `*known-users` — whitelisted user IDs
   - `*pending-votes` — open votes: `{user-id {:messages [...] :vote-message ... :approve #{...} :ban #{...} :added ts}}`
   - `*admin-cache` — cached chat admin lists
   - `*reaction-channel-posts` — channel posts tracked for the reaction monitor

### Bot Algorithm

1. Known users pass through untouched
2. Unknown users are checked against the https://lols.bot spam database; if banned there, they are banned immediately
3. Otherwise the bot replies to their first message with a vote: inline keyboard "🤖 Бот / 🧑 Не бот"
   - `vote-limit` votes on either side decide the outcome; a single admin vote decides immediately
   - Approve → whitelist; ban → delete all their messages and ban
4. While a vote is pending, further messages from the user are collected; posting the same message `repeated-messages-limit` times gets them banned outright
5. A timer re-checks every user with an open vote once a minute:
   - re-queries lols.bot — if now reported as spammer, closes the vote and bans
   - probes whether their messages still exist — if the user deleted all of them, the vote is retracted and the user stays unknown
6. Pending votes older than `vote-ttl` are cleaned up hourly (vote message stays, user stays unknown)

Note: the Bot API sends no update when messages are deleted, so deletion is detected by probing with `forwardMessage` into a private dump group (`:dump-group-id` in config; the forward is deleted right away). A "message to forward not found" error means the message is gone; any other error fails open (`message-exists?`).

### Key Functions

- `handle-message` — main entry point for incoming messages
- `handle-callback-query` — processes vote button presses
- `check-external` — queries lols.bot by user ID
- `start-vote` — posts the vote message for a first-time poster
- `resolve-vote!` — atomically removes a pending vote and deletes the vote message; every vote outcome goes through it
- `check-pending-votes` — the per-minute re-check (lols.bot + deleted-messages probe)
- `ban-user` — deletes user's messages and bans them
- `whitelist-user` — adds user to `known_users` file
- `post!` — Telegram API call wrapper (returns `:result` or nil on error)

### Dev Mode

With `:dev? true` in `config.edn`, the actual ban call and `known_users` writes are skipped (logged only; message deletion still happens), and messages from `nikitonsky` are treated as coming from an unknown user for testing.

### External Dependencies

- `http-kit` — HTTP client for Telegram API
- `cheshire` — JSON parsing
- `clojure+` — `cond+`/`when+` control-flow macros, REPL server
- Development tools: `clj-reload`
