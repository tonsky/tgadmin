# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Telegram moderation bot written in Clojure that automatically detects and bans spam accounts in Telegram groups. The bot uses a multi-step algorithm to identify suspicious first messages and gives legitimate users a chance to prove they are not bots.

## Key Commands

- **Run the bot**: `./script/run.sh` or `clojure -M -m tgadmin.core`
- **Start REPL**: `./script/repl.sh` or `clojure -M:dev -m user`
- **Run tests**: `./script/test.sh` or `clojure -A:dev -X user/-test`
- **Build JAR**: `./script/package.sh` (creates `target/tgadmin.jar`)
- **Reload code in REPL**: `(reload)` (via clj-reload)

## Architecture

### Core Components

1. **Main namespace**: `tgadmin.core` - Contains all bot logic including:
   - Message filtering logic with multiple checks (external links, media, stop words, etc.)
   - User management (whitelisting, banning)
   - Telegram API integration via http-kit
   - Challenge/response system for suspicious users

2. **Configuration**: 
   - `config.edn` - Contains bot token and other settings
   - `known_users` - Persistent storage of whitelisted users

3. **State Management**:
   - `*known-users` - Atom containing whitelisted user IDs
   - `*pending-warnings` - Atom tracking users who need to respond to challenges

### Bot Algorithm

The bot implements a sophisticated spam detection algorithm:

1. Checks https://lols.bot database for known spammers
2. Allows messages from previously seen users
3. For first-time users, checks for suspicious content:
   - External links or media
   - Mixed language characters (Latin + Cyrillic)
   - Special Unicode symbols
   - Stop words (работа, доход, курсы, etc.)
4. Suspicious users receive a challenge to reply with "не бот" within 1 minute
5. Users who pass are whitelisted; others are banned

### Key Functions

- `handle-message` - Main entry point for processing Telegram updates
- `check-message` - Runs all spam detection checks
- `warn` - Issues challenge to suspicious users
- `ack` - Processes challenge responses
- `ban-user` - Bans user and deletes their messages
- `whitelist-user` - Adds user to known_users file

### External Dependencies

- `http-kit` - HTTP client for Telegram API
- `cheshire` - JSON parsing
- Development tools: `clj-reload`, `nrepl`, `duti`