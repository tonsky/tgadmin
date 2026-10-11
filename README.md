# Telegram moderation bot

Algorithm:

User posting for the first time in a group is checked:

1. With https://lols.bot database
2. With [Jev](https://docs.typesafe.ai)
3. With a vote: “🤖 Бот / 🧑 Не бот”:
   - Three votes on either side decide the outcome
   - A single admin vote decides immediately

While the vote is open:

- Same message posted three times gets the user banned.
- Once a minute, lols.bot is checked again.
- If the user deleted all their messages, the vote is removed. No decision is made.
- Votes that are not resolved in 24 hours are dropped, user stays unknown.
