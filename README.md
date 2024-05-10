# Telegram moderation bot

Algorithm:

1. Check https://lols.bot database. If spam, ban
2. Is it the first message by this user in this group? If not, pass
3. Does the first message contain link, image, or stop words (работа/доход/etc). If not, pass
4. So now it’s the first message and it looks sus, we ask user to write a reply with the word “не бот”
5. User replied within one minute? Great, add them to whitelist
6. Otherwise, ban