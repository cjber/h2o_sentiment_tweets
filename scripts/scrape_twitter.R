library(twitteR)
library(tidyverse)

consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw <- searchTwitter("apple",
    n = 10000,
    since = "2010-01-01",
    resultType = "recent"
)
apple <- twListToDF(tw) %>%
    select(id, text)

tw <- searchTwitter("linux",
    n = 10000,
    since = "2010-01-01",
    resultType = "recent"
)

linux <- twListToDF(tw) %>%
    select(id, text)

write.csv(apple, "../data/apple.csv")
write.csv(linux, "../data/linux.csv")
