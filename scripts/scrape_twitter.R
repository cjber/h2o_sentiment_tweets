library(twitteR)
library(tidyverse)

consumer_key <- "7Cv6QTdfKvzlZKKGQ8a5VyD7u"
consumer_secret <- "nMNsOiAwJpVg4zBnwan6k1xTm4ql6DLF06v6HhyYtFrxWidndE"
access_token <- "390378238-W1likclkOKb1hR0Gdq5Fwwk9u9HW8VgJHPBknIBx"
access_secret <- "p7RKAduP3TJ4YNLeRv4H4hAz2PgDFr7GZEk7ilEcKLW4B"

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
