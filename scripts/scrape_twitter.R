library(twitteR)
library(tidyverse)

# hidden twitter keys
consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# find 10,000 most recent tweets mentioning apple
tw <- searchTwitter("apple",
    n = 10000,
    since = "2010-01-01",
    resultType = "recent"
)
#convert to df
apple <- twListToDF(tw) %>%
    select(id, text)

# find 10,000 most recent tweets mentioning linux
tw <- searchTwitter("linux",
    n = 10000,
    since = "2010-01-01",
    resultType = "recent"
)
# convert to df
linux <- twListToDF(tw) %>%
    select(id, text)

write.csv(apple, "../data/apple.csv")
write.csv(linux, "../data/linux.csv")
