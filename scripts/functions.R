library(h2o)
library(tidyverse)
library(tidytext)

## @knitr tokenizetext
tokenize <- function(sentences, stop_words = get_stopwords()$word) {
    tokenized <- sentences %>%
        # remove urls
        h2o.gsub(r'(https?://\\S+|www\\.\\S+)', "", .) %>%
        h2o.gsub(r'(\\S*\\.com)', "", .) %>%
        # remove usernames
        h2o.gsub(r'(@\\w+)', "", .) %>%
        # remove hashtags
        h2o.gsub(r'(#\\w+)', "", .) %>%
        # remove all numbers
        h2o.gsub(r'(\\d)', "", .) %>%
        # fix words with >2 repeated letters
        h2o.gsub(r'((\\w)\\1{2,99})', "$1", .) %>%
        # tokenize by space
        h2o.tokenize(r"(\\W+)") %>%
        h2o.tolower()

    word_lens <- h2o.nchar(tokenized)
    # remove common stop words
    # and words less than 2 characters
    tokenized <- tokenized[
        !tokenized %in% stop_words ||
            !is.na(tokenized) ||
            word_lens >= 2,
    ]
}

## @knitr prediction
.predict <- function(tweet, w2v, model) {
    # tokenize text
    words <- tokenize(as.character(as.h2o(tweet)))
    # encode to single vector using average
    tweet_vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
    # predict values
    h2o.predict(model, tweet_vec, seed = 1234)
}
