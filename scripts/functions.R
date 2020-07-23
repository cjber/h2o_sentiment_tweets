library(h2o)
library(tidyverse)

## @knitr tokenizetext
tokenize <- function(sentences, stop_words = get_stopwords()$word) {
    tokenized <- sentences %>%
        h2o.gsub(r'(https?://\\S+|www\\.\\S+)', "", .) %>%
        h2o.gsub(r'(\\S*\\.com)', "", .) %>%
        h2o.gsub(r'(@\\w+)', "", .) %>%
        h2o.gsub(r'(#\\w+)', "", .) %>%
        h2o.gsub(r'([0-9])', "", .) %>%
        # fix words with >2 repeated letters
        h2o.gsub(r'((\\w)\\1{2,99})', "$1", .) %>%
        h2o.tokenize(r"(\\W+)") %>%
        h2o.tolower()

    word_lens <- h2o.nchar(tokenized)
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
    h2o.predict(model, tweet_vec, seed = 1234)
}
