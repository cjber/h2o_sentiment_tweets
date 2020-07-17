library(tidyverse)
library(ggrepel)
library(h2o)
library(tidytext)

h2o.init()

train <- read.csv("../data/train.csv",
    header = TRUE
) %>%
    filter(sentiment != "neutral") %>%
    mutate(sentiment = as.factor(sentiment)) %>%
    as.h2o()

tokenize <- function(sentences, stop_words = get_stopwords()$word) {
    tokenized <- sentences %>%
        h2o.gsub(r'(https?://\\S+|www\\.\\S+)', "", .) %>%
        h2o.gsub(r'(\\S*\\.com)', "", .) %>%
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

.predict <- function(tweet, w2v, model) {
    # tokenize text
    words <- tokenize(as.character(as.h2o(tweet)))
    # encode to single vector using average
    tweet_vec <- h2o.transform(w2v, words, aggregate_method = "AVERAGE")
    h2o.predict(model, tweet_vec)
}

words <- tokenize(train$text)
# create word embeddings using w2v and training data
w2v_model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)
w2v_path <- h2o.saveModel(object = w2v_model, path = "../models", force = TRUE)
# create single vector encoding
tweet_vecs <- h2o.transform(w2v_model, words, aggregate_method = "AVERAGE")

data <- h2o.cbind(
    train["sentiment"],
    tweet_vecs
) %>% na.omit()
data_split <- h2o.splitFrame(data, ratios = 0.8)

# Gradient Boosting Machine
gbm_model <- h2o.gbm(
    x = names(tweet_vecs),
    y = "sentiment",
    training_frame = data_split[[1]],
    validation_frame = data_split[[2]],
    seed = 1234
)
gbm_base_auc <- h2o.auc(h2o.performance(gbm_model, valid = TRUE))

gbm_ifl <- h2o.gbm(
    x = names(tweet_vecs),
    y = "sentiment",
    training_frame = data_split[[1]],
    validation_frame = data_split[[2]],
    ntrees = 10000,
    learn_rate = 0.01,
    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
    sample_rate = 0.8,
    col_sample_rate = 0.8,
    seed = 1234,
    score_tree_interval = 10
)
gbm_ifl_auc <- h2o.auc(h2o.performance(gbm_ifl, valid = TRUE))
gbm_path <- h2o.saveModel(object = gbm_ifl, path = "../models", force = TRUE)

test <- read.csv("../data/test.csv",
    header = TRUE
) %>%
    filter(sentiment != "neutral") %>%
    mutate(sentiment = as.factor(sentiment)) %>%
    as.h2o()

preds <- .predict(test$text, w2v_model, gbm_model) %>%
    as.data.frame() %>%
    select(predict)
preds <- ifelse(preds$predict == "positive", 1, 0)

test_sent <- test$sentiment %>%
    as.data.frame()
test_sent <- ifelse(test_sent$sentiment == "positive", 1, 0)

precision <- sum(preds & test_sent) / sum(preds)
recall <- sum(preds & test_sent) / sum(test_sent)
f_score <- 2 * precision * recall / (precision + recall)
