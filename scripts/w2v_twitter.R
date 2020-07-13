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
    nfolds = 5,
    seed = 1
)

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
f_score1 <- 2 * precision * recall / (precision + recall)
paste("F Score:", round(f_score1, 2))

# automl
y <- "sentiment"
x <- setdiff(names(data), y)

aml <- h2o.automl(
    x = x, y = y,
    training_frame = data,
    max_models = 20,
    seed = 1,
    nfolds = 5
)

words <- tokenize(test$text)
test_vecs <- h2o.transform(w2v_model, words, aggregate_method = "AVERAGE")

lb <- h2o.get_leaderboard(object = aml, extra_columns = "ALL")

preds <- .predict(test$text, w2v_model, aml@leader) %>%
    as.data.frame() %>%
    select(predict)
preds <- ifelse(preds$predict == "positive", 1, 0)

precision <- sum(preds & test_sent) / sum(preds)
recall <- sum(preds & test_sent) / sum(test_sent)
f_score2 <- 2 * precision * recall / (precision + recall)
paste("F Score:", round(f_score2, 4))

aml_path <- h2o.saveModel(
    object = aml@leader,
    path = "../models/", force = TRUE
)

f_scores <- data.frame(
    model_type = c("GBM", "AML"),
    f_score = c(f_score1, f_score2)
)
write.csv(f_scores, "../data/fscores.csv")
