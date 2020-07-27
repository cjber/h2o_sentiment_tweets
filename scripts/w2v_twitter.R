library(tidyverse)
library(ggrepel)
library(h2o)
library(tidytext)

source("./functions.R")

# connect locally to the h2o java instance
h2o.init()

# read in training data, remove neutral and convert sentiment to factor
# easier to work with two outcome categories
train <- read.csv("../data/train.csv",
    header = TRUE
) %>%
    filter(sentiment != "neutral") %>%
    mutate(sentiment = as.factor(sentiment)) %>%
    as.h2o()

# tokenize using external function
words <- tokenize(train$text)
# create word embeddings using w2v and training data
w2v_model <- h2o.word2vec(words, sent_sample_rate = 0, epochs = 10)
w2v_path <- h2o.saveModel(object = w2v_model, path = "../models", force = TRUE)
# create single vector encoding
tweet_vecs <- h2o.transform(w2v_model,
    words,
    aggregate_method = "AVERAGE"
)

# split data into train and validation
data <- h2o.cbind(
    train["sentiment"],
    tweet_vecs
) %>% na.omit()
data_split <- h2o.splitFrame(data, ratios = 0.8, seed = 1234)

# Gradient Boosting Machine base model
gbm_model <- h2o.gbm(
    x = names(tweet_vecs),
    y = "sentiment",
    training_frame = data_split[[1]],
    validation_frame = data_split[[2]],
    seed = 1234
)

# GBM cross val
gbm_ifl <- h2o.gbm(
    x = names(tweet_vecs),
    y = "sentiment",
    training_frame = data_split[[1]],
    validation_frame = data_split[[2]],
    ntrees = 10000, # large starting number of trees
    learn_rate = 0.01, # lower learning rate
    # enable stopping so not all trees will be used
    stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "AUC",
    # randomly sample subsets of cols and rows
    sample_rate = 0.8,
    col_sample_rate = 0.8,
    seed = 1234,
    score_tree_interval = 10,
    nfolds = 5 # use cross validation
)
gbm_path <- h2o.saveModel(object = gbm_ifl, path = "../models", force = TRUE)
gbm_path <- h2o.saveModel(object = gbm_model, path = "../models", force = TRUE)

# evaluate using the data data
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

# obtain F1 score
precision <- sum(preds & test_sent) / sum(preds)
recall <- sum(preds & test_sent) / sum(test_sent)
f_score <- 2 * ((precision * recall) / (precision + recall))
