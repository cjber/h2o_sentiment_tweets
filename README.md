# Machine Learning for Tweet Sentiment Detection with H2O
## Submitted for ENVS803

Repository: https://github.com/cjber/h2o_sentiment_tweets

The main document is `201374125_h2o.html` and is identical to `./main/index.html`.

The `./scripts` directory contains the R scripts for querying the Twitter API, creating the W2V and GBM models, and some helper functions.

The `.Rmd` file in the `main` directory may be compiled using `rmarkdown::render('index.Rmd')` and will use the saved models. To use updated models from the w2v script, the file will have to be edited with the paths of the new binaries.
