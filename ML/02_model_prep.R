library(tidymodels)

# 70-15-15 split
# train-validation-test

# holdout first
set.seed(100)
holdout_split <- initial_split(raw,
                               prop = 0.85,
                               strata = appealed20)
holdout <- testing(holdout_split)
train_raw <- training(holdout_split)

# train and validation
set.seed(101)
split <- initial_split(train_raw,
                       prop = 1 - nrow(holdout) / nrow(train_raw),
                       strata = appealed20)
train <- training(split)
test <- testing(split)

# k-fold cross-validation
# change v for the number of folds
set.seed(102)
fold <- vfold_cv(train, 
                 v = 5,
                 strata = appealed20)
