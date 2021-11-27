library(tidymodels)
tidymodels_prefer() 

# 70-30 train-test split
set.seed(100)
split <- raw %>% 
  initial_split(prop = 0.7, strata = appealed20)
train <- training(split)
test <- testing(split)

# k-fold cross-validation
# change v for the number of folds
set.seed(101)
folds <- train %>% 
  vfold_cv(v = 3, strata = appealed20)
