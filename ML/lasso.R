# start with a LASSO

# set up

# specification
lasso_spec <- logistic_reg(penalty = tune()) %>%
  set_engine("glmnet")

# recipe
lasso_rec <- 
  recipe(appealed20 ~ ., data = train) %>% 
#step_other(bld_type, zip, nbhd, threshold = 0.01) %>% 
# step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_normalize(all_predictors()) %>% 
  themis::step_smote(appealed20, seed = 69)
# step_impute_median(all_numeric_predictors()) %>% 

# workflow
lasso_wf <- workflow() %>% 
  add_model(lasso_spec) %>% 
  add_recipe(lasso_rec)

##########################################################################

# tune


# grid
grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)

# tuning
set.seed(102)
lasso_tune <-
  tune_grid(object = lasso_wf,
            resamples = folds,
            metrics = metric_set(roc_auc),
            control = grid_control,
            grid = grid_regular(penalty(), levels = 50))

lasso_tune %>% 
  autoplot()

# obtain the best tuning parameter(s)
lasso_best <- lasso_tune %>% 
  select_best("roc_auc")

lasso_best

# penalty = 0.000339


# finalize the model based on the selected best/optimal hyperparam values
lasso_final <- lasso_wf %>% 
  finalize_workflow(lasso_best)

# fit a final model on the entire train set
lasso_last <- lasso_final %>% 
  last_fit(split)

# evaluate (on the test set)

lasso_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed20, .pred_appealed) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed()

lasso_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed20, .pred_appealed)

lasso_last %>% 
  collect_predictions() %>% 
  conf_mat(appealed20, .pred_class)

# AUC = 0.752 on test set

lasso_fit_final <- lasso_final %>% 
  fit(raw)


lasso_pred <- lasso_fit_final %>% 
  augment(raw)
