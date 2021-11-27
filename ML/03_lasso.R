# start with a LASSO

# specification
lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

# recipe
lasso_rec <- 
  recipe(appealed20 ~ ., data = train) %>% 
  step_other(bld_type, zip, nbhd, threshold = 0.01) %>% 
# step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())
# step_impute_median(all_numeric_predictors()) %>% 

# workflow
lasso_wf <- workflow() %>% 
  add_model(lasso_spec) %>% 
  add_recipe(lasso_rec)

# grid
grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)

# tuning
lasso_tune <- lasso_wf %>% 
  tune_grid(folds,
            metrics = metric_set(roc_auc),
            control = grid_control,
            grid = grid_regular(penalty(), levels = 50))

lasso_tune %>% 
  autoplot()

lasso_best <- lasso_tune %>% 
  select_best("roc_auc")

lasso_best

lasso_final <- lasso_wf %>% 
  finalize_workflow(lasso_best)

lasso_last <- lasso_final %>% 
  last_fit(split)

lasso_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed20, .pred_no) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed()

lasso_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed20, .pred_no)

# AUC = 0.733 on test set
