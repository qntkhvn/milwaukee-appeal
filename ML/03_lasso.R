

# specification
# start with a LASSO
lr_spec <- logistic_reg(penalty = tune(), 
                        mixture = 1) %>%
  set_engine("glmnet")

# recipe
lr_rec <- 
  recipe(appealed20 ~ ., data = train) %>% 
  step_rm(prop_id, sale_price) %>% 
  step_other(bld_type, zip, nbhd, threshold = 0.01) %>% 
  step_log(finished_area, land_sf, offset = 1) %>% 
# step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())
# step_impute_median(all_numeric_predictors()) %>% 

# workflow
lr_wf <- workflow() %>% 
  add_model(lr_spec) %>% 
  add_recipe(lr_rec)

# grid
grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)

# tuning
lr_tune <- lr_wf %>% 
  tune_grid(folds,
            metrics = metric_set(roc_auc),
            control = grid_control,
            grid = grid_regular(penalty(), levels = 50))

lr_tune %>% 
  autoplot()

lr_best <- lr_tune %>% 
  select_best("roc_auc")

lr_best

lr_final <- finalize_workflow(
  lr_wf,
  lr_best
)

lr_last <- last_fit(lr_final, split)

lr_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed20, .pred_appealed) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed()

lr_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed20, .pred_appealed)
# AUC = 0.737 on validation

# fit on holdout

lr_fit_final <- lr_final %>% 
  fit(train_raw)

lr_preds <- lr_fit_final %>% 
  augment(holdout)

lr_preds %>% 
  roc_auc(appealed20, .pred_appealed)

# AUC = 0.732 on holdout
