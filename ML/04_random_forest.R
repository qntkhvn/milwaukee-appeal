rf_spec <-
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# recipe
rf_rec <- 
  recipe(appealed20 ~ ., data = train) %>% 
  step_rm(prop_id, sale_price) %>% 
  step_other(bld_type, zip, nbhd, threshold = 0.01) %>% 
  step_log(finished_area, land_sf, offset = 1) %>% 
  # step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())
# step_impute_median(all_numeric_predictors()) %>% 

# workflow
rf_wf <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(rf_rec)

# grid
grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)

# tuning
rf_tune <- rf_wf %>% 
  tune_grid(folds,
            metrics = metric_set(roc_auc),
            control = grid_control,
            grid = crossing(mtry = 3:10, min_n = 3:10))

rf_tune %>% 
  autoplot()

rf_best <- rf_tune %>% 
  select_best("roc_auc")

rf_best

rf_final <- finalize_workflow(
  rf_wf,
  rf_best
)

rf_last <- last_fit(rf_final, split)

rf_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed20, .pred_appealed) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed()

rf_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed20, .pred_appealed)
# AUC on validation

# fit on holdout

rf_fit_final <- rf_final %>% 
  fit(train_raw)

rf_preds <- rf_fit_final %>% 
  augment(holdout)

rf_preds %>% 
  roc_auc(appealed20, .pred_appealed)

# AUC on holdout
