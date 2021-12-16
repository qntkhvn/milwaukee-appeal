
rf_spec <-
  rand_forest(mtry = tune(), 
              min_n = tune(), 
              trees = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_rec <- recipe(appealed20 ~ ., data = train) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

# workflow
rf_wf <- workflow() %>% 
  add_model(rf_spec) %>% 
  add_recipe(rf_rec)

# grid
rf_grid <- grid_latin_hypercube(
  min_n(range = c(3, 15)),
  finalize(mtry(range = c(3, 15)), train),
  trees(range = c(300, 1000)),
  size = 20
)

grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)

# min_n = 30

# tuning
doParallel::registerDoParallel(cores = 5)
rf_tune <- rf_wf %>% 
  tune_grid(folds,
            metrics = metric_set(roc_auc),
            control = grid_control,
            grid = rf_grid)

write_rds(rf_tune, "~/Desktop/mke/rf_tune.rds")

rf_tune %>% 
  autoplot()



rf_best <- rf_tune %>% 
  select_best("roc_auc")

rf_best

rf_final <- rf_wf %>% 
  finalize_workflow(rf_best)

rf_final


rf_last <- rf_final %>% 
  last_fit(split)

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


rf_fit_final <- rf_final %>% 
  fit(raw)


rf_pred <- rf_fit_final %>% 
  augment(raw)

