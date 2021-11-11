
xg_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),
  sample_size = tune(), 
  mtry = tune(),
  learn_rate = 0.02 
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# recipe
xg_rec <- 
  recipe(appealed20 ~ ., data = train) %>% 
  step_rm(prop_id, sale_price) %>% 
  step_other(bld_type, zip, nbhd, threshold = 0.01) %>% 
  step_log(finished_area, land_sf, offset = 1) %>% 
  # step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())
# step_impute_median(all_numeric_predictors()) %>% 

# workflow
xg_wf <- workflow() %>% 
  add_model(xg_spec) %>% 
  add_recipe(xg_rec)

doParallel::registerDoParallel()
set.seed(103)

xg_grid <- grid_latin_hypercube(
  #trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  #learn_rate(),
  size = 30
)

xg_tune <- tune_grid(
  xg_wf,
  resamples = fold,
  grid = xg_grid,
  control = control_grid(save_pred = TRUE)
)

xg_tune %>% 
  autoplot()

xg_best <- xg_tune %>% 
  select_best("roc_auc")

xg_best

xg_final <- finalize_workflow(
  xg_wf,
  xg_best
)

xg_final

xg_final %>% 
  fit(data = train) %>% 
  pull_workflow_fit() %>% 
  vip::vip(num_features = 20)


xg_last <- last_fit(xg_final, split)

xg_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed20, .pred_appealed) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed()

xg_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed20, .pred_appealed)


xg_fit_final <- xg_final %>% 
  fit(train_raw)


xg_preds <- xg_fit_final %>% 
  augment(holdout)

xg_preds %>% 
  roc_auc(appealed20, .pred_appealed)


xg_mod <- pull_workflow_fit(xg_fit_final)

library(vip)
vip(xg_mod$fit)


library(fastshap)

X <- prep(xg_rec, train) %>% 
  juice() %>% 
  select(-appealed20) %>% 
  as.matrix()

shap <- explain(xg_mod$fit, X = X, exact = TRUE)

autoplot(shap)


shap %>% 
  abs() %>% 
  map_df(mean) %>% 
  pivot_longer(everything()) %>% 
  slice_max(n = 20, order_by = value) %>% 
  mutate(name = fct_reorder(name, value)) %>% 
  ggplot(aes(value, name)) +
  geom_col()


library(SHAPforxgboost)
shap2 <- shap.prep(xg_mod$fit, X_train = X)
shap.importance(shap2, names_only = TRUE)
shap.plot.summary.wrap1(xg_mod$fit, X, top_n = 20)

shap.importance(shap2) %>% 
  as_tibble() %>% 
  slice_max(n = 20, order_by = mean_abs_shap) %>% 
  mutate(variable = fct_reorder(variable, mean_abs_shap)) %>% 
  ggplot(aes(mean_abs_shap, variable)) +
  geom_col() +
  ylab(NULL) +
  xlab("Mean(|SHAP value|) (average impact on model output magnitude)")
