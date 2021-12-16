

xg_smote_spec <- boost_tree(trees = tune(),
                            tree_depth = tune(), 
                            learn_rate = tune(),
                            min_n = tune(),
                            sample_size = 0.5,
                            mtry = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# recipe
library(themis)

xg_smote_rec <- recipe(appealed20 ~ ., data = train) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_smote(appealed20, over_ratio = 0.2)

# workflow
xg_smote_wf <- workflow() %>% 
  add_model(xg_smote_spec) %>% 
  add_recipe(xg_smote_rec)


xg_smote_grid <- grid_latin_hypercube(
  trees(range = c(500, 1000)),
  tree_depth(range = c(3, 8)),
  min_n(range = c(5, 12)),
  finalize(mtry(range = c(5, 12)), train),
  #loss_reduction(),
  # sample_size = sample_prop(),
  learn_rate(range = c(0.005, 0.02), trans = NULL),
  size = 30
)


# tree_depth = c(6, 8, 10, 12, 14),
# learn_rate = c(0.005, 0.01)

# xg_smote_grid <- crossing(trees = c(500, 800, 1000),
#                     tree_depth = 10,
#                     learn_rate = 0.01)

grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE,
                             extract = extract_model)

doParallel::registerDoParallel(cores = 5)
set.seed(103)
xg_smote_tune <- xg_smote_wf %>% 
  tune_grid(resamples = folds,
            metrics = metric_set(mn_log_loss, roc_auc),
            control = grid_control,
            grid = xg_smote_grid)


xg_smote_tune %>% 
  autoplot()

xg_smote_best <- xg_smote_tune %>% 
  select_best("mn_log_loss")

xg_smote_best

xg_smote_final <- xg_smote_wf %>% 
  finalize_workflow(xg_smote_best)

xg_smote_final


xg_smote_last <- xg_smote_final %>% 
  last_fit(split)

xg_smote_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed20, .pred_appealed) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed()

xg_smote_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed20, .pred_appealed)

xg_smote_fit_final <- xg_smote_final %>% 
  fit(raw)

xg_smote_pred <- xg_smote_fit_final %>% 
  augment(raw)

## Feature importance

xg_smote_mod <- extract_fit_parsnip(xg_smote_fit_final)

library(vip)
vip(xg_smote_mod$fit)


library(fastshap)

X <- prep(xg_smote_rec, train) %>% 
  juice() %>% 
  select(-appealed20) %>% 
  as.matrix()

shap <- explain(xg_smote_mod$fit, X = X, exact = TRUE)

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
shap2 <- shap.prep(xg_smote_mod$fit, X_train = X)


shap.importance(shap2, names_only = TRUE)
shap.plot.summary.wrap1(xg_smote_mod$fit, X, top_n = 15)

shap.importance(shap2) %>% 
  as_tibble() %>% 
  slice_max(n = 15, order_by = mean_abs_shap) %>% 
  mutate(variable = fct_reorder(variable, mean_abs_shap)) %>% 
  ggplot(aes(mean_abs_shap, variable)) +
  geom_col() +
  ylab(NULL) +
  xlab("Mean(|SHAP|)")
