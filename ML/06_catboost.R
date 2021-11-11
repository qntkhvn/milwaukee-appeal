devtools::install_url('https://github.com/catboost/catboost/releases/download/v1.0.0/catboost-R-Linux-1.0.0.tgz')
library(catboost)

y_train <- ifelse(train$appealed20 == "no", 0, 1)
x_train <- train %>% select(-appealed20, -prop_id, -sale_price)
x_test <- test %>% select(-appealed20, -prop_id, -sale_price)

train_pool <- catboost.load_pool(data = x_train, label = y_train)
test_pool <- catboost.load_pool(data = x_test)

fit <- catboost.train(learn_pool = train_pool,
                      params = list(learning_rate = 0.05,
                                    loss_function = "Logloss",
                                    eval_metric = "AUC"))
                                    
test %>% 
  mutate(pred_appealed = catboost.predict(fit, test_pool, prediction_type = "Probability")) %>% 
  roc_auc(appealed20, pred_appealed)
  
test %>% 
  mutate(pred_appealed = catboost.predict(fit, test_pool, prediction_type = "Probability")) %>% 
  roc_curve(appealed20, pred_appealed) %>%
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path()
  
var_shap <- catboost.get_feature_importance(fit, pool = test_pool, type = "ShapValues")
 
shap <- var_shap[,1:19] %>% as_tibble()

names(shap) <- names(x_test)

shap %>%
  pivot_longer(everything()) %>%
  mutate(value = abs(value)) %>%
  group_by(name) %>%
  summarize(mean_abs_shap = mean(value)) %>%
  arrange(-mean_abs_shap) %>%
  mutate(name = fct_reorder(name, mean_abs_shap)) %>%
  ggplot(aes(x = name, y = mean_abs_shap)) +
  geom_col() +
  coord_flip() +
  theme_light()
  
