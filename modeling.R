library(tidyverse)
library(janitor)
library(scales)
library(lubridate)
theme_set(theme_light()) 

library(here)
path <- "Desktop/consulting/project"
load(here(path, "consulting_data.RData"))
load(here(path, "location_data.RData"))
# load(here(path, "nbhd_map.RData"))

geo_cleaned <- read_csv("https://www.dropbox.com/s/9jcsk1477qc4k60/geo_cleaned.csv?dl=1") %>% 
  mutate(prop_id = as.character(prop_id),
         zip = as.character(zip))


# cleaning by column order
housing <- final %>%
  clean_names() %>%
  mutate(
    
    prop_id = as.character(prop_id),
    
    bld_type = str_remove(bld_type, ".*- "),
    
    appraiser = str_sub(appraiser, start = 7),
    
    nbhd = str_sub(nbhd, 1, 4),
    
    qual = ifelse(str_detect(qual, "M"), NA, str_remove(qual, " -.*")),
    
    cond = str_remove(cond, ".*- "),
    
    kitchen_rating = str_remove(kitchen_rating, ".*- "),
    kitchen_rating = ifelse(kitchen_ct == 0, NA, kitchen_rating),
    
    
    full_bath_ct = ifelse(is.na(full_bath_ct), 0, full_bath_ct),
    full_bath_rating = str_remove(full_bath_rating, ".*- "),
    full_bath_rating = ifelse(full_bath_ct == 0, NA, full_bath_rating),
    
    half_bath_ct = ifelse(is.na(half_bath_ct), 0, half_bath_ct),
    half_bath_rating = str_remove(half_bath_rating, ".*- "),
    half_bath_rating = ifelse(half_bath_ct == 0, NA, half_bath_rating),
    
    year_built = ifelse(year_built == 0, NA, year_built),
    
    finished_area = ifelse(finished_area == 0, NA, finished_area),
    
    land_sf = ifelse(land_sf == 0, NA, land_sf),
    
    # sale_month = ifelse(is.na(sale_date), 0, month(sale_date)), # month
    # sale_year = ifelse(is.na(sale_date), 0, year(sale_date)), # year
    # sale_wday = ifelse(is.na(sale_date), 0, wday(sale_date)), # day of the week
    # sale_week = ifelse(is.na(sale_date), 0, week(sale_date)), # week number (1-52)
    # sale_year = ifelse(sale_year == 2021 & sale_month > 8, 2020, sale_year),
    
    sale_status = ifelse(is.na(sale_price), "no", "yes"),
    
    sale_price = ifelse(is.na(sale_price), 0, sale_price),
    # sale_price_group = case_when(
    #   sale_price <= 100000 ~ "<100k",
    #   sale_price > 100000 & sale_price <= 150000 ~ "100-150k",
    #   sale_price > 150000 & sale_price <= 200000 ~"150-200k",
    #   sale_price > 200000 ~ ">200k",
    #   is.na(sale_price) ~ "no_sale"),   # several sale price of 0
    
    appealed19 = ifelse(is.na(appealed19), "no", "yes"), 
    appealed20 = ifelse(is.na(appealed20), "no", "yes"),
    appealed21 = ifelse(is.na(appealed21), "no", "yes"),
    
  ) %>% 
  left_join(geo_cleaned, by = "prop_id") # finally, joined with latitude/longitude data



# ordinal encoding
rating_levels <- c("Excellent", "Very Good", "Good", "Average", 
                   "Fair", "Poor", "Very Poor", "Unsound")
qual_levels <- c("AA", "AA-", "A+", "A", "A-", "B+", "B", "B-", 
                 "C+", "C", "C-", "D+", "D", "D-", "E+", "E", "E-")


raw <- housing %>% 
  mutate(qual = as.numeric(factor(qual, levels = rev(qual_levels))),
         qual = ifelse(is.na(qual), 0 , qual),
         cond = as.numeric(factor(cond, levels = rev(rating_levels))),
         cond = ifelse(is.na(cond), 0 , cond),
         kitchen_rating = as.numeric(factor(kitchen_rating, levels = rev(rating_levels))),
         kitchen_rating = ifelse(is.na(kitchen_rating), 0 , kitchen_rating),
         full_bath_rating = as.numeric(factor(full_bath_rating, levels = rev(rating_levels))),
         full_bath_rating = ifelse(is.na(full_bath_rating), 0 , full_bath_rating),
         half_bath_rating = as.numeric(factor(half_bath_rating, levels = rev(rating_levels))),
         half_bath_rating = ifelse(is.na(half_bath_rating), 0 , half_bath_rating),
         appealed = ifelse(appealed19 == "yes" | appealed20 == "yes" | appealed21 == "yes", "appealed", "no")) %>% 
  select(-address, -geo_tract, -sale_date, -appealed19, -appealed20, -appealed21) %>% 
  mutate(across(where(is.character), as.factor)) %>% 
  drop_na() #only 11 obs



library(tidymodels)

# 70-15-15 split
set.seed(100)
holdout_split <- initial_split(raw,
                               prop = 0.85,
                               strata = appealed)
holdout <- testing(holdout_split)
train_raw <- training(holdout_split)

set.seed(101)
split <- initial_split(train_raw,
                       prop = 1 - nrow(holdout) / nrow(train_raw),
                       strata = appealed)
train <- training(split)
test <- testing(split)

# 10-fold cross-validation
set.seed(102)
fold <- vfold_cv(train, 
                 v = 10,
                 strata = appealed)
  
xg_spec <- boost_tree(
  trees = tune(), 
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),                   ## first three: model complexity
  sample_size = tune(), 
  mtry = tune(),                             ## randomness
  learn_rate = tune()                        ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

xg_rec <- 
  recipe(appealed ~ ., data = train) %>% 
  step_rm(prop_id, nbhd, sale_price) %>% 
  step_other(bld_type, zip, threshold = 0.01) %>% 
  # step_log(finished_area, land_sf, offset = 1) %>% 
  # step_YeoJohnson(sale_price) %>% 
  # step_unknown(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors())
  # step_impute_median(all_numeric_predictors()) %>% 
  # step_nzv(all_numeric_predictors())


b <- xg_rec %>% prep() %>% juice()

xg_wf <- workflow() %>% 
  add_model(xg_spec) %>% 
  add_recipe(xg_rec)


doParallel::registerDoParallel()
set.seed(103)


xg_grid <- grid_latin_hypercube(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train),
  learn_rate(),
  size = 30
)

xg_tune <- tune_grid(
  xg_wf,
  resamples = fold,
  grid = xg_grid,
  control = control_grid(save_pred = TRUE)
)



xg_tune %>% 
  collect_metrics() %>% 
  filter(.metric == "roc_auc") %>% 
  select(mean, mtry, trees:learn_rate) %>% 
  pivot_longer(!mean) %>% 
  ggplot(aes(value, mean, color = name)) +
  geom_point(show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~ name, scales = "free_y")


best_auc <- xg_tune %>% 
  select_best("roc_auc")

best_auc

xg_final <- finalize_workflow(
  xg_wf,
  best_auc
)

xg_final

xg_final %>% 
  fit(data = train) %>% 
  pull_workflow_fit() %>% 
  vip::vip(num_features = 20)


xg_last <- last_fit(xg_final, split)

xg_last %>% 
  collect_predictions() %>% 
  roc_curve(appealed, .pred_appealed) %>% 
  ggplot(aes(1 - specificity, sensitivity)) +
  geom_abline() +
  geom_path() +
  coord_fixed() +
  labs(title = "AUC = 0.743")

xg_last %>% 
  collect_predictions() %>% 
  roc_auc(appealed, .pred_appealed)

xg_last %>% 
  collect_predictions() %>% 
  conf_mat(appealed, .pred_class)


xg_fit_final <- xg_final %>% 
  fit(train_raw)


xg_preds <- xg_fit_final %>% 
  augment(holdout)

xg_preds %>% 
  select(appealed, contains(".pred")) %>% 
  roc_auc(appealed, .pred_appealed)

xg_preds %>% 
  select(appealed, contains(".pred")) %>% 
  conf_mat(appealed, .pred_class)








xg_mod <- pull_workflow_fit(xg_fit_final)

library(vip)
vip(xg_mod$fit)


library(fastshap)

X <- prep(xg_rec, train) %>% 
  juice() %>% 
  select(-appealed) %>% 
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


# feat <- prep(xg_rec, train) %>% 
#   juice()


# autoplot(shap, 
#          type = "dependence", 
#          feature = "appealed20_yes", 
#          X = feat,
#          smooth = TRUE, 
#          color_by = "appealed21")




