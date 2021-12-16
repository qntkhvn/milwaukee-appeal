xg_smote_last %>% 
  collect_predictions() %>% 
  select(.pred_appealed, appealed20) %>% 
  mutate(model = "SMOTE") %>% 
  bind_rows(
    xg_adasyn_last %>% 
      collect_predictions() %>% 
      select(.pred_appealed, appealed20) %>% 
      mutate(model = "ADASYN")
  ) %>%
  bind_rows(
    xg_bsmote_last %>%
      collect_predictions() %>%
      select(.pred_appealed, appealed20) %>%
      mutate(model = "BSMOTE")
  ) %>%
  group_by(model) %>% 
  roc_curve(appealed20, .pred_appealed) %>% 
  autoplot() +
  theme_bw()
