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



housing %>% 
  mutate(prob_appealed = xg_pred$.pred_appealed) %>% 
  ggplot(aes(long, lat, color = prob_appealed)) +
  geom_point() +
  scale_color_gradient2()

write_csv(mutate(housing, prob_appealed = lasso_pred$.pred_appealed), "~/Desktop/consulting/map/housing2.csv")
