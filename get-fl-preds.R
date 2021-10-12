fl_preds <- s3readRDS("predictions_cards.rds", s3bucket) %>%
  filter(geo_value == "fl", signal == "confirmed_incidence_num",
         forecaster %in% c("COVIDhub-4_week_ensemble", "Karlen-pypm"))
saveRDS(fl_preds, "fl_predictions.rds")

fl_preds <- readRDS("fl_predictions.rds") %>%
  filter(is.na(quantile) |
           abs(quantile - .025) < 1e-6 |
           abs(quantile - .975) < 1e-6) %>%
  mutate(
    forecast_date = target_end_date - ahead * 7,
    ci = case_when(
      is.na(quantile) ~ "point",
      abs(quantile - .025) < 1e-2 ~"lo",
      TRUE ~"hi"))

fl_preds <- fl_preds %>%
  select(ahead, value, forecaster, forecast_date, target_end_date, ci) %>%
  pivot_wider(names_from = ci, values_from = value)

saveRDS(fl_preds, "fl_predictions.rds")
fl_actuals <- covidcast_signal("jhu-csse", "confirmed_incidence_num", geo_type = "state",
                               geo_values = "fl") %>%
  evalcast:::sum_to_epiweek() %>%
  mutate(target_end_date = lubridate::ymd(time_value))
saveRDS(fl_actuals, "fl_actuals.rds")

