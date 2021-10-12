library(aws.s3)
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
s3bucket <- get_bucket("forecast-eval")

Mean <- function(x) mean(x, na.rm = TRUE)

dea <- s3readRDS("score_cards_state_deaths.rds", s3bucket) %>%
  select(ahead:forecaster, target_end_date, cov_95) %>%
  filter(forecaster %in% c("COVIDhub-ensemble", "Karlen-pypm", "CMU-TimeSeries")) %>%
  mutate(forecast_date = target_end_date - 7 * ahead) %>%
  group_by(forecaster, ahead, target_end_date) %>%
  summarise(cov = Mean(cov_95))
cas <- s3readRDS("score_cards_state_cases.rds", s3bucket) %>%
  select(ahead:forecaster, target_end_date, cov_95) %>%
  filter(forecaster %in% c("COVIDhub-ensemble", "Karlen-pypm")) %>%
  mutate(forecast_date = target_end_date - 7 * ahead) %>%
  group_by(forecaster, ahead, target_end_date) %>%
  summarise(cov = Mean(cov_95))

saveRDS(dea, "deaths-coverage.rds")
saveRDS(cas, "cases-coverage.rds")
