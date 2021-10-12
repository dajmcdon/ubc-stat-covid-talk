library(evalcast)
cmuts <- get_covidhub_predictions(
  "CMU-TimeSeries", "2020-12-28", 
  signal = "deaths_incidence_num", geo_values = "ca")
pd <- evalcast:::setup_plot_trajectory(cmuts, geo_type = "state")

ca_traj <- ggplot(pd$truth_df, mapping = aes(x = target_end_date)) +
  geom_ribbon(
    data = pd$quantiles_df,
    mapping = aes(ymin = lower, ymax = upper, fill = interval)) +
  scale_fill_brewer(palette = "Blues") +
  coord_cartesian(xlim = lubridate::ymd(c("2020-11-01", "2021-02-01"))) +
  geom_line(aes(y = value)) + # reported
  geom_point(aes(y = value)) + # reported gets dots
  geom_line(data = pd$points_df, 
            mapping = aes(y = value),
            color = "orange", size = .5) +
  geom_point(data = pd$points_df,
             mapping = aes(y = value),
             color = "orange", size = 1) +
  geom_vline(xintercept = as.Date("2020-12-28"), linetype = "dashed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme_bw(base_size = 14) + 
  theme(legend.position = "none") + 
  ylab("Incident deaths/epiweek in CA") + 
  xlab("Date")
