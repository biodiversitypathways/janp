dat <- janp_main |>
  filter(!is.na(species_code)) |>
  mutate(
    year = year(recording_date_time),
    visit_date = as.Date(recording_date_time)
  ) |>
  group_by(
    location,
    visit_date,
    year,
    species_code
  ) |>
  summarise(
    Y = n(),               # observed count
    .groups = "drop"
  )

det <- janp_main |>
  mutate(
    visit_date = as.Date(recording_date_time),
    timeday = hour(recording_date_time) +
      minute(recording_date_time) / 60,
    julian = yday(recording_date_time)
  ) |>
  distinct(location, visit_date, timeday, julian, observer)

dat <- dat |>
  left_join(det, by = c("location", "visit_date"))

library(detect)

fits <- dat |>
  group_split(species_code) |>
  setNames(unique(dat$species_code)) |>
  lapply(function(d) {

    if (nrow(d) < 50) return(NULL)

    tryCatch(
      svabu(
        Y ~ scale(year) | scale(timeday) + scale(julian),
        data = d,
        distr = "NB"
      ),
      error = function(e) NULL
    )
  }) |>
  (\(x) x[!vapply(x, is.null, logical(1))])()


```{r}
#| warning: false
#| echo: true
#| eval: true
#| message: false
#| include: true
#| results: hide
#| code-fold: true
#| fig-align: center
#| fig-width: 10
#| fig-height: 12
#| fig-cap: Estimated single-visit mean abundance trends per species and ecoregion
#| label: fig-abundance-trends
#| cap-location: bottom

# Plot abundance trends
ggplot(lambda_all, aes(x = year, y = lambda_hat, color = ecoregion)) +
  geom_line(size = 1.1) +
  facet_wrap(~species_code, scales = "free_y", ncol = 6) +
  ylim(0, 3) +
  labs(
    x = "Year",
    y = expression("Estimated Abundance (" * hat(lambda) * ")"),
    title = "Detection-Corrected Abundance Trends by Ecoregion"
  ) +
  theme_bw() +
  scale_colour_viridis_d(option = "cividis") +
  theme(strip.text = element_text(size = 8))

```

```{r}
#| warning: false
#| echo: true
#| eval: true
#| message: false
#| include: true
#| results: hide
#| code-fold: true

lambda_all %>%
  group_by(species_code) %>%
  summarise(
    mk_test = list(mmkh(lambda_hat))) |>
  ungroup() |>
  unnest_wider(mk_test)

```

```{r}
#| warning: false
#| echo: true
#| eval: true
#| message: false
#| include: true
#| results: hide
#| code-fold: true
#| fig-align: center
#| fig-width: 10
#| fig-cap: Abundance trends with thresholds
#| label: fig-abundance-trends-thresholds
#| cap-location: bottom

mean_lambda <- lambda_all %>%
  group_by(species_code, ecoregion) %>%
  summarise(mean_lambda = mean(lambda_hat), .groups = "drop")

trend_alls <- trend_all %>%
  left_join(mean_lambda, by = c("species_code", "ecoregion")) %>%
  mutate(
    perc_change = sen_slope / mean_lambda * 100,
    trend_flag = case_when(
      perc_change >= 2.5 ~ "Increase > 2.5%",
      perc_change <= -2.5 ~ "Decrease > 2.5%",
      TRUE ~ "Stable"
    )
  )

lambda_all_flagged <- lambda_all %>%
  left_join(trend_alls %>% dplyr::select(species_code, ecoregion, trend_flag),
            by = c("species_code", "ecoregion")) |>
  arrange(species_code, year)

# Plot
ggplot(lambda_all_flagged |> filter(ecoregion == "Alpine"), aes(x = year, y = lambda_hat, colour = trend_flag)) +
  geom_point(data = tot_a_filtered %>% filter(species_code %in% mdm),
             aes(x = year, y = individual_order), alpha = 0.3, color = "gray") +
  geom_smooth(method = "lm", se = TRUE, size = 1.1) +  # straight line with confidence interval
  facet_wrap(~species_code, scales = "free_y") +
  scale_color_manual(values = c("Increase > 2.5%" = "blue",
                                "Decrease > 2.5%" = "red",
                                "Stable" = "black")) +
  labs(x = "Year", y = expression("Estimated Abundance ("*lambda*")"),
       color = "Trend Category",
       title = "Detection-Corrected Abundance Trends with ±2.5% Threshold") +
  theme_bw() +
  theme(legend.position = "bottom")

```
