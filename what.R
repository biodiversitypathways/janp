

```{r}
#| warning: false
#| echo: true
#| eval: true
#| message: false
#| include: true
#| results: hide
#| code-fold: true

all_lambda <- list()
trend_summary <- list()

tot_abundance <- tot_a_filtered |>
  filter(!detection_time > 180) |>
  mutate(julian = yday(recording_date_time),
         hour = hour(recording_date_time)) |>
  filter(species_code %in% mdm)

for (sp in unique(tot_abundance$species_code)) {
  for (ec in unique(tot_abundance$ecoregion)) {
    simp <- tot_abundance %>%
      filter(species_code == sp, ecoregion == ec)

    if (nrow(simp) < 5) next

    fit <- svabu(individual_order ~ year | year + julian + hour, data = simp)

    mean_julian <- mean(simp$julian, na.rm = TRUE)

    pred_grid <- expand.grid(year = unique(simp$year), julian = mean_julian)
    pred_grid$lambda <- predict(fit, newdata = pred_grid, type = "response")

    lambda_year <- pred_grid %>%
      group_by(year) %>%
      summarise(lambda_hat = mean(lambda)) %>%
      mutate(species_code = sp, ecoregion = ec)

    all_lambda[[paste(sp, ec, sep = "_")]] <- lambda_year

    # Mann–Kendall test on λ̂
    mk <- try(mmkh(lambda_year$lambda_hat), silent = TRUE)

    if (!inherits(mk, "try-error")) {
      trend_summary[[paste(sp, ec, sep = "_")]] <- tibble(
        species_code = sp,
        ecoregion = ec,
        tau = mk[["Tau"]],
        p_value = mk[["new P-value"]],
        sen_slope = mk[["Sen's slope"]]
      )
    }
  }
}

lambda_all <- bind_rows(all_lambda)
trend_all <- bind_rows(trend_summary)

trend_alls <- trend_all %>%
  mutate(
    perc_change = sen_slope / mean(lambda_all$lambda_hat[lambda_all$species_code == species_code & lambda_all$ecoregion == ecoregion]) * 100,
    trend_flag = case_when(
      perc_change >= 2.5 ~ "Increase > 2.5%",
      perc_change <= -2.5 ~ "Decrease > 2.5%",
      TRUE ~ "Stable"
    )
  )
```
