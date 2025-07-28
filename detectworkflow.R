library(detect)

simp <- tot_a_filtered |>
  filter(species_code == "YRWA")

simp$detcov <- scale(simp$year)

fit <- svabu(individual_order ~ year | ecoregion, data = simp,
             distr = "P", zeroinfl = FALSE, area = 1)

pred_years <- expand.grid(
  year = seq(min(simp$year), max(simp$year)),
  ecoregion = unique(simp$ecoregion)
)

pred_years$detcov <- pred_years$ecoregion
pred_years$lambda <- exp(predict(fit, newdata = pred_years, type = "response"))

ggplot() +
  geom_point(data = simp, aes(x = year, y = individual_order), alpha = 0.4) +
  geom_line(data = pred_years, aes(x = year, y = lambda), color = "blue", size = 1.2) +
  facet_wrap(~ ecoregion) +
  labs(x = "Year", y = "Estimated Abundance") +
  theme_minimal()


