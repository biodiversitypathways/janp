old_data <- read_csv("./jasper_legacy_data.csv")

old_standardized <- old_data |>
  select(observer,pointID,latitude,longitude,elevation,ecoregion,temperature,sky,wind,survey_duration,date,species_code,species_common_name,TTFD,start_time,abundance3.3:abundance10,max.species.individuals) |>
  distinct() |>
  rename(location = pointID) |>
  mutate(location = str_to_upper(location)) |>
  mutate(recording_date_time = lubridate::mdy(date) + seconds(as.numeric(hms(start_time)))) |>
  relocate(c(location,latitude, longitude, recording_date_time, observer, species_common_name)) |>
  group_by(location, latitude, longitude, recording_date_time, elevation, ecoregion, temperature, sky, wind, survey_duration, observer, species_code, species_common_name, TTFD) |>
  rowwise() |>
  summarise(individual_order = as.numeric(max(across(starts_with("abundance"))))) |> # Max count per visit of individuals
  ungroup() |>
  select(location, latitude, longitude, recording_date_time, observer, species_common_name, TTFD, individual_order) |>
  distinct()



