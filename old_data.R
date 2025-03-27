old_data <- read_csv("./jasper_legacy_data.csv")

old_s <- old_data |>
  select(observer,pointID,latitude,longitude,elevation,ecoregion,temperature,sky,wind,survey_duration,date,species_code,species_common_name,TTFD,start_time,abundance3.3:abundance10,max.species.individuals) |>
  distinct() |>
  rename(location = pointID) |>
  mutate(location = str_to_upper(location),
         location = gsub("([A-Z]+)([0-9]+)", "\\1-\\2", location),
         location = case_when(grepl("VALLEY",location) ~ gsub("(\\w+)-([0-9])", "\\1\\2-", location),
                              TRUE ~ location)) |>
  mutate(recording_date_time = lubridate::mdy(date) + seconds(as.numeric(hms(start_time)))) |>
  relocate(c(location,latitude, longitude, recording_date_time, observer, species_common_name)) |>
  group_by(location, latitude, longitude, recording_date_time, elevation, ecoregion, temperature, sky, wind, survey_duration, observer, species_code, species_common_name, TTFD) |>
  rowwise() |>
  summarise(individual_order = as.numeric(max(across(starts_with("abundance"))))) |> # Max count per visit of individuals
  ungroup() |>
  select(location, latitude, longitude, ecoregion, recording_date_time, observer, species_code, species_common_name, TTFD, individual_order) |>
  distinct() |>
  rename(detection_time = TTFD) |>
  mutate(detection_time = floor(detection_time) * 60 + (detection_time %% 1) * 100) |>
  mutate(location = case_when(grepl("^POBOK",location) ~ gsub("POBOKTON","POBOKTAN",location),
                              grepl("^ELY",location) ~ gsub("ELYISUM","ELYSIUM",location),
                              TRUE ~ location)) |>
  mutate(organization = "JNP", .before = location,
         data_type = "legacy") |>
  mutate(year = year(recording_date_time)) |>
  filter(!year %in% c(2021:2023)) |> # Remove duplicate data between legacy and WildTrax
  select(-year)

