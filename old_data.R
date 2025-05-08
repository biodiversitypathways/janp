### Legacy data from Jasper National Park

old_data <- read_csv("./jasper_legacy_data.csv")

### Three different versions of legacy data tiedied and formatted. One is max count, one is mean across the ten minute period and one is mean across the 3 minute period. Goal is to determine the differences between the uses cases for modelling.

# Function to preprocess data
preprocess_data <- function(data, summary_func) {
  data |>
    dplyr::select(observer, pointID, latitude, longitude, elevation, ecoregion, temperature, sky, wind,
                  survey_duration, date, species_code, species_common_name, TTFD, start_time,
                  abundance3.3:abundance10, max.species.individuals) |>
    distinct() |>
    rename(location = pointID) |>
    mutate(location = str_to_upper(location),
           location = gsub("([A-Z]+)([0-9]+)", "\\1-\\2", location),
           location = case_when(
             grepl("VALLEY", location) ~ gsub("(\\w+)-([0-9])", "\\1\\2-", location),
             TRUE ~ location
           )) |>
    mutate(recording_date_time = lubridate::mdy(date) + seconds(as.numeric(hms(start_time)))) |>
    relocate(c(location, latitude, longitude, recording_date_time, observer, species_common_name)) |>
    group_by(location, latitude, longitude, recording_date_time, elevation, ecoregion, temperature, sky, wind,
             survey_duration, observer, species_code, species_common_name, TTFD) |>
    summarise(individual_order = as.numeric(summary_func(across(starts_with("abundance")))), .groups = "drop") |>
    dplyr::select(location, latitude, longitude, ecoregion, recording_date_time, observer, species_code,
                  species_common_name, TTFD, individual_order) |>
    distinct() |>
    rename(detection_time = TTFD) |>
    mutate(detection_time = floor(detection_time) * 60 + (detection_time %% 1) * 100) |>
    mutate(location = case_when(
      grepl("^POBOK", location) ~ gsub("POBOKTON", "POBOKTAN", location),
      grepl("^ELY", location) ~ gsub("ELYISUM", "ELYSIUM", location),
      TRUE ~ location
    )) |>
    mutate(organization = "JNP", .before = location, data_type = "legacy") |>
    mutate(year = year(recording_date_time)) |>
    filter(!year %in% c(2021:2023)) |>
    dplyr::select(-year)
}

# Apply the preprocessing function with specific summaries
old_s_max <- preprocess_data(old_data, max)

old_s_mean <- preprocess_data(old_data, mean)

old_s_3_mean <- preprocess_data(old_data, mean) |>
  filter(detection_time <= 180)  # Additional filtering for 3-minute mean
