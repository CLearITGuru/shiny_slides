
pacman::p_load(
  "tidyverse", "tidycovid19", 
  "EpiModel", "ggplot2", "zoo", "janitor")

clean <- function(.state, .county, ...) {
  jhu_state_county <- download_jhu_csse_covid19_data(
    cached = TRUE,
    type = "us_county") %>%
    filter(
      state == .state,
      county == .county)

  formatted_data <- jhu_state_county %>%
    mutate(
      date_rep = lubridate::ymd(date),
      deaths = as.numeric(deaths),
      new_deaths = deaths - lag(deaths),
      avg_new_deaths = rollmean(new_deaths, 7, na.pad = TRUE, align = "right"),
      confirmed = as.numeric(confirmed),
      CFR = round_half_up(as.numeric(deaths / confirmed), 3),
      rolling7_CFR = rollmean(CFR, 7, na.pad = TRUE, align = "right")) %>%
    drop_na(CFR) %>%
    filter(rolling7_CFR != "NaN")

  return(formatted_data)
}

# end
