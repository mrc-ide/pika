# Prepare bundled wastewater and case-count example datasets ---------------------------
# Sources:
#   - CDC National Wastewater Surveillance System (NWSS), "CDC Wastewater Data for
#     SARS-CoV-2" (dataset j9g8-acpt), https://data.cdc.gov/resource/j9g8-acpt
#   - The New York Times, "Coronavirus (Covid-19) Data in the United States",
#     https://github.com/nytimes/covid-19-data (us-states.csv)
#
# Both series are aggregated to state-week to align irregular wastewater sampling
# dates with daily cumulative case reporting, and restricted to California, Ohio, and
# New York (good sample density) over 2020-01-01 to 2023-03-23 (last date the NYT
# repository was updated).

library(dplyr)
library(tidyr)

states  <- c("ca", "oh", "ny")
date_lo <- "2020-01-01"
date_hi <- "2023-03-31"

# floor a Date to the Monday that starts its ISO week -----------------------------------
floor_date_to_monday <- function(date) {
  date - (as.integer(format(date, "%u")) - 1)
}

# fetch raw wastewater samples from CDC NWSS (paginated Socrata CSV export) -------------
fetch_state <- function(state, page_size = 5000) {
  offset <- 0
  pages <- list()
  repeat {
    resp <- httr::GET(
      "https://data.cdc.gov/resource/j9g8-acpt.csv",
      query = list(
        state_territory = state,
        `$where`  = sprintf("sample_collect_date between '%s' and '%s'", date_lo, date_hi),
        `$select` = "state_territory,site,county_fips,counties_served,population_served,sample_collect_date,pcr_target,pcr_target_flowpop_lin",
        `$limit`  = page_size,
        `$offset` = offset
      )
    )
    httr::stop_for_status(resp)
    page <- read.csv(text = httr::content(resp, as = "text", encoding = "UTF-8"), stringsAsFactors = FALSE)
    if (nrow(page) == 0) break
    page$county_fips <- as.character(page$county_fips)
    page$site        <- as.character(page$site)
    pages[[length(pages) + 1]] <- page
    offset <- offset + page_size
    if (nrow(page) < page_size) break
  }
  bind_rows(pages)
}

raw_samples <- bind_rows(lapply(states, fetch_state))

# clean and aggregate to state-week ------------------------------------------------------
state_lookup <- c(ca = "California", oh = "Ohio", ny = "New York")

samples_clean <- raw_samples %>%
  filter(
    tolower(pcr_target) == "sars-cov-2",
    !is.na(pcr_target_flowpop_lin),
    !is.na(population_served),
    population_served > 0
  ) %>%
  mutate(
    date  = as.Date(sample_collect_date),
    state = unname(state_lookup[state_territory])
  )

wastewater_data <- samples_clean %>%
  mutate(week = floor_date_to_monday(date)) %>%
  group_by(state, week) %>%
  summarise(
    conc     = weighted.mean(pcr_target_flowpop_lin, w = population_served),
    n_sites  = n_distinct(site),
    n_samples = n(),
    .groups = "drop"
  ) %>%
  rename(date = week) %>%
  arrange(state, date) %>%
  as.data.frame()

# fetch and clean NYT state-level case counts --------------------------------------------
nyt_states <- read.csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv",
  stringsAsFactors = FALSE
)

covid_case_data <- nyt_states %>%
  filter(state %in% unname(state_lookup)) %>%
  mutate(date = as.Date(date)) %>%
  filter(date >= as.Date(date_lo), date <= as.Date(date_hi)) %>%
  arrange(state, date) %>%
  group_by(state) %>%
  mutate(new_cases = pmax(cases - lag(cases), 0)) %>%
  filter(!is.na(new_cases)) %>%
  ungroup() %>%
  mutate(week = floor_date_to_monday(date)) %>%
  group_by(state, week) %>%
  summarise(cases = sum(new_cases), .groups = "drop") %>%
  rename(date = week) %>%
  arrange(state, date) %>%
  as.data.frame()

# save bundled datasets --------------------------------------------------------------
usethis::use_data(wastewater_data, overwrite = TRUE)
usethis::use_data(covid_case_data, overwrite = TRUE)
