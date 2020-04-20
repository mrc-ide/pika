
#'
#'
#' This function determines lag at which the cross correlation is highest
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param x primary time series (should be a column in dat)
#' @param y secondary time series (should be a column in dat)
#' @param max_lag integer value of the maximum number of lags to perform corss correlation for
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export

# Determine cross correlation and max lag between time series ---------------------------
cross_corr <- function(dat, grp_var, x, y, max_lag = 20){
  # calculate cross correlation for different lags --------------------------------------
  rhos <- dat %>%
    tidyr::nest(gg = -c(grp_var)) %>%
    mutate_at("gg",purrr::map, function(x) ccf(x$x, x$y, lag.max = max_lag))

  # determine lags with max cross correlation -------------------------------------------
  lags_max <- numeric(nrow(rhos)) # empty vector for max lag values by grp_var
  # loop through grp_var values to determine max lag for each member of grp_var ---------
  for (p in 1:nrow(rhos)){
    df <- tibble(
      name = rhos$name[p],
      lags = rhos$gg[[p]]$lag[,1,1],
      cc = rhos$gg[[p]]$acf[,1,1]
      ) %>%
      filter(lags < 1) # restrict to lags less than 1
    lags_max[p] <- df$lags[which(df$cc == max(df$cc))]
  }
  #
  lag_df <- tibble(
    grp = rhos$grp_var,
    lag = lags_max
    )

  # output ------------------------------------------------------------------------------
  return(lag_df)
}



# create date_lag variable
data_phec2 <- data_phec %>% mutate(date_end = date_end + med_lag) %>%
  select(-avg_pp_trips, -trips_scaled)
data_phec2 <- left_join(data_phec2, movement_region, by = c("date_end" = "date", "name" = "region",
                                                            "journey_purpose")) %>%
  select(date_end, name, r_mean, r_q2.5, r_q97.5, avg_pp_trips, trips_scaled, journey_purpose) %>%
  filter(!is.na(avg_pp_trips))
# full data
data_full1 <- data_full %>% mutate(date_end = date_end + med_lag_full) %>%
  select(-avg_pp_trips, -trips_scaled, -date_start)
data_full2 <- left_join(data_full1, movement_dat2 %>%
                          ungroup() %>%
                          select(date, lad_code, journey_purpose, avg_pp_trips, trips_scaled),
                        by = c("date_end" = "date", "code" = "lad_code", "journey_purpose")
)

### Determine weekly rolling correlation between R_t and movement

### determine rolling weekly correlation at district (utla level)
rolling_corr <- data_full2 %>%
  group_by(code, name, journey_purpose) %>%
  filter(!is.na(trips_scaled)) %>%
  tq_transmute_xy(x          = trips_scaled,
                  y          = r_mean,
                  mutate_fun = runCor,
                  n          = 7,
                  col_rename = "roll_corr_weekly") %>%
  mutate(date_end = as.Date(as.POSIXct(date_end, 'GMT')))
### determine rolling weekly correlation at region (phec level)
rolling_corr2 <- data_phec2 %>%
  group_by(name, journey_purpose) %>%
  tq_transmute_xy(x          = trips_scaled,
                  y          = r_mean,
                  mutate_fun = runCor,
                  n          = 7,
                  col_rename = "roll_corr_weekly") %>%
  mutate(date_end = as.Date(as.POSIXct(date_end, 'GMT')))



# join corr data set with movement and rt for plotting
data_full3 <- left_join(data_full2, rolling_corr, by = c("code", "name", "date_end", "journey_purpose")) %>%
  select(date_end, code, name, region, nation, journey_purpose, r_mean, r_q2.5, r_q97.5,
         trips_scaled, avg_pp_trips, roll_corr_weekly)

#saveRDS(data_full3, "uk_rt_move_corr.rds")

data_region <- left_join(data_phec2, rolling_corr2, by = c("name", "date_end", "journey_purpose")) %>%
  select(date_end, name,r_mean, r_q2.5, r_q97.5, journey_purpose, trips_scaled,
         avg_pp_trips, roll_corr_weekly)

#saveRDS(data_region, "uk_rt_move_corr_region.rds")





