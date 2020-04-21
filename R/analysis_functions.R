# Functions to perform correlation analysis of two time series


#' This function determines lag at which the cross correlation is highest
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param x_var primary time series (should be a column in dat)
#' @param y_var secondary time series (should be a column in dat)
#' @param max_lag integer value of the maximum number of lags to perform corss correlation for
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export
# Determine cross correlation and max lag between time series ---------------------------
cross_corr <- function(dat, grp_var, x_var, y_var, max_lag = 20){
  # calculate cross correlation for different lags --------------------------------------
  rhos <- dat %>%
    tidyr::nest(gg = -c(grp_var)) %>%
    mutate_at("gg",purrr::map, function(x) ccf(x$x_var, x$y_var, lag.max = max_lag))

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


#' This function splits the time series to lag the primary without impacting the date of the secondary
#' @param dat_x data frame with at least three columns: date, x_var, grp_var
#' @param dat_y data frame with at least three columns: date, y_var, grp_var
#' @param date_var date variable name in dat_x and dat_y
#' @param x_var primary time series (should be a column in dat_x)
#' @param y_var secondary time series (should be a column in dat_y)
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param my_lag integer value of the lag
#' @return data frame
#' @keywords pika
#' @export
create_lag_date <- function(dat_x, dat_y, date_var, x_var, y_var, grp_var, my_lag){
# lag the date variable -----------------------------------------------------------------
dat1 <- dat_x %>%
  mutate({{date_end}} = {{date_end}} + my_lag)

dat1 %>%
  left_join(., dat_y, by = c(date_var, grp_var))
}


#' This function calculates the rolling correlation between two time series
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param x_var primary time series (should be a column in dat)
#' @param y_var secondary time series (should be a column in dat)
#' @param n the number of time points to calculate the rolling correlation for
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export
# Determine rolling correlation between R_t and movement ---------------------------
rolling_corr <- function(dat, grp_var, x_var, y_var, n = 7){
  dat %>%
    group_by({{ grp_var }}) %>%
    # group_by(!! enquo(grp_var)) %>%
    mutate(x = {{ x_var }}, y = {{ y_var }}) %>%
    tq_transmute_xy(
      x = x,
      y = y,
      mutate_fun = runCor,
      n = n,
      col_rename = "roll_corr"
    )
}




