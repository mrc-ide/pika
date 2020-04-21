# Functions to perform correlation analysis of two time series --------------------------


#' This function determines lag at which the cross correlation is highest
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param date_var character string of the column name that corresponds to the date variable
#' date_var must be specified if subset_date is not null.
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param x_var primary time series (should be a column in dat)
#' @param y_var secondary time series (should be a column in dat)
#' @param max_lag integer value of the number of lags to perform cross correlation for
#' @param subset_date a character string of the same format as the date variable
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export
# Determine cross correlation and max lag between time series ---------------------------
cross_corr <- function(dat, date_var = NULL, grp_var, x_var, y_var, max_lag = 20,
                       subset_date = NULL){
  # check for missing values ------------------------------------------------------------
  if(any(is.na(dat[,x_var])) || any(is.na(dat[,y_var]))){
    warning("There are NAs in your data set and they will be removed before calculating
            the cross correlation")
  }

  # check that date_var specified when subset_date is not null --------------------------
  if(!is.null(subset_date) & is.null(date_var)){
    stop("date_var must be specified when subset_date is non-null")
  }

  # subset data by subset_date if subset_date is not NULL -------------------------------
  if (!is.null(subset_date)){
    dat <- dat[ dat[, date_var] <= subset_date, ]
   }

  # calculate cross correlation for different lags --------------------------------------
  rhos <- dat %>%
    rename(x_var = {{x_var}}, y_var = {{y_var}}) %>% # rename x_var and y_var for ccf ---
    filter(!is.na(x_var), !is.na(y_var)) %>% # remove NA values -------------------------
    tidyr::nest(gg = -c(grp_var)) %>%
    mutate_at("gg",purrr::map, function(x) ccf(x$x_var, x$y_var, lag.max = max_lag))

  # determine lags with max cross correlation -------------------------------------------
  lags_max <- numeric(nrow(rhos)) # empty vector for max lag values by grp_var ----------

  for (p in 1:nrow(rhos)){ # loop through grp_var values to determine max lag for each --
                           # member of grp_var ------------------------------------------
    df <- tibble(
      name = rhos[p,1],
      lags = rhos$gg[[p]]$lag[,1,1],
      cc = rhos$gg[[p]]$acf[,1,1]
      ) %>%
      filter(lags < 1) # restrict to lags less than 1 -----------------------------------
    lags_max[p] <- df$lags[which(df$cc == max(df$cc))]
  }
  # create tibble of max lag by grp_var -------------------------------------------------
  lag_df <- tibble(
    grp = rhos[,grp_var],
    lag = lags_max
    )

  # output ------------------------------------------------------------------------------
  return(lag_df)
}


#' This function calculates the rolling correlation between two time series
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param x_var primary time series (should be a column in dat)
#' @param y_var secondary time series (should be a column in dat)
#' @param period the time period over which to calculate rolling corr, values = c("weekly", "biweekly")
#' @return tibble of lags by grp_var
#' @keywords pika
#' @export
# Determine rolling correlation between two time series ----------------------------------
rolling_corr <- function(dat, grp_var, x_var, y_var, period = "weekly"){
  # a little bit of data wrangling to feed into tq_transmutate_xy ------------------------
  dat1 <- dat %>%
    rename(x = {{x_var}}, y = {{y_var}}, grp = {{ grp_var }}) %>%
    filter(!is.na(x), !is.na(y)) %>%
    group_by(grp)

  # if period == "weekly", n = 7 ---------------------------------------------------------
  if(period == "weekly"){
    dat1 %>%
      tq_transmute_xy(
        x = x,
        y = y,
        mutate_fun = runCor,
        n = 7,
      col_rename = "roll_corr"
    )
  }

  # if period == "biweekly", n = 14 ------------------------------------------------------
  if(period == "biweekly"){
    dat1 %>%
      tq_transmute_xy(
        x = x,
        y = y,
        mutate_fun = runCor,
        n = 14,
        col_rename = "roll_corr"
      )
  }

}




