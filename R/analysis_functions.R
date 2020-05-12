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
#' @import dplyr
#' @import tidyr
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
    mutate_at("gg",purrr::map, function(x) stats::ccf(x$x_var, x$y_var, lag.max = max_lag))

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
#' @param dat data frame with columns that correspond to two time series and grouping variable
#' @param date_var character string of date column name (should be of class "Date")
#' @param grp_var character string of column name in dat to be used as grouping variable
#' @param x_var primary time series (should be a column in dat)
#' @param y_var secondary time series (should be a column in dat)
#' @param n the number of time periods over which to calculate rolling correlation
#' @return tibble of lags by grp_var
#' @keywords pika
#' @import TTR
#' @export
# Determine rolling correlation between two time series -----------------------------------
rolling_corr <- function(dat, date_var = "date", grp_var, x_var, y_var, n = 14){

  # check that date_var is of class "Date" ------------------------------------------------
  if(class(dat[,date_var]) != "Date"){stop("date_var must be of class 'Date'")}

  # a little bit of data wrangling to feed into runCor ------------------------------------
  dat1 <- dat %>%
    # rename column names to work inside runCor -------------------------------------------
    rename(x = {{x_var}}, y = {{y_var}}, grp = {{ grp_var }}, date = {{date_var}}) %>%
    filter(!is.na(.data$x), !is.na(.data$y)) %>%
    group_by(.data$grp) %>%
    # determine rolling correlation between x and y ---------------------------------------
    mutate(roll_corr = TTR::runCor(.data$x, .data$y, n)) %>%
    ungroup()

  # rename columns back to original column names ------------------------------------------
  name_index <- which(names(dat1) %in% c("grp", "x", "y"))
  names(dat1)[name_index] <- c(grp_var, x_var, y_var)

  # output --------------------------------------------------------------------------------
  return(dat1)
}


#' This function estimates reproduction number by group using EpiEstim's estimate_R() and
#' then binds the results together into a single data frame
#' @param dat data frame with columns that correspond to two time series and grouping variable(s)
#' @param grp_var character string of column names in dat to be used as grouping variable(s)
#' @param date_var primary time series (should be a column in dat)
#' @param incidence_var secondary time series (should be a column in dat)
#' @param est_method estimation method to be used to estimate R in EpiEstim:
#' c("non_parametric_si","parametric_si","uncertain_si","si_from_data","si_from_sample")
#' @param si_mean mean of serial interval distribution to be specified when
#' est_method = "parametric_si"
#' @param si_std standard deviation of serial interval distribution to be specified when
#' est_method = "parametric_si"
#' @return data frame of mean, median, and 95% credible interval reproduction number
#' @keywords pika
#' @import EpiEstim
#' @export
# Estimate R_t with EpiEstim by grp_var -----------------------------------------------------
estimate_rt <- function(dat, grp_var, date_var, incidence_var, est_method = "parametric_si",
                        si_mean = 6.48, si_std = 3.83) {
  # check for NAs -----------------------------------------------------------------------------
  if (nrow(dat[which(is.na(dat[[incidence_var]])),]) > 0){
    stop("Error: NAs are present in your data set, please remove them before estimating Rt.")
  }
  # estimate Rt by group-----------------------------------------------------------------------
  r <- dat %>%
    # rename column names to work inside EpiEstim ---------------------------------------------
  rename(dates = {{date_var}}, I = {{incidence_var}}, grp = {{ grp_var }}) %>%
    # group by grp
    tidyr::nest(gg = -"grp") %>%
    mutate_at("gg", purrr::map,function(x) estimate_R(x, method=est_method,
                                                      config = make_config(list(mean_si = si_mean,
                                                                                std_si = si_std)))
    )
  # loop through groups to extract R estimates ------------------------------------------------
  R_t <- list()
  for(i in 1:length(r$grp)){
    R_t[[i]] <- r$gg[[i]]$R %>%
      mutate(grp = r$grp[i],
             date_start = r$gg[[i]]$dates[.data$t_start],
             date_end = r$gg[[i]]$dates[.data$t_start][.data$t_end]) %>%
      rename(r_mean = .data$`Mean(R)`, r_q2.5 = .data$`Quantile.0.025(R)`,
             r_q97.5 = .data$`Quantile.0.975(R)`,
             r_median = .data$`Median(R)`) %>%
      select(.data$date_start, .data$date_end, .data$grp, .data$r_mean, .data$r_q2.5,
             .data$r_q97.5, .data$r_median)
  }

  # bind rows to create single data frame -----------------------------------------------------
  r_dat <- bind_rows(R_t)

  # rename columns back to original column names ----------------------------------------------
  name_index <- which(names(r_dat) == "grp")
  names(r_dat)[name_index] <- grp_var

  # output ------------------------------------------------------------------------------------
  return(r_dat)
}


#' This function converts a count variable over time into a percent change based on the average
#' value in the specified baseline period. This function was written for application to
#' mobility data, where the percent change in mobility over time relative to baseline is of
#' interest. However, this function can be applied to any count type time series.
#' @param dat data frame with columns that correspond to count variable and grouping variable
#' @param grp_var character string of column name in dat to be used as grouping variable
#' @param date_var character string of the name of the date column (should be of class "Date")
#' @param count_var character string of the name of the count column, such as number of trips
#' @param n_baseline_periods Number of periods to calculate baseline average over. For example,
#' if the time series is days, n_baseline_periods = 7 for a baseline week.
#' @param start_date start date of baseline period (character string)
#' @return data frame of with an additional column of the percent change relative to baseline
#' @keywords pika
#' @export
# convert counts to % change -------------------------------------------------------------
calc_percent_change <- function(dat, date_var = "date", grp_var, count_var,
                                n_baseline_periods = 7, start_date = NULL){

  # check that there are enough observations for n_baseline_periods ----------------------
  if(n_baseline_periods > nrow(dat)){
    stop("Number of baseline periods is larger than number of observations in the input dataset")
  }

  # chack that start_date is a valid format ----------------------------------------------
  if(!is.null(start_date) & (start_date %in% df[,date_var]) == FALSE){
    stop("start_date does not match any dates in input dataset")
  }

  dat1 <- dat %>%
  # rename column names to work inside piping -------------------------------------------
  rename(date = {{date_var}}, grp = {{grp_var}}, counts = {{ count_var }}) # %>%
  #dplyr::select(.data$date, .data$grp, .data$counts)

  # define minimum date -------------------------------------------------------------------
  if(is.null(start_date)){start_date <- min(dat1$date)}
  # define baseline dates -----------------------------------------------------------------
  baseline_dates <- seq(start_date, start_date + (n_baseline_periods - 1), by = 1)

  # mean movement for baseline days -------------------------------------------------------
  baseline <- dat1 %>%
    filter(date %in% baseline_dates) %>%
    group_by(.data$grp) %>%
    summarise_at(.vars = "counts", .funs = "mean") %>%
    rename("baseline_counts" = "counts")

  # calculate percentage change in movement relative to baseline --------------------------
  dat1a <- left_join(dat1, baseline, by = "grp")
  rtn <- dat1a %>%
    mutate(perc_change = .data$counts / .data$baseline_counts) %>%
    dplyr::select(-.data$baseline_counts)

  # rename columns back to original column names ------------------------------------------
  name_index <- which(names(rtn) == "date"); names(rtn)[name_index] <- date_var
  name_index <- which(names(rtn) == "grp"); names(rtn)[name_index] <- grp_var
  name_index <- which(names(rtn) == "counts"); names(rtn)[name_index] <- count_var

  return(rtn)
}

