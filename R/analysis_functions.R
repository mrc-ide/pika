# Functions to perform correlation analysis of two time series --------------------------


#' Find the lag at which cross-correlation between two time series is maximised
#'
#' Computes the cross-correlation function (CCF) between \code{x_var} and
#' \code{y_var} for lags from \code{-max_lag} to 0 using
#' \code{\link[stats]{ccf}}, then returns the lag with the highest CCF for each
#' group. Only non-positive lags are considered (i.e. \code{x_var} leading
#' \code{y_var}), reflecting the assumption that changes in the primary series
#' precede changes in the secondary series.
#'
#' @param dat A data frame containing the two time series and a grouping column.
#' @param date_var Character string giving the name of the date column. Required
#'   when \code{subset_date} is non-\code{NULL}.
#' @param grp_var Character string giving the name of the grouping column. The
#'   CCF is computed separately within each group.
#' @param x_var Character string giving the name of the primary (leading) time
#'   series column.
#' @param y_var Character string giving the name of the secondary (lagged) time
#'   series column.
#' @param max_lag Integer. Maximum number of lags to evaluate. CCF is computed
#'   for lags \code{-max_lag} to 0. Default is 20.
#' @param subset_date Character string in the same format as \code{date_var}.
#'   If supplied, only rows with dates on or before \code{subset_date} are used.
#'   Requires \code{date_var} to be specified.
#'
#' @return A tibble with one row per group containing:
#'   \describe{
#'     \item{\code{<grp_var>}}{Group identifier; column name matches \code{grp_var}.}
#'     \item{\code{lag}}{Integer \eqn{\leq 0}. The lag at which the CCF between
#'       \code{x_var} and \code{y_var} is highest within that group.}
#'   }
#'
#' @seealso \code{\link{rolling_corr}} to compute rolling correlation at the
#'   identified lag; \code{\link[stats]{ccf}} for the underlying CCF method.
#'
#' @examples
#' \dontrun{
#' lags <- cross_corr(
#'   dat      = my_data,
#'   date_var = "date",
#'   grp_var  = "region",
#'   x_var    = "r_mean",
#'   y_var    = "movement",
#'   max_lag  = 14
#' )
#' }
#'
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
  tidyr::nest(gg = -tidyselect::all_of(grp_var)) %>%
    mutate(gg = purrr::map(.data$gg, function(x) stats::ccf(x$x_var, x$y_var, lag.max = max_lag)))

  # determine lags with max cross correlation -------------------------------------------
  lags_max <- numeric(nrow(rhos)) # empty vector for max lag values by grp_var ----------

  for (p in seq_len(nrow(rhos))){ # loop through grp_var values to determine max lag for each --
    # member of grp_var ------------------------------------------
    df <- tibble(
      lags = rhos$gg[[p]]$lag[,1,1],
      cc = rhos$gg[[p]]$acf[,1,1]
    ) %>%
      filter(lags < 1) # restrict to lags less than 1 -----------------------------------
    lags_max[p] <- df$lags[which(df$cc == max(df$cc))]
  }
  # create tibble of max lag by grp_var -------------------------------------------------
  lag_df <- tibble(
    grp = rhos[[grp_var]],
    lag = lags_max
  )
  names(lag_df)[1] <- grp_var

  # output ------------------------------------------------------------------------------
  return(lag_df)
}


#' Calculate rolling (moving-window) correlation between two time series
#'
#' Computes the Pearson correlation between \code{x_var} and \code{y_var}
#' over a rolling window of \code{n} time periods within each group, using
#' \code{\link[TTR]{runCor}}. The first \code{n - 1} observations in each
#' group will be \code{NA} because there are insufficient data to fill the
#' window.
#'
#' @param dat A data frame containing the two time series, a date column, and
#'   a grouping column.
#' @param date_var Character string giving the name of the date column. Must be
#'   of class \code{Date}. Default is \code{"date"}.
#' @param grp_var Character string giving the name of the grouping column.
#'   Rolling correlation is computed separately within each group.
#' @param x_var Character string giving the name of the primary time series column.
#' @param y_var Character string giving the name of the secondary time series column.
#' @param n Integer. Width of the rolling window in time periods. Default is 14.
#'
#' @return A data frame with the same columns as the input plus one additional
#'   numeric column, \code{roll_corr}, containing the rolling Pearson
#'   correlation between \code{x_var} and \code{y_var}. Values range from -1
#'   to 1. The first \code{n - 1} observations per group are \code{NA}. Note
#'   that rows where \code{x_var} or \code{y_var} are \code{NA} are removed
#'   before the rolling correlation is computed, so the returned frame may have
#'   fewer rows than the input.
#'
#' @seealso \code{\link{cross_corr}} to identify the optimal lag before
#'   computing rolling correlation; \code{\link[TTR]{runCor}} for the
#'   underlying method; \code{\link{plot_corr}} to visualise the result.
#'
#' @examples
#' \dontrun{
#' data_corr <- rolling_corr(
#'   dat      = my_data,
#'   date_var = "date",
#'   grp_var  = "region",
#'   x_var    = "r_mean",
#'   y_var    = "movement",
#'   n        = 14
#' )
#' }
#'
#' @keywords pika
#' @import TTR
#' @export
# Determine rolling correlation between two time series -----------------------------------
rolling_corr <- function(dat, date_var = "date", grp_var, x_var, y_var, n = 14){

  # check that date_var is of class "Date" ------------------------------------------------
  if(!inherits(dat[[date_var]], "Date")){stop("date_var must be of class 'Date'")}

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


#' Estimate the effective reproduction number (Rt) over time by group
#'
#' A grouped wrapper around \code{\link[EpiEstim]{estimate_R}} (Cori et al.
#' 2013). For each group, Rt is estimated in a sliding weekly window using a
#' Bayesian framework with a Gamma-distributed serial interval. Results from all
#' groups are combined into a single data frame.
#'
#' The default serial interval parameters (\code{si_mean = 6.48},
#' \code{si_std = 3.83}) are from Nishiura et al. (2020) for COVID-19 and
#' should be updated for other pathogens.
#'
#' @param dat A data frame with at least a date column, an incidence column,
#'   and a grouping column. No \code{NA} values are permitted in
#'   \code{incidence_var}.
#' @param grp_var Character string giving the name of the grouping column. Rt
#'   is estimated independently for each group.
#' @param date_var Character string giving the name of the date column.
#' @param incidence_var Character string giving the name of the daily incidence
#'   (case count) column.
#' @param est_method Character string specifying the serial interval estimation
#'   method passed to \code{\link[EpiEstim]{estimate_R}}. One of
#'   \code{"parametric_si"} (default), \code{"non_parametric_si"},
#'   \code{"uncertain_si"}, \code{"si_from_data"}, or \code{"si_from_sample"}.
#' @param si_mean Mean of the serial interval distribution (days). Used when
#'   \code{est_method = "parametric_si"}. Default is 6.48 (COVID-19;
#'   Nishiura et al. 2020).
#' @param si_std Standard deviation of the serial interval distribution (days).
#'   Used when \code{est_method = "parametric_si"}. Default is 3.83 (COVID-19;
#'   Nishiura et al. 2020).
#'
#' @return A data frame with one row per estimation window per group, containing:
#'   \describe{
#'     \item{\code{date_start}}{Start date of the estimation window.}
#'     \item{\code{date_end}}{End date of the estimation window.}
#'     \item{\code{<grp_var>}}{Group identifier; column name matches \code{grp_var}.}
#'     \item{\code{r_mean}}{Posterior mean Rt.}
#'     \item{\code{r_median}}{Posterior median Rt.}
#'     \item{\code{r_q2.5}}{2.5th percentile of the posterior (lower 95\% credible interval).}
#'     \item{\code{r_q97.5}}{97.5th percentile of the posterior (upper 95\% credible interval).}
#'   }
#'
#' @references
#' Cori A, Ferguson NM, Fraser C, Cauchemez S (2013). A new framework and
#' software to estimate time-varying reproduction numbers during epidemics.
#' \emph{American Journal of Epidemiology}, 178(9), 1505--1512.
#' \doi{10.1093/aje/kwt133}
#'
#' Nishiura H, Linton NM, Akhmetzhanov AR (2020). Serial interval of novel
#' coronavirus (COVID-19) infections. \emph{International Journal of Infectious
#' Diseases}, 93, 284--286. \doi{10.1016/j.ijid.2020.02.060}
#'
#' @seealso \code{\link[EpiEstim]{estimate_R}} for full estimation control,
#'   including non-parametric serial intervals.
#'
#' @examples
#' \dontrun{
#' rt_estimates <- estimate_rt(
#'   dat           = china_case_data,
#'   grp_var       = "province",
#'   date_var      = "date",
#'   incidence_var = "cases"
#' )
#' }
#'
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
    mutate(gg = purrr::map(.data$gg, function(x) estimate_R(x, method=est_method,
                                                      config = make_config(list(mean_si = si_mean,
                                                                                std_si = si_std)))))
  # loop through groups to extract R estimates ------------------------------------------------
  R_t <- list()
  for(i in seq_along(r$grp)){
    R_t[[i]] <- r$gg[[i]]$R %>%
      mutate(grp = r$grp[i],
             date_start = r$gg[[i]]$dates[.data$t_start],
             date_end = r$gg[[i]]$dates[.data$t_end]) %>%
      rename(r_mean = "Mean(R)", r_q2.5 = "Quantile.0.025(R)",
             r_q97.5 = "Quantile.0.975(R)",
             r_median = "Median(R)") %>%
      select("date_start", "date_end", "grp", "r_mean", "r_q2.5",
             "r_q97.5", "r_median")
  }

  # bind rows to create single data frame -----------------------------------------------------
  r_dat <- bind_rows(R_t)

  # rename columns back to original column names ----------------------------------------------
  name_index <- which(names(r_dat) == "grp")
  names(r_dat)[name_index] <- grp_var

  # output ------------------------------------------------------------------------------------
  return(r_dat)
}


#' Convert a count time series to fractional change relative to a baseline period
#'
#' For each group, computes the mean of \code{count_var} over a baseline period
#' of \code{n_baseline_periods} consecutive time steps starting at
#' \code{start_date} (or the earliest date if \code{start_date} is \code{NULL}).
#' Each observation is then expressed as a fractional change relative to that
#' baseline mean:
#'
#' \deqn{\texttt{perc\_change} = \frac{\texttt{count} -
#'   \texttt{baseline mean}}{\texttt{baseline mean}}}
#'
#' A value of 0 indicates no change from baseline; -0.5 indicates a 50\%
#' decrease; 1.0 indicates a doubling. Originally developed for population
#' mobility data but applicable to any non-negative count series.
#'
#' @param dat A data frame containing a count column, a date column, and a
#'   grouping column.
#' @param date_var Character string giving the name of the date column (class
#'   \code{Date}). Default is \code{"date"}.
#' @param grp_var Character string giving the name of the grouping column. The
#'   baseline mean is computed separately per group.
#' @param count_var Character string giving the name of the count column.
#' @param n_baseline_periods Integer. Number of consecutive time steps used to
#'   compute the baseline mean. For daily data, \code{7} gives a one-week
#'   baseline. Default is 7.
#' @param start_date Start date of the baseline period. Accepts a \code{Date}
#'   object or a character string in \code{"YYYY-MM-DD"} format
#'   (e.g. \code{"2020-01-13"}). If \code{NULL} (default), the earliest date
#'   across the combined dataset is used as the baseline start.
#'
#' @return The input data frame with one additional numeric column,
#'   \code{perc_change}, giving each observation as a fractional change
#'   relative to the group-specific baseline mean (0 = no change from
#'   baseline, -1 = zero counts, positive values = above baseline).
#'
#' @examples
#' \dontrun{
#' dat_pct <- calc_percent_change(
#'   dat                = mobility_data,
#'   date_var           = "date",
#'   grp_var            = "region",
#'   count_var          = "trips",
#'   n_baseline_periods = 7,
#'   start_date         = "2020-01-13"
#' )
#' }
#'
#' @keywords pika
#' @export
# convert counts to % change -------------------------------------------------------------
calc_percent_change <- function(dat, date_var = "date", grp_var, count_var,
                                n_baseline_periods = 7, start_date = NULL){

  # check that there are enough observations for n_baseline_periods ----------------------
  min_group_size <- min(table(dat[[grp_var]]))
  if(n_baseline_periods > min_group_size){
    stop("Number of baseline periods is larger than the number of observations per group in the input dataset")
  }

  # check that start_date is a valid format ----------------------------------------------
  if(!is.null(start_date)){
    if(!(start_date %in% dat[[date_var]])){
      stop("start_date does not match any dates in input dataset")
    }
    start_date <- as.Date(start_date)
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
    summarise(counts = mean(counts), .groups = "drop") %>%
    rename("baseline_counts" = "counts")

  # calculate percentage change in movement relative to baseline --------------------------
  dat1a <- left_join(dat1, baseline, by = "grp")
  rtn <- dat1a %>%
    mutate(perc_change = .data$counts / .data$baseline_counts - 1) %>%
    dplyr::select(-"baseline_counts")

  # rename columns back to original column names ------------------------------------------
  name_index <- which(names(rtn) == "date"); names(rtn)[name_index] <- date_var
  name_index <- which(names(rtn) == "grp"); names(rtn)[name_index] <- grp_var
  name_index <- which(names(rtn) == "counts"); names(rtn)[name_index] <- count_var

  return(rtn)
}

