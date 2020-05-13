context("analysis")

test_that("estimate_rt throws errors when given invalid data", {
  expect_error(
    estimate_rt(data.frame(a = 1:10, b = NA), "a", "a", "b"),
    "NAs are present in your data set")
})


test_that("cross_corr throws a warning when given data with NAs",{

  # fake data
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(date = rep(my_dates, 2),
                   grp = c(rep("a", 42), rep("b", 42)),
                   r_mean = c(rep(rgamma(83, shape = 4)), NA),
                   movement = rgamma(84, shape = 5)
  )


  expect_warning(cross_corr(dat = df,
                            date_var = "date",
                            grp_var = "grp",
                            x_var = "r_mean",
                            y_var = "movement"))

})

test_that("cross_corr throws an error subset_data specified and date_var = NULL",{

  # fake data
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(date = rep(my_dates, 2),
                   grp = c(rep("a", 42), rep("b", 42)),
                   r_mean = rgamma(84, shape = 4),
                   movement = rgamma(84, shape = 5)
  )

  expect_error(cross_corr(dat = df,
                          grp_var = "grp",
                          x_var = "r_mean",
                          y_var = "movement",
                          subset_date = "2020-02-01"))

})

test_that("rolling_corr throws an error when date_var is not of class 'Date'",{

  # fake data
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(date = as.character(rep(my_dates, 2)),
                   grp = c(rep("a", 42), rep("b", 42)),
                   r_mean = rgamma(84, shape = 4),
                   movement = rgamma(84, shape = 5)
  )

  expect_error(rolling_corr(dat = df,
                            date_var = "date",
                            grp_var = "grp",
                            x_var = "r_mean",
                            y_var = "movement")
               )

})

test_that("calc_percent_change throws an error when n_baseline_periods is larger than nrow(dat)",{

  # fake data
  df <- data.frame(date = seq(as.Date("2020-01-01"), as.Date("2020-01-11"), by = "day"),
                   grp = rep("a", 11),
                   my_count = rpois(11, lambda = 3)
  )

  expect_error(calc_percent_change(dat = df,
                            date_var = "date",
                            grp_var = "grp",
                            count_var = "my_count",
                            n_baseline_periods = 14)
  )

})

test_that("calc_percent_change throws an error when start_date is the wrong format",{

  # fake data
  df <- data.frame(date = seq(as.Date("2020-01-01"), as.Date("2020-01-11"), by = "day"),
                   grp = rep("a", 11),
                   my_count = rpois(11, lambda = 3)
  )

  expect_error(calc_percent_change(dat = df,
                                   date_var = "date",
                                   grp_var = "grp",
                                   count_var = "my_count",
                                   start_date = "2020-1-1")
  )

})
