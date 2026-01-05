context("analysis")

# Helper function to create test data
create_test_data <- function(n_days = 42, n_groups = 2) {
  my_dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n_days)
  group_names <- letters[1:n_groups]

  data.frame(
    date = rep(my_dates, n_groups),
    grp = rep(group_names, each = n_days),
    x_var = sin(seq(0, 4 * pi, length.out = n_days * n_groups)) + rnorm(n_days * n_groups, sd = 0.1),
    y_var = sin(seq(0, 4 * pi, length.out = n_days * n_groups) + 0.5) + rnorm(n_days * n_groups, sd = 0.1)
  )
}

# ==============================================================================
# cross_corr tests
# ==============================================================================

test_that("cross_corr returns correct structure", {
  set.seed(123)
  df <- create_test_data()

  result <- cross_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    max_lag = 10
  )

  # Check output structure

  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 2)

  expect_true("grp" %in% names(result))
  expect_true("lag" %in% names(result))

  # Check correct number of groups

  expect_equal(nrow(result), 2)
})

test_that("cross_corr returns lags within expected range", {
  set.seed(123)
  df <- create_test_data()

  result <- cross_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    max_lag = 10
  )

  # Lags should be <= 0 (function filters to lags < 1)
  expect_true(all(result$lag <= 0))
  expect_true(all(result$lag >= -10))
})

test_that("cross_corr works with subset_date", {
  set.seed(123)
  df <- create_test_data()

  result <- cross_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    max_lag = 10,
    subset_date = "2020-01-20"
  )

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
})

test_that("cross_corr throws a warning when given data with NAs", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = c(rep(rgamma(83, shape = 4)), NA),
    movement = rgamma(84, shape = 5)
  )

  expect_warning(
    cross_corr(
      dat = df,
      date_var = "date",
      grp_var = "grp",
      x_var = "r_mean",
      y_var = "movement"
    )
  )
})

test_that("cross_corr throws an error when subset_date specified and date_var = NULL", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5)
  )

  expect_error(
    cross_corr(
      dat = df,
      grp_var = "grp",
      x_var = "r_mean",
      y_var = "movement",
      subset_date = "2020-02-01"
    )
  )
})

# ==============================================================================
# rolling_corr tests
# ==============================================================================

test_that("rolling_corr returns correct structure", {
  set.seed(123)
  df <- create_test_data()

  result <- rolling_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    n = 7
  )

  # Check output structure

  expect_s3_class(result, "data.frame")
  expect_true("roll_corr" %in% names(result))

  # Should have same number of rows as input

  expect_equal(nrow(result), nrow(df))
})

test_that("rolling_corr produces values in valid correlation range", {
  set.seed(123)
  df <- create_test_data()

  result <- rolling_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    n = 7
  )

  # Correlation values should be between -1 and 1 (excluding NAs)
  valid_corr <- result$roll_corr[!is.na(result$roll_corr)]
  expect_true(all(valid_corr >= -1 & valid_corr <= 1))
})

test_that("rolling_corr has NAs for first n-1 observations per group", {
  set.seed(123)
  df <- create_test_data(n_days = 20, n_groups = 1)
  n_window <- 7

  result <- rolling_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    n = n_window
  )

  # First n-1 values should be NA

  expect_true(all(is.na(result$roll_corr[1:(n_window - 1)])))
  # Rest should not be NA

  expect_true(all(!is.na(result$roll_corr[n_window:nrow(result)])))
})

test_that("rolling_corr preserves original column names", {
  set.seed(123)
  df <- create_test_data()

  result <- rolling_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "x_var",
    y_var = "y_var",
    n = 7
  )

  expect_true("grp" %in% names(result))
  expect_true("x_var" %in% names(result))
  expect_true("y_var" %in% names(result))
})

test_that("rolling_corr throws an error when date_var is not of class 'Date'", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = as.character(rep(my_dates, 2)),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5)
  )

  expect_error(
    rolling_corr(
      dat = df,
      date_var = "date",
      grp_var = "grp",
      x_var = "r_mean",
      y_var = "movement"
    )
  )
})

# ==============================================================================
# estimate_rt tests
# ==============================================================================

test_that("estimate_rt throws errors when given invalid data", {
  expect_error(
    estimate_rt(data.frame(a = 1:10, b = NA), "a", "a", "b"),
    "NAs are present in your data set"
  )
})

test_that("estimate_rt returns correct structure", {
  skip_on_cran()

  # Create simple case data
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 30),
    province = rep("test_region", 30),
    cases = c(1, 2, 3, 5, 8, 12, 18, 25, 30, 35,
              38, 40, 38, 35, 30, 25, 20, 16, 12, 10,
              8, 6, 5, 4, 3, 3, 2, 2, 1, 1)
  )

  result <- estimate_rt(
    dat = df,
    grp_var = "province",
    date_var = "date",
    incidence_var = "cases"
  )

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true("province" %in% names(result))
  expect_true("r_mean" %in% names(result))
  expect_true("r_median" %in% names(result))
  expect_true("r_q2.5" %in% names(result))
  expect_true("r_q97.5" %in% names(result))
  expect_true("date_start" %in% names(result))
  expect_true("date_end" %in% names(result))
})

test_that("estimate_rt returns positive R values", {
  skip_on_cran()

  df <- data.frame(
    date = seq(as.Date("2020-01-01"), by = "day", length.out = 30),
    province = rep("test_region", 30),
    cases = c(1, 2, 3, 5, 8, 12, 18, 25, 30, 35,
              38, 40, 38, 35, 30, 25, 20, 16, 12, 10,
              8, 6, 5, 4, 3, 3, 2, 2, 1, 1)
  )

  result <- estimate_rt(
    dat = df,
    grp_var = "province",
    date_var = "date",
    incidence_var = "cases"
  )

  # R values should be positive

  expect_true(all(result$r_mean > 0))
  expect_true(all(result$r_median > 0))
})

test_that("estimate_rt works with multiple groups", {
  skip_on_cran()

  cases_pattern <- c(1, 2, 3, 5, 8, 12, 18, 25, 30, 35,
                     38, 40, 38, 35, 30, 25, 20, 16, 12, 10,
                     8, 6, 5, 4, 3, 3, 2, 2, 1, 1)

  df <- data.frame(
    date = rep(seq(as.Date("2020-01-01"), by = "day", length.out = 30), 2),
    province = c(rep("region_a", 30), rep("region_b", 30)),
    cases = c(cases_pattern, cases_pattern * 2)
  )

  result <- estimate_rt(
    dat = df,
    grp_var = "province",
    date_var = "date",
    incidence_var = "cases"
  )

  # Should have results for both regions

  expect_true(all(c("region_a", "region_b") %in% result$province))
})

# ==============================================================================
# calc_percent_change tests
# ==============================================================================

test_that("calc_percent_change returns correct structure", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-21"), by = "day"),
    grp = rep("a", 21),
    my_count = c(100, 100, 100, 100, 100, 100, 100,  # baseline week
                 110, 120, 90, 80, 150, 130, 140,     # week 2
                 50, 60, 70, 80, 90, 100, 110)        # week 3
  )

  result <- calc_percent_change(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    count_var = "my_count",
    n_baseline_periods = 7
  )

  # Check output structure
  expect_s3_class(result, "data.frame")
  expect_true("perc_change" %in% names(result))
  expect_equal(nrow(result), nrow(df))
})

test_that("calc_percent_change calculates correct values", {
  # Create data with known baseline
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day"),
    grp = rep("a", 14),
    my_count = c(rep(100, 7),  # baseline = 100
                 50, 100, 150, 200, 25, 75, 100)  # test values
  )

  result <- calc_percent_change(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    count_var = "my_count",
    n_baseline_periods = 7
  )

  # Check calculated percent changes
  # baseline average = 100, so perc_change = value / 100
  expect_equal(result$perc_change[8], 0.5, tolerance = 0.001)   # 50/100
  expect_equal(result$perc_change[9], 1.0, tolerance = 0.001)   # 100/100
  expect_equal(result$perc_change[10], 1.5, tolerance = 0.001)  # 150/100
  expect_equal(result$perc_change[11], 2.0, tolerance = 0.001)  # 200/100
})

test_that("calc_percent_change works with custom start_date", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-21"), by = "day"),
    grp = rep("a", 21),
    my_count = c(50, 50, 50, 50, 50, 50, 50,    # first week (ignored)
                 100, 100, 100, 100, 100, 100, 100,  # baseline week
                 150, 150, 150, 150, 150, 150, 150)  # test week
  )

  result <- calc_percent_change(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    count_var = "my_count",
    n_baseline_periods = 7,
    start_date = as.Date("2020-01-08")
  )

  # With start_date = 2020-01-08, baseline = 100
  # perc_change for last week should be 1.5 (150/100)
  expect_equal(result$perc_change[15], 1.5, tolerance = 0.001)
})

test_that("calc_percent_change works with multiple groups", {
  df <- data.frame(
    date = rep(seq(as.Date("2020-01-01"), as.Date("2020-01-14"), by = "day"), 2),
    grp = c(rep("a", 14), rep("b", 14)),
    my_count = c(rep(100, 7), rep(200, 7),  # group a: baseline=100, test=200
                 rep(50, 7), rep(100, 7))   # group b: baseline=50, test=100
  )

  result <- calc_percent_change(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    count_var = "my_count",
    n_baseline_periods = 7
  )

  # Group a: 200/100 = 2.0
  result_a <- result[result$grp == "a", ]
  expect_equal(result_a$perc_change[14], 2.0, tolerance = 0.001)

  # Group b: 100/50 = 2.0
  result_b <- result[result$grp == "b", ]
  expect_equal(result_b$perc_change[14], 2.0, tolerance = 0.001)
})

test_that("calc_percent_change throws an error when n_baseline_periods is larger than nrow(dat)", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-11"), by = "day"),
    grp = rep("a", 11),
    my_count = rpois(11, lambda = 3)
  )

  expect_error(
    calc_percent_change(
      dat = df,
      date_var = "date",
      grp_var = "grp",
      count_var = "my_count",
      n_baseline_periods = 14
    )
  )
})

test_that("calc_percent_change throws an error when start_date is the wrong format", {
  df <- data.frame(
    date = seq(as.Date("2020-01-01"), as.Date("2020-01-11"), by = "day"),
    grp = rep("a", 11),
    my_count = rpois(11, lambda = 3)
  )

  expect_error(
    calc_percent_change(
      dat = df,
      date_var = "date",
      grp_var = "grp",
      count_var = "my_count",
      start_date = "2020-1-1"
    )
  )
})
