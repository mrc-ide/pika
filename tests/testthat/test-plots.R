context("plots")

# ==============================================================================
# plot_corr tests
# ==============================================================================

test_that("plot_corr produces a ggplot object", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_corr works with confidence bands", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    r_lower = rgamma(84, shape = 3),
    r_upper = rgamma(84, shape = 5),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  # Ensure lower < mean < upper
  df$r_lower <- df$r_mean * 0.8
  df$r_upper <- df$r_mean * 1.2

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement",
    x_var_lower = "r_lower",
    x_var_upper = "r_upper"
  )

  expect_s3_class(p, "ggplot")

  # Check that ribbon layer exists
  layer_types <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomRibbon" %in% layer_types)
})

test_that("plot_corr works with custom facet labels", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  custom_labels <- c("a" = "Group A", "b" = "Group B")

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement",
    facet_labels = custom_labels
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_corr works with custom legend labels", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  custom_legend <- c("Correlation", "Rt", "Mobility")

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement",
    legend_labels = custom_legend
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_corr works with y_max parameter", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement",
    y_max = 10
  )

  expect_s3_class(p, "ggplot")

  # Check y-axis limits
  y_scale <- p$scales$get_scales("y")
  expect_equal(y_scale$limits[2], 10)
})

test_that("plot_corr works with custom colors", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  custom_colors <- c("red", "blue", "green")
  custom_legend <- c("Correlation", "Rt", "Mobility")

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement",
    col_values = custom_colors,
    legend_labels = custom_legend
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_corr includes horizontal reference lines", {
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(
    date = rep(my_dates, 2),
    grp = c(rep("a", 42), rep("b", 42)),
    r_mean = rgamma(84, shape = 4),
    movement = rgamma(84, shape = 5),
    roll_corr = runif(84, -1, 1)
  )

  p <- plot_corr(
    dat = df,
    date_var = "date",
    grp_var = "grp",
    x_var = "r_mean",
    y_var = "movement"
  )

  # Check that hline layers exist (for -1, 0, 1 reference lines)
  layer_types <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true(sum(layer_types == "GeomHline") >= 3)
})

# ==============================================================================
# plot_lag tests
# ==============================================================================

test_that("plot_lag produces a ggplot object", {
  df <- data.frame(
    grp = letters[1:10],
    lag = sample(-10:0, 10, replace = TRUE)
  )

  p <- plot_lag(
    dat = df,
    lag_var = "lag"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_lag works with custom bin width", {
  df <- data.frame(
    grp = letters[1:20],
    lag = sample(-20:0, 20, replace = TRUE)
  )

  p <- plot_lag(
    dat = df,
    lag_var = "lag",
    bins = 5
  )

  expect_s3_class(p, "ggplot")

  # Check that histogram layer exists
  layer_types <- sapply(p$layers, function(l) class(l$geom)[1])
  expect_true("GeomBar" %in% layer_types)
})

test_that("plot_lag handles single lag value", {
  df <- data.frame(
    grp = letters[1:5],
    lag = rep(-5, 5)
  )

  p <- plot_lag(
    dat = df,
    lag_var = "lag"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_lag handles negative and zero lags", {
  df <- data.frame(
    grp = letters[1:10],
    lag = c(-10, -8, -6, -4, -2, 0, -1, -3, -5, -7)
  )

  p <- plot_lag(
    dat = df,
    lag_var = "lag"
  )

  expect_s3_class(p, "ggplot")
})

test_that("plot_lag has no legend", {
  df <- data.frame(
    grp = letters[1:10],
    lag = sample(-10:0, 10, replace = TRUE)
  )

  p <- plot_lag(
    dat = df,
    lag_var = "lag"
  )

  # Check legend position is none
  expect_equal(p$theme$legend.position, "none")
})

test_that("plot_lag has clean theme (no grid lines)", {
  df <- data.frame(
    grp = letters[1:10],
    lag = sample(-10:0, 10, replace = TRUE)
  )

  p <- plot_lag(
    dat = df,
    lag_var = "lag"
  )

  # Check that panel background is blank
  expect_s3_class(p$theme$panel.background, "element_blank")
  expect_s3_class(p$theme$panel.grid.major, "element_blank")
  expect_s3_class(p$theme$panel.grid.minor, "element_blank")
})
