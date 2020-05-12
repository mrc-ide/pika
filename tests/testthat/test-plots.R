context("plots")

test_that("Check that plot_corr produces a ggplot object", {

  # fake data
  my_dates <- seq(as.Date("2020-01-01"), as.Date("2020-02-11"), by = "day")

  df <- data.frame(date = rep(my_dates, 2),
                   grp = c(rep("a", 42), rep("b", 42)),
                   r_mean = rgamma(84, shape = 4),
                   movement = rgamma(84, shape = 5),
                   roll_corr = runif(84, -1, 1)
        )

  # plot
  p <- plot_corr(dat = df,
                 date_var = "date",
                 grp_var = "grp",
                 x_var = "r_mean",
                 y_var = "movement",
  )

  expect_is(p, "ggplot")
})
