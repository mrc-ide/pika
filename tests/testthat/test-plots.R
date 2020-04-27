context("plots")

test_that("Check that plot_corr produces a ggplot object", {

  ## Load in example data
  data(china_rt_estimates)
  data(exante_movement_data)

  # join datasets
  data_joined <- left_join(china_rt_estimates,
                           exante_movement_data,
                           by = c("date","province")
  )

  # calculate rolling correlation
  data_corr <- rolling_corr(dat = data_joined,
                            grp_var = "province",
                            x_var = "r_mean",
                            y_var = "movement",
                            n = 14)

  # plot
  p <- plot_corr(dat = data_corr,
                 date_var = "date",
                 grp_var = "province",
                 x_var = "r_mean",
                 y_var = "movement",
                 x_var_lower = "r_q2.5",
                 x_var_upper = "r_q97.5",
                 y_max = 10
  )

  expect_is(p, "ggplot")
})
