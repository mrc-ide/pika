context("plots")

test_that("Check that plot_corr produces a ggplot object", {

  ## Load in example data
  data(china_case-data)
  data(exante_movement_data)

  # estiamte Rt
  rt_estimates <- estimate_rt(dat = china_case_data,
                              grp_var = "province",
                              date_var = "date",
                              incidence_var = "cases"
                              ) %>%
    mutate(province = to_snake_case(province)) %>%
    select(-date_start) %>%
    rename("date" = "date_end")

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
