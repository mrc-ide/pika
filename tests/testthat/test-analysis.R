context("analysis")

test_that("estimate_rt throws errors when given invalid data", {
  expect_error(
    estimate_rt(data.frame(a = 1:10, b = NA), "a", "a", "b"),
    "NAs are present in your data set")
})


test_that("cross_corr throws a warning when given data with NAs",{

  data(china_case_data)
  data(exante_movement_data)

  rt_estimates <- estimate_rt(dat = china_case_data,
                              grp_var = "province",
                              date_var = "date",
                              incidence_var = "cases"
                              ) %>%
    mutate(province = to_snake_case(province)) %>%
    select(-date_start) %>%
    rename("date" = "date_end")

  data_joined <- left_join(rt_estimates,
                           exante_movement_data,
                           by = c("date","province"))

  expect_warning(cross_corr(dat = data_joined,
                            date_var = "date",
                            grp_var = "province",
                            x_var = "r_mean",
                            y_var = "movement"))

})

test_that("cross_corr throws an error subset_data specified and date_var = NULL",{

  data(china_case_data)
  data(exante_movement_data)

  rt_estimates <- estimate_rt(dat = china_case_data,
                              grp_var = "province",
                              date_var = "date",
                              incidence_var = "cases"
                              ) %>%
    mutate(province = to_snake_case(province)) %>%
    select(-date_start) %>%
    rename("date" = "date_end")

  data_joined <- left_join(rt_estimates,
                           exante_movement_data,
                           by = c("date","province")) %>%
    filter(!is.na(movement))

  expect_error(cross_corr(dat = data_joined,
                          grp_var = "province",
                          x_var = "r_mean",
                          y_var = "movement",
                          subset_date = "2020-02-15"))

})
