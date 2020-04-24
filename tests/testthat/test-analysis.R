context("analysis")

test_that("estimate_rt throws errors when given invalid data", {
  expect_error(
    estimate_rt(data.frame(a = 1:10, b = NA), "a", "a", "b"),
    "NAs are present in your data set")
})
