test_that("stats_frequency function", {

  library(ggplot2)
  data(diamonds)

  ds_stats <- stats_frequency(diamonds)

  expect_is(ds_stats, c("data.frame", "tibble"))

  expect_equal(colnames(ds_stats), c("variable", "value", "n", "freq"))

  ds_stats <- stats_frequency(diamonds, color)

  expect_is(ds_stats, c("data.frame", "tibble"))

  expect_equal(colnames(ds_stats), c("color", "variable", "value", "n", "freq"))
})
