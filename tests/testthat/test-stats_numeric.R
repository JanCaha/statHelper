test_that("stats_numeric function", {
  library(ggplot2)
  data(diamonds)

  ds_stats <- stats_numeric(diamonds)

  expect_is(ds_stats, c("data.frame", "tibble"))

  expect_equal(colnames(ds_stats), c("column", "n", "mean", "sd", "median", "iqr", "min", "max"))

  stats_to_calculate <- list(
    n = ~ n(),
    mean = ~ mean(.x, na.rm = TRUE),
    sd = ~ sd(.x, na.rm = TRUE)
  )

  ds_stats <- stats_numeric(diamonds, stats_to_calculate = stats_to_calculate)

  expect_is(ds_stats, c("data.frame", "tibble"))

  expect_equal(colnames(ds_stats), c("column", "n", "mean", "sd"))
})

test_that("stats_numeric function fails", {
  library(ggplot2)
  data(diamonds)

  expect_error(
    stats_numeric(1),
    "Variable `data` must be a `data.frame`"
  )

  stats_to_calculate <- 1

  expect_error(
    stats_numeric(diamonds, stats_to_calculate = stats_to_calculate),
    "Variable `stats_to_calculate` must be a named list"
  )

  stats_to_calculate <- list(
    ~ n(),
    ~ mean(.x, na.rm = TRUE)
  )

  expect_error(
    stats_numeric(diamonds, stats_to_calculate = stats_to_calculate),
    "Variable `stats_to_calculate` must be a named list"
  )

  stats_to_calculate <- list(
    a = ~ n(),
    b = ~ mean(.x, na.rm = TRUE),
    c = "aa"
  )

  expect_error(
    stats_numeric(diamonds, stats_to_calculate = stats_to_calculate),
    "Every element of `stats_to_calculate` must be a formula"
  )
})
