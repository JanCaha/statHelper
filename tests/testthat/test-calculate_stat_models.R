test_that("calculate_stat_models function", {

  library(ggplot2)
  library(dplyr)

  data(diamonds)

  diamonds <- diamonds %>%
    filter(color %in% c("D", "E"))

  models <- list(carat ~ color,
                 depth ~ color,
                 x ~ color)

  df_stats <- calculate_stat_models(diamonds, models, t.test)

  expect_is(df_stats, c("data.frame", "tibble"))

  expect_equal(colnames(df_stats),
               c("model", "func", "estimate", "estimate1", "estimate2", "statistic", "p.value", "parameter", "conf.low", "conf.high", "method", "alternative"))

  expect_equal(nrow(df_stats), length(models))

  df_stats <- calculate_stat_models(diamonds, carat ~ color, t.test)

  expect_equal(nrow(df_stats), 1)

  df_stats <- calculate_stat_models(diamonds, models, t.test, alternative = "less")

  expect_equal(nrow(df_stats), length(models))

  expect_equal(unique(df_stats$alternative), c("less"))

})

test_that("calculate_stat_models function error", {

  library(ggplot2)
  library(dplyr)

  data(diamonds)

  diamonds <- diamonds %>%
    filter(color %in% c("D", "E"))

  models <- list(carat ~ color,
                 depth ~ color,
                 x ~ color,
                 1)
  expect_error(calculate_stat_models(diamonds, models, t.test),
               "Every element of `models` must be a formula")

  expect_error(calculate_stat_models(diamonds, "model", t.test),
               "Every element of `models` must be a formula")
})
