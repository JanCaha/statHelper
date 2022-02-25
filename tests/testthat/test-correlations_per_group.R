test_that("calculate_stat_models function", {

  df_result <- correlations_per_group(mtcars, wt, qsec, group_var = cyl)

  expect_is(df_result, "data.frame")
  expect_equal(ncol(df_result), 12)
  expect_equal(nrow(df_result), 3)

  expect_warning(suppressMessages(df_result <- correlations_per_group(mtcars)),
                 "Number of variables selected for correlation calculation is `0`")

  expect_is(df_result, "data.frame")
  expect_equal(ncol(df_result), 12)
  expect_equal(nrow(df_result), 55)

  expect_message(suppressWarnings(df_result <- correlations_per_group(mtcars)),
                 "No grouping variable selected")

  expect_warning(suppressMessages(df_result <- correlations_per_group(mtcars, wt)),
                 "Number of variables selected for correlation calculation is `1`")

  expect_message(df_result <- correlations_per_group(mtcars, wt, qsec),
                 "No grouping variable selected")

  expect_is(df_result, "data.frame")
  expect_equal(ncol(df_result), 12)
  expect_equal(nrow(df_result), 1)

})

test_that("calculate_stat_models function errors", {

  expect_error(correlations_per_group(mtcars, wt, qsec, wt1, group_var = cyl),
                 ".+Colum\\(s\\) `wt1` are missing from `mtcars`")

  expect_error(correlations_per_group(mtcars, wt, qsec, group_var = cyl1),
               "Column `cyl1` not found in `.data`")

})
