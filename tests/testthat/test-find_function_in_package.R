test_that("find function", {
  package_name <- find_function_in_package(t.test)

  expect_equal(package_name, "stats")

  package_name <- find_function_in_package(stats::t.test)

  expect_equal(package_name, "stats")
})

test_that("find function error", {
  expect_error(find_function_in_package(1), "of uknown type")

  expect_error(find_function_in_package(t.test1), "Parameter `f` is not a function.")
})
