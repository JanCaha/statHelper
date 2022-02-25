test_that(".check_variables_exist function", {
  f <- function(data, ...) {
    .check_variables_exist(data, ...)
  }

  expect_invisible(f(mtcars, mpg, cyl))

  expect_error(
    f(mtcars, mpg, cyl1),
    "Cannot use function `f\\(\\)` correctly"
  )
  expect_error(
    f(mtcars, mpg, cyl1),
    "The call `f\\(mtcars, mpg, cyl1\\)` is not correct"
  )
  expect_error(
    f(mtcars, mpg, cyl1),
    "Colum\\(s\\) `cyl1` are missing from `.data`"
  )

  expect_error(
    f(mtcars, mpg, cyl1, mpg2),
    "Cannot use function `f\\(\\)` correctly"
  )
  expect_error(
    f(mtcars, mpg, cyl1, mpg2),
    "The call `f\\(mtcars, mpg, cyl1, mpg2\\)` is not correct"
  )
  expect_error(
    f(mtcars, mpg, cyl1, mpg2),
    "Colum\\(s\\) `cyl1, mpg2` are missing from `.data`"
  )
})
