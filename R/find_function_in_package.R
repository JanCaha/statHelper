#' Find function in package
#'
#' @param f function or character with name of the function
#'
#' @return character name of the package that contains the function
#' @export
#'
#' @examples
#' package_name <- find_function_in_package(t.test)
#' package_name <- find_function_in_package("t.test")
#' @importFrom rlang englue is_function is_character abort
#' @importFrom stringr str_detect str_split
#' @importFrom glue glue
#' @importFrom utils getAnywhere
#'
find_function_in_package <- function(f) {
  name <- rlang::englue("{{ f }}")

  rlang::try_fetch(rlang::is_function(f),
    error = function(cnd) {
      rlang::abort(glue::glue("Parameter `f` is not a function."), parend = cnd)
    }
  )

  if (rlang::is_function(f)) {
    if (stringr::str_detect(name, "::")) {
      name <- stringr::str_split(name, "::", simplify = TRUE)[2]
    }

    f <- name
  }

  if (rlang::is_character(f)) {
    func <- utils::getAnywhere(f)
  } else {
    rlang::abort(glue::glue("Parameter `f` of uknown type `{class(f)}`."))
  }

  if (length(func$where) == 0) {
    func_name <- rlang::caller_arg(f)

    stop(glue::glue("Function `{func_name}` not found in currently isntalled packages."))
  } else {
    return(stringr::str_split(func$where[1], ":", simplify = TRUE)[2])
  }
}
