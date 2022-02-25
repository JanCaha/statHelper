#' Calculate function for set of models
#'
#' @param dataset Dataset to operate on.
#' @param models Models to calculate for.
#' @param fun Function to calculate (i.e. t.test).
#' @param ... Parameters passed to `fun`.
#'
#' @return Data frame with results of function for each model.
#' @export
#'
#' @importFrom rlang is_formula abort
#' @importFrom purrr map_lgl map_df
#' @importFrom dplyr mutate
#' @importFrom broom tidy
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data(diamonds)
#' diamonds <- diamonds %>%
#'   filter(color %in% c("D", "E"))
#' models <- list(
#'   carat ~ color,
#'   depth ~ color,
#'   x ~ color
#' )
#' calculate_stat_models(diamonds, models, t.test)
calculate_stat_models <- function(dataset, models, fun, ...) {
  if (rlang::is_formula(models)) {
    models <- list(models)
  }

  if (!all(purrr::map_lgl(models, rlang::is_formula))) {
    rlang::abort("Every element of `models` must be a formula.")
  }

  name <- as.character(substitute(fun))

  if (length(name) == 3) {
    name <- name[3]
  }

  purrr::map_df(models, .calculate_stat, dataset = dataset, fun = fun, fun_name = name, ...)
}

.calculate_stat <- function(model, dataset, fun, fun_name, ...) {
  fun(model, data = dataset, ...) %>%
    broom::tidy() %>%
    dplyr::mutate(
      model = Reduce(paste, deparse(model)),
      func = fun_name,
      .before = 1
    )
}
