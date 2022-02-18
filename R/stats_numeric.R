#' Calculates numeric statistics of data
#'
#' @param data A data frame to process.
#' @param ... Grouping variables to be applied before calculation.
#' @param stats_to_calculate List of statistics function to calculate. Default value is `NULL` which means that standard functions (n, mean, sd, median, iqr, min and max) are calculate.
#' @param .select_function Function used to select data columns for which the stats will be calculated. Default value is `is.numeric`.
#'
#' @return Data frame containing stats.
#' @export
#'
#' @importFrom rlang abort is_null is_list is_named quos .data
#' @importFrom purrr map map_lgl as_vector
#' @importFrom dplyr across all_of group_by summarise n
#' @importFrom tidyr pivot_longer separate pivot_wider
#' @import utils
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' data("diamonds")
#' ds_stats <- stats_numeric(diamonds)
#' stats_to_calculate <- list(n = ~ n(), mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))
#' ds_stats <- stats_numeric(diamonds, stats_to_calculate = stats_to_calculate)
stats_numeric <- function(data, ..., stats_to_calculate = NULL, .select_function = is.numeric) {
  if (!"data.frame" %in% class(data)) {
    rlang::abort("Variable `data` must be a `data.frame`.")
  }

  if (rlang::is_null(stats_to_calculate)) {
    stats_to_calculate <- list(
      n = ~ n(),
      mean = ~ mean(.x, na.rm = TRUE),
      sd = ~ sd(.x, na.rm = TRUE),
      median = ~ median(.x, na.rm = TRUE),
      iqr = ~ IQR(.x, na.rm = TRUE),
      min = ~ min(.x, na.rm = TRUE),
      max = ~ max(.x, na.rm = TRUE)
    )
  } else {
    if (!(rlang::is_list(stats_to_calculate) && rlang::is_named(stats_to_calculate))) {
      rlang::abort("Variable `stats_to_calculate` must be a named list.")
    } else if (!all(purrr::map_lgl(stats_to_calculate, rlang::is_formula))) {
      rlang::abort("Every element of `stats_to_calculate` must be a formula.")
    }
  }

  vars_groupped <- rlang::quos(...) %>%
    purrr::map(rlang::as_name) %>%
    purrr::as_vector()

  if (!rlang::is_null(vars_groupped)) {
    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(vars_groupped),
          ~ as.character(.)
        )
      ) %>%
      dplyr::group_by(...)
  }

  data <- data %>%
    dplyr::summarise(
      dplyr::across(where(.select_function),
        stats_to_calculate,
        .names = "{.col}:{.fn}"
      )
    )

  data <- data %>%
    tidyr::pivot_longer(cols = where(is.numeric)) %>%
    tidyr::separate(.data$name,
      c("column", "statistics"),
      sep = ":"
    ) %>%
    tidyr::pivot_wider(
      names_from = "statistics",
      values_from = "value"
    )

  data
}

utils::globalVariables(c("where"))
