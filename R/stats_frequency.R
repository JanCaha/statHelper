#' Calculates frequency statistics of data
#'
#' @param data A data frame to process.
#' @param ... Grouping variables to be applied before calculation.
#' @param .select_function Function used to select data columns for which the stats will be calculated. Default value is `is.factor`.
#'
#' @return Data frame containing stats.
#' @export
#'
#' @importFrom rlang quos as_name syms sym
#' @importFrom purrr map as_vector
#' @importFrom dplyr group_by summarize n cur_group_rows left_join mutate select rename bind_rows
#' @importFrom tibble tibble
#'
#' @examples
#' data("diamonds")
#' ds_stats <- stats_frequency(diamonds)
#' ds_stats <- stats_frequency(diamonds, color)
#'
stats_frequency <- function(data, ..., .select_function = is.factor) {

  n_rows <- nrow(data)

  vars_groupped <- rlang::quos(...) %>%
    purrr::map(rlang::as_name) %>%
    purrr::as_vector()

  col_names <- colnames(data)

  selected_variables <- c()

  for (col_name in col_names){
    if (.select_function(data[[col_name]])){
      selected_variables <- c(selected_variables, col_name)
    }
  }

  selected_variables <- setdiff(selected_variables, vars_groupped)

  stat_data <- tibble::tibble()

  for (selected_variable in selected_variables) {

    vars_groupped_local <- rlang::syms(c(vars_groupped, selected_variable))

    if (!is.null(vars_groupped)){
      data_counts <- data %>%
        dplyr::group_by(!!! rlang::syms(c(vars_groupped))) %>%
        dplyr::summarize(total = dplyr::n())
    }

    data_local <- data %>%
      dplyr::group_by(!!! vars_groupped_local)

    suppressMessages({

      data_local <- data_local %>%
        dplyr::summarise(n = length(dplyr::cur_group_rows()))

      if (!is.null(vars_groupped)) {
        data_local <- data_local %>%
          dplyr::left_join(data_counts) %>%
          dplyr::mutate(freq = (n/total) * 100) %>%
          dplyr::select(-total)
      } else {
        data_local <- data_local %>%
          dplyr::mutate(freq = (n/n_rows) * 100)
      }

    })

    data_local <- data_local %>%
      dplyr::mutate(variable = selected_variable, .before = rlang::sym(selected_variable)) %>%
      dplyr::rename(value = rlang::sym(selected_variable)) %>%
      dplyr::mutate(value = as.character(value))


    if (nrow(stat_data) == 0){
      stat_data <- data_local
    } else {
      stat_data <- dplyr::bind_rows(stat_data, data_local)
    }
  }

  stat_data
}

