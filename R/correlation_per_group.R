#' Calculate correlation for selected variables in groups
#'
#' @param .data Data frame containing the data.
#' @param ... Variable names (quoted or unquoted) to calculate the correlation for.
#' @param group_var Variable used to divide data into categories. Default value is `NULL`.
#'
#' @return Data frame containing correlations summaries.
#' @export
#'
#' @importFrom rlang quos warn abort is_null as_name quo try_fetch as_label enquo inform
#' @importFrom dplyr select mutate group_by pull filter bind_rows
#' @importFrom tidyr nest
#' @importFrom glue glue
#' @importFrom correlation correlation
#' @importFrom tidyselect eval_select
#'
#' @examples
#' df_corrs <- correlations_per_group(mtcars, wt, qsec, group_var = cyl)
correlations_per_group <- function(.data, ..., group_var = NULL) {
  dots <- rlang::quos(...)

  data_name <- rlang::as_label(rlang::enquo(.data))

  no_group_var <- TRUE

  rlang::try_fetch(
    {
      selection_eval <- tidyselect::eval_select(rlang::enquo(group_var), .data)
      if (0 < length(selection_eval)) {
        no_group_var <- FALSE
      }
    },
    error = function(cnd) {
      variable <- rlang::as_label(rlang::enquo(group_var))
      rlang::abort(c("Cannot use functions correctly.",
        "x" = glue::glue("Column `{variable}` not found in `.data`.")
      ),
      parent = cnd
      )
    }
  )

  .check_variables_exist(.data, ..., .input_data_name = data_name)

  if (2 <= length(dots)) {
    if (no_group_var) {
      data <- .data %>%
        dplyr::select(...)
    } else {
      data <- .data %>%
        dplyr::select({{ group_var }}, ...)
    }
  } else {
    rlang::warn(glue::glue("Number of variables selected for correlation calculation is `{length(dots)}`, need at least 2. All variables will be used."))
    data <- .data
  }

  if (no_group_var) {
    rlang::inform("No grouping variable selected, calculating for all elements together.")

    complete_corr_table <- correlation::correlation(data) %>%
      dplyr::mutate(
        group = NA_character_,
        .before = 1
      )
  } else {
    data <- data %>%
      dplyr::group_by({{ group_var }}) %>%
      tidyr::nest()

    for (value in unique(data %>% dplyr::pull({{ group_var }}))) {
      df_sel <- data %>%
        dplyr::filter({{ group_var }} == value) %>%
        dplyr::pull("data")

      df_sel <- df_sel[[1]]

      cor <- correlation::correlation(df_sel) %>%
        dplyr::mutate(
          group = value,
          .before = 1
        )

      if (exists(rlang::as_name(rlang::quo(complete_corr_table)))) {
        complete_corr_table <- dplyr::bind_rows(complete_corr_table, cor)
      } else {
        complete_corr_table <- cor
      }
    }
  }

  complete_corr_table %>%
    dplyr::as_tibble()
}
