#' @importFrom rlang caller_call call_name is_null quos as_label abort
#' @importFrom glue glue glue_collapse
.check_variables_exist <- function(.data, ..., .input_data_name = NULL) {

  caller <- rlang::caller_call()

  if (!rlang::is_null(caller)) {
    call_function <- rlang::call_name(caller)
  } else {
    call_function <- ".check_variables_exist"
  }

  dots <- rlang::quos(...)

  if (rlang::is_null(.input_data_name)){
    .input_data_name = ".data"
  }

  missing_cols <- setdiff(purrr::map_chr(dots, rlang::as_label), colnames(.data))
  missing_cols <- glue::glue_collapse(missing_cols, sep=", ")

  msg = glue::glue("Cannot use function `{call_function}()` correctly.")
  msg1 = glue::glue("The call `{rlang::as_label(caller)}` is not correct.")
  msg2 = glue::glue( "Colum(s) `{missing_cols}` are missing from `{.input_data_name}`.")

  if (0 < length(missing_cols)) {
    rlang::abort(message = c(msg,
                             ">" = msg1,
                             "x" = msg2))
  }
}
