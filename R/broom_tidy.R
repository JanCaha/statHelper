#' @importFrom generics tidy
#' @export
#'
tidy.effsize <- function(x) {
  dplyr::tibble(
    estimate = x$estimate,
    sd = x$sd,
    conf_inf_min = x$conf.int[1],
    conf_inf_max = x$conf.int[2],
    var = x$var,
    conf_level = x$conf.level,
    magnitude = x$magnitude
  )
}
