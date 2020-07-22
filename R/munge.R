#' Daily means for one feature
#'
#' @param data
#' @param feature
#'
#' @return
#' @importFrom dplyr mutate summarise group_by '%>%'
#' @export
#'
#' @examples
ndbc_reduce_daily <- function(data, feature) {
  data %>%
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    group_by(date) %>%
    summarise({{feature}} := mean({{feature}}, na.rm = TRUE))
}

#' Aggregate daily data by day of year
#'
#' @param daily_data
#' @param fn
#'
#' @return
#' @export
#'
#' @examples
ndbc_agg_by_doy <- function(daily_data, fn = c(mean, min, max)) {
  daily_data %>%
    mutate(doy = as.factor(format(date, "%m-%d"))) %>%
    group_by(doy) %>%
    summarise(across(2, .fns = {{fn}}, na.rm = T, .names = "{col}"))
}
