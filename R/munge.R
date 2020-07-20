#' Daily means for one feature
#'
#' @param data
#' @param feature
#'
#' @return
#' @importFrom rlang '!!'
#' @importFrom dplyr mutate summarise group_by '%>%'
#' @export
#'
#' @examples
ndbc_reduce_daily <- function(data, feature) {
  feat_name <- rlang::quo_name(feature)
  feat_enq <- rlang::enquo(feature)
  data %>%
    mutate(date = lubridate::floor_date(date, unit = "day")) %>%
    group_by(date) %>%
    summarise(!!feat_name := mean(!!feat_enq))
}

ndbc_agg_by_doy <- function(daily_data, FUN = c(mean, min, max)) {
  daily_data %>%
    mutate(doy = as.factor(strftime(date, "%m-%d"))) %>%
    group_by(doy) %>%
    summarise(across(where(is.numeric), ~FUN(.x, na.rm = T), .names = "{fn}"))
}

# Find average, min, or max of a feature by day of year.
# Input: buoy, feature, statistic (avg, min, or max)
# Output: aggregate data.frame, 366 rows for each day of year
# Modified 20150513

aggDOY <- function(daily_data, stat) {
  names(daily_data) <- c("date", "feat")
  agg <- daily_data %>%
    mutate(doy = as.factor(strftime(date, "%m-%d")))

  if (stat == "mean") {
    agg <- group_by(agg, doy) %>%
      summarise(feat = mean(feat))
  } else if (stat == "min") {
    agg <- group_by(agg, doy) %>%
      summarise(feat = min(feat))
  } else if (stat == "max") {
    agg <- group_by(agg, doy) %>%
      summarise(feat = max(feat))
  }

  names(agg)[2] <- "feature"

  return(agg)
}
