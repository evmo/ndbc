#' Convert NDBC data to Imperial units
#'
#' @param data
#'
#' @return
#' @importFrom measurements conv_unit
#' @export
#'
#' @examples
ndbc_imperial <- function(data) {
  data %>%
    mutate(
      ATMP = conv_unit(ATMP, 'C', 'F'),
      WTMP = conv_unit(WTMP, 'C', 'F'),
      WSPD = conv_unit(WSPD, 'm_per_sec', 'mph'),
      GST = conv_unit(GST, 'm_per_sec', 'mph')
    )
}

#' Plot NDBC feature
#'
#' @param data
#' @param feature
#' @param ylab
#' @param tz_offset
#' @param conv_units
#'
#' @return
#' @import rlang
#' @importFrom ggplot2 qplot
#' @importFrom measurements conv_unit
#' @export
#'
#' @examples
ndbc_plot <- function(data, feature, ylab, tz_offset, conv_units=NULL) {
  feat <- enquo(feature)
  if (!is.null(conv_units)) {
    data <- data %>%
      dplyr::mutate(
        !!feat := conv_unit(!!feat, conv_units[1], conv_units[2])
      )
  }
  data$date <- data$date + tz_offset*3600
  qplot(date, !!feat, data=data, geom='line', xlab="Time", ylab=ylab)
}
