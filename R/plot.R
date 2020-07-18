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

# Plot aggregate mean, min, & max for a feature, for a buoy.
# Modified: 20150519

plotAgg <- function(buoy_id, feature, agg_mean, agg_min, agg_max) {
  library(ggplot2); library(scales)

  d <- merge(agg_mean, merge(agg_min, agg_max, by = "doy"), by = "doy")
  d <- data.frame(d[, 1], apply(d[, c(2:4)], 2, function(x) 9/5 * x + 32))
  names(d) <- c("doy", "avg", "min", "max")
  d$doy <- as.Date(d$doy, "%m-%d")
  p <- ggplot(d, aes(x = doy)) +
    geom_line(aes(y=avg, colour="avg"), size=1, colour="black") +
    geom_line(aes(y=min, colour="min"), size=1, colour="blue", linetype="dashed") +
    geom_line(aes(y=max, colour="max"), size=1, colour="red", linetype="dashed") +
    scale_y_continuous(name="degrees Fahrenheit") +
    xlab("") +
    scale_x_date(labels = date_format("%b")) +
    annotate("text", x=d[30,1], y=max(d$max),
             label="max", col="red", hjust=1, size=5) +
    annotate("text", x=d[30,1], y=max(d$max)-1,
             label="avg", col="black", hjust=1, size=5) +
    annotate("text", x=d[30,1], y=max(d$max)-2,
             label="min", col="blue", hjust=1, size=5)
    #+ annotate("text", x=d[240,1], y=min(d$min), label="marathonswimmers.org",
     #        col="orange", hjust=0, fontface="bold", size=3)
  return(p)
}
