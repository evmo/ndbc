deduce_col_types <- function(buoy_data) {
  col_lines <- read_lines(gzfile(buoy_data), n_max = 1)
  num_cols <- str_count(col_lines, ",") + 1
  paste0("T", paste(rep("d", num_cols - 1), collapse=""))
}

#' Reduce complete buoy data to one observation per day
#'
#' @param buoy_data complete data file for a buoy (.csv.gz)
#' @param feature string (WTMP, ATMP, WSPD)
#'
#' @return data.frame - Daily means for one feature
#' @export
#' @import xts dplyr
#'
#' @examples
#' reduceToDaily("FTPC1", "WTMP")
reduceToDaily <- function(buoy_data, feature) {
  feat <- toupper(feature)
  d <- readr::read_csv(gzfile(buoy_data),
                       col_types = deduce_col_types(buoy_data)) %>%
    select(date, feat) %>%
    rename(feat = feat) %>%
    filter(!is.na(feat)) %>%
    mutate(date = as.POSIXct(as.character(date))) %>%
    filter(strftime(date, "%m-%d") != "02-29")    # remove Feb 29ths
  ## remove outliers
  # outliers = boxplot(d$feat, plot=FALSE)$out
  # d <- filter(d, !(feat %in% outliers))
  # get daily averages
  dx <- apply.daily(xts(d$feat, d$date), mean, na.rm=T)
  colnames(dx) <- "feat"
  # convert xts back to df
  dd <- data.frame(date = as.Date(index(dx)), coredata(dx))
  names(dd)[2] <- feat
  return(dd)
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
