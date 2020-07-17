#' Read and bind multiple monthly datasets
#'
#' @param start_month
#' @param end_month
#' @param buoy_id
#'
#' @return
insist_read_months <- function(...) "dummy"

ndbc_read_months <- function(buoy_id, start_month, end_month) {
  purrr::map_df(seq(start_month, end_month),
                ~ ndbc_read_month_recent(buoy_id, .x))
}

.onLoad <- function(lib, pkg) {
  insist_read_months <<- purrr::insistently(ndbc_read_months)
}

#' Read and bind multiple yearly datasets
#'
#' @param buoy_id
#' @param years list of years
#'
#' @return
insist_read_years <- function(...) "dummy"

ndbc_read_years <- function(buoy_id, years) {
  purrr::map_df(years, ~ ndbc_read_year(buoy_id, .x))
}

.onLoad <- function(lib, pkg) {
  insist_read_years <<- purrr::insistently(ndbc_read_years)
}

#' Remove duplicate rows
#'
#' @param data
#'
#' @importFrom dplyr arrange distinct '%>%'
#'
#' @return
ndbc_dedup <- function(data) {
  data %>%
    arrange(date) %>%
    distinct(date, .keep_all = TRUE)
}

#' Read std met data from arbitrary window of dates in current year
#'
#' @param buoy_id
#' @param start_date
#' @param end_date
#' @importFrom lubridate year month
#'
#' @return
ndbc_window_current_year <- function(buoy_id, start_date, end_date) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  today <- Sys.Date()

  if (year(start) < year(today))
    stop("Start and end dates must be in current year")
  # window is within past 5 days
  else if (start > today - 5)
    d <- ndbc_read_5day(buoy_id)
  # window is within past 45 days
  else if (start > today - 45)
    d <- ndbc_read_45day(buoy_id)
  # window is within current year
  else {
    if (end < today - 45)
      d <- ndbc_read_months(buoy_id, month(start), month(end))
    else
      d <- ndbc_dedup(dplyr::bind_rows(
        ndbc_read_months(buoy_id, month(start), month(end - 45)),
        ndbc_read_45day(buoy_id)
      ))
  }

  dplyr::filter(d, date > start, date <= end)
}

#' Read std met data from arbitrary window of dates
#'
#' @param buoy_id
#' @param start_date
#' @param end_date
#' @importFrom lubridate year month
#'
#' @return
#' @export
#'
#' @examples
ndbc_window <- function(buoy_id, start_date, end_date) {
  start <- as.Date(start_date)
  end <- as.Date(end_date)
  today <- Sys.Date()

  if (year(end) < year(today))  # window doesn't include current year
    d <- ndbc_read_years(buoy_id, year(start), year(end))
  else {  # window includes both prior year(s) and current year
    d <- dplyr::bind_rows(
      ndbc_read_years(buoy_id, seq(year(start), year(end) - 1)),
      ndbc_window_current_year(
        buoy_id,
        as.Date(ISOdate(year(today), 1, 1)),
        end
      )
    )
  }

  dplyr::filter(d, date > start, date <= end)
}

#' Download, merge, & munge all historical data-years for a NDBC buoy
#'
#' @param buoy_id
#'
#' @return
#' @export
ndbc_all_hist <- function(buoy_id) {
  purrr::map_df(avail_years_for_buoy(buoy_id),
                ~ ndbc_read_years(buoy_id, .x))
}

# Efficient update of local 45-day data
# Download 5-day data, reduce to 1-day, append to 45-day file
# Intended for daily crontab
# Modified: 20150721

refresh45 <- function(buoylist, path = "./") {
  yesterday <- Sys.Date() - 1
  cutoff <- Sys.Date() - 45
  for (b in buoylist) {
    fn <- paste0(path, "b", b, "_", "45d.csv")
    # read in local 45-day data, remove oldest
    locData <- read.csv(fn, stringsAsFactors=F) %>%
      subset(as.Date(date) > cutoff)
    # download latest 5-day data, reduce to 1-day
    newData <- getNDBC(b, scope = "5d") %>%
      subset(as.Date(date) == yesterday)
    write.table(newData, fn, append = T, row.names = F, col.names = F, sep = ",")
  }
}
