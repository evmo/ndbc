#' Download, merge, & munge all historical data-years for a NDBC buoy
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
getAllHist <- function(id) {
  years <- avail_years_for_buoy(id)

  # download the first year
  d <- ndbc_read_year(id, years[1])
  cleanEnv()
  if (length(years) > 1) {

    # download subsequent years, merge with first year
    for (y in years[2:length(years)]) {
      cat(paste0(toupper(id), "/", y, ": "))
      ptm <- proc.time()
      nextd <- ndbc_read_year(id, y)
      elapsed <- proc.time() - ptm
      cat(paste0("downloaded in ", round(elapsed[[3]], 2), " seconds, "))
      ptm <- proc.time()
      d <- suppressMessages(full_join(d, nextd))
      elapsed <- proc.time() - ptm
      cat(paste0("processed in ", round(elapsed[[3]], 2), " seconds.\n"))
      cleanEnv()
      Sys.sleep(1)
    }
  }

  # zip the merged DF and write to directory
  outpath <- file.path("~/Downloads", paste0(toupper(id), ".csv.gz"))
  z <- gzfile(outpath)
  write.csv(d, z, row.names=F)
  print(paste0(outpath, " written!"))
}

#' Read and bind multiple monthly datasets
#'
#' @param start_month
#' @param end_month
#' @param buoy_id
#'
#' @return
ndbc_read_mult_month <- function(buoy_id, start_month, end_month) {
  purrr::map_df(seq(start_month, end_month),
                ~ ndbc_read_month_recent(buoy_id, .x))
}

#' Read and bind multiple yearly datasets
#'
#' @param start_year
#' @param end_year
#' @param buoy_id
#'
#' @return
ndbc_read_mult_year <- function(buoy_id, start_year, end_year) {
  purrr::map_df(seq(start_year, end_year),
                ~ ndbc_read_year(buoy_id, .x))
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

  # window is within past 5 days
  if (start > today - 5)
    d <- ndbc_read_5day(buoy_id)
  # window is within past 45 days
  else if (start > today - 45)
    d <- ndbc_read_45day(buoy_id)
  # window is within current year
  else if (start == year(today))
    d <- ndbc_read_mult_month(buoy_id, month(start), month(end))
  else {  # window includes prior years (need historical data)
    if (year(end) < year(today))  # window doesn't include current year
      d <- ndbc_read_mult_year(buoy_id, year(start), year(end))
    else {  # window includes both prior year(s) and current year
      d <- dplyr::bind_rows(
        ndbc_read_mult_year(buoy_id, year(start), year(end) - 1),
        ndbc_read_mult_month(buoy_id, 1, month(end))
      )
    }
  }

  dplyr::filter(d, date > start, date < end)
}

# Download short-term data for multiple NDBC buoys.
# Modified: 20150721
# Parameters:
#	* buoylist - vector of 5-letter buoy codes
#   * scope - "5d" ('5day2' directory) or "45d" ('realtime2' directory)
#   * path - directory to write file
# Writes CSV file

getNDBCmult <- function(buoylist, scope = "5d", path = "./") {
  for (b in buoylist) {
    print(paste0("downloading station ", b))
    fn <- paste0(path, "b", b, "_", scope, ".csv")
    data <- try(getNDBC(b, scope = scope), silent = T)
    if (class(data)=='try-error')
      next
    write.csv(data, fn, row.names=F)
  }
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

# Download all historical data-years for a NDBC buoy,
# unzip, merge, munge, and re-zip into single file.
# Modified: 20150402

getAllHist <- function(id) {
  # remove temp DFs created by each call to getNDBC()
  cleanEnv <- function() {
    rm(list=ls(pattern = "b.{5}_\\d{4}", pos=".GlobalEnv"), envir=.GlobalEnv)
  }

  # get available years for the buoy
  BY <- read.csv("buoyYears.csv")
  if (!(id %in% BY$buoy))
    stop(paste0("No historical data available for buoy ", id))
  BY <- subset(BY, buoy==id)
  years <- as.character(BY$year)

  # download the first year
  d <- getNDBC(id, scope = "histY", date = years[1])
  cleanEnv()
  if (length(years) > 1) {

    # download subsequent years, merge with first year
    for (y in years[2:length(years)]) {
      cat(paste0(toupper(id), "/", y, ": "))
      ptm <- proc.time()
      nextd <- getNDBC(id, scope = "histY", date = y)
      elapsed <- proc.time() - ptm
      cat(paste0("downloaded in ", round(elapsed[[3]], 2), " seconds, "))
      ptm <- proc.time()
      d <- merge(d, nextd, all = T)
      elapsed <- proc.time() - ptm
      cat(paste0("processed in ", round(elapsed[[3]], 2), " seconds.\n"))
      cleanEnv()
      Sys.sleep(1)
    }
  }

  # zip the merged DF and write to directory
  outpath <- paste0("hist/", toupper(id), ".csv.gz")
  z <- gzfile(outpath)
  write.csv(d, z, row.names=F)
  print(paste0(outpath, " written!"))
}

# Get all historical data from a list of multiple NDBC buoys
# Modified: 20150402

getAllHistMult <- function(buoylist) {
  for (b in buoylist)
    try(getAllHist(b))
}
