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

#' Get window of NDBC std met data
#'
#' @param buoy_id
#' @param start
#' @param end
#'
#' @return
#' @importFrom lubridate year month
#' @export
#'
#' @examples
ndbc_window <- function(buoy_id, start, end) {
  start <- as.POSIXct(start, tz = 'UTC')
  end <- as.POSIXct(end, tz = "UTC")
  if (year(start) < year(Sys.Date())) {
    d <- ndbc_read_year(buoy_id, year(start))
  } else {
    if (month(start) == month(end))
      d <- ndbc_read_month_recent(buoy_id, month(start))
    else {
      d1 <- ndbc_read_month_recent(buoy_id, start)
      d2 <- ndbc_read_month_recent(buoy_id, end)
      d <- dplyr::bind_rows(d1, d2)
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
