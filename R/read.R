URLBASE <- "https://www.ndbc.noaa.gov/data"

#' Read column names from a NDBC std-met data
#'
#' @param url
#'
#' @return vector of names
#' @importFrom magrittr '%>%'
#'
#' @examples
#' ndbc_read_cols('https://www.ndbc.noaa.gov/data/5day2/46053_5day.txt')
ndbc_read_cols <- function(url) {
  readr::read_lines(url, n_max = 1) %>%
    substring(2) %>%  # remove '#' char at beginning of line
    strsplit(split = "[[:space:]]+") %>%  # split by space --> vector
    unlist
}

#' Download and read a plain-text data file from NDBC
#'
#' @param url
#' @return a data frame
#' @examples
#' ndbc_read('https://www.ndbc.noaa.gov/data/5day2/46053_5day.txt')
#' ndbc_read('https://www.ndbc.noaa.gov/data/historical/stdmet/46053h2019.txt.gz')
ndbc_read <- function(url) {
  readr::read_table(
    url,
    col_names = ndbc_read_cols(url),
    col_types = cols(.default = "d"),
    skip = 2,
    na = c("MM", "99.00","999","999.0","99.0","9999.0",""),
    comment = ""
  )
}

#' Munge raw NDBC data frame
#'
#' @param data a data frame
#' @importFrom dplyr mutate select everything '%>%'
#' @return a better data frame
ndbc_munge <- function(data) {
  # remove totally-NA columns
  data[, !apply(data, 2, function(x) all(is.na(x)))] %>%
    # combine date cols
    mutate(
      date = ISOdatetime(YY, MM, DD, hh, mm, 0, tz = "GMT")
    ) %>%
    select(-YY, -MM, -DD, -hh, -mm) %>%
    select(date, everything())
}

#' Get last 5 days' data from a NDBC buoy
#'
#' @param buoy_id
#'
#' @return a data frame
#' @importFrom magrittr '%>%'
#' @export
#'
#' @examples
ndbc_read_5day <- function(buoy_id) {
  sprintf("%s/5day2/%s_5day.txt", URLBASE, toupper(buoy_id)) %>%
    ndbc_read %>%
    ndbc_munge
}

#' Get last 45 days' data from a NDBC buoy
#'
#' @param buoy_id
#'
#' @return a data frame
#' @importFrom magrittr '%>%'
#' @export
#'
#' @examples
ndbc_read_45day <- function(buoy_id) {
  sprintf("%s/realtime2/%s.txt", URLBASE, buoy_id) %>%
    ndbc_read %>%
    ndbc_munge
}

#' Get historical (quality-controlled) data from a NDBC buoy,
#' for a given month during the past year.
#'
#' @param buoy_id
#' @param month (1-12)
#'
#' @return a data frame
#' @importFrom magrittr '%>%'
#' @export
#'
#' @examples
ndbc_read_month_recent <- function(buoy_id, month) {
  months <- c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")

  sprintf("%s/stdmet/%s/%s%s",
          URLBASE,
          months[month],
          tolower(buoy_id),
          paste0(month, format(Sys.Date(), "%Y"), ".txt.gz")) %>%
    ndbc_read %>%
    ndbc_munge
}

#' Fix variations in older historical data
#'
#' @param data
#'
#' @return
#' @export
#' @importFrom magrittr "%<>%"
#' @importFrom dplyr rename mutate
ndbc_fix <- function(data) {
  if ("YYYY" %in% names(data))
    data %<>% rename(YY = YYYY)
  if (any(d$YY < 100))
    data %<>% mutate(YY = YY + 1900)
  if (!("mm" %in% names(data)))
    data$mm <- 0
  if ("WD" %in% names(data))
    data %<>% rename(WDIR = WD)
  if ("BAR" %in% names(data))
    data %<>% rename(PRES = BAR)

  return(data)
}

#' Get historical (quality-controlled) data from a NDBC buoy for a given year.
#'
#' @param buoy_id
#' @param year
#'
#' @return a data frame
#' @export
#' @importFrom dplyr '%>%'
#'
#' @examples
ndbc_read_year <- function(buoy_id, year) {
  ndbc_read(
    sprintf("%s/historical/stdmet/%sh%s.txt.gz",
            URLBASE,
            tolower(buoy_id),
            year)
  ) %>%
    ndbc_fix %>%
    ndbc_munge
}

#' Find available years for a single buoy
#'
#' @param id
#' @return vector of years
avail_years_for_buoy <- function(id) {
  all_buoy_years <- avail_buoy_years()
  if (!(id %in% all_buoy_years$buoy))
    stop(paste0("No historical data available for buoy ", id))
  buoy_years <- subset(all_buoy_years, buoy == id)
  buoy_years$year
}

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
  d <- ndbc_histY(id, years[1])
  cleanEnv()
  if (length(years) > 1) {

    # download subsequent years, merge with first year
    for (y in years[2:length(years)]) {
      cat(paste0(toupper(id), "/", y, ": "))
      ptm <- proc.time()
      nextd <- ndbc_histY(id, y)
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
    d <- ndbc_histY(buoy_id, year(start))
  } else {
    if (month(start) == month(end))
      d <- ndbc_histM(buoy_id, month(start))
    else {
      d1 <- ndbc_histM(buoy_id, start)
      d2 <- ndbc_histM(buoy_id, end)
      d <- bind_rows(d1, d2)
    }
  }
  filter(d, date > start, date < end)
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
