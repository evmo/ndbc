URLBASE <- "https://www.ndbc.noaa.gov/data"

#' Munge raw NDBC data frame
#'
#' @param data a data frame
#' @return a better data frame
ndbc_munge <- function(data) {
  # remove totally-NA columns
  dat <- data[, !apply(data, 2, function(x) all(is.na(x)))]
  colnames(dat)[1] <- "YY"
  if (nchar(dat$YY) == 2)
    dat$YY <- sapply(dat$YY, function(x) paste0("19", x))
  if (!('mm' %in% names(dat))) {
    dat$mm <- '00'
    dat <- dat[, c(1:4, ncol(dat), 5:(ncol(dat) - 1))]
  }
    # merge date/time columns
  dat$date <- strptime(paste(dat$YY,dat$MM,dat$DD,dat$hh,dat$mm, sep="-"),
    format="%Y-%m-%d-%H-%M")
  dat <- dat[, -c(1:5)]
    # convert columns to numeric
  for(col in c(1, 2:(ncol(dat) - 1)))
    dat[, col] <- as.numeric(dat[, col])
    # convert date class
  dat$date <- as.POSIXct(dat$date)
    # put date in first column
  dat <- dat[, c(ncol(dat), 1:(ncol(dat) - 1))]
    # recode columns that have alternate names
  if ('BAR' %in% names(dat))
    colnames(dat)[colnames(dat)=="BAR"] <- "PRES"
  if ('WD' %in% names(dat))
    colnames(dat)[colnames(dat)=="WD"] <- "WDIR"

  return(dat)
}

#' Download and read a plain-text data file from NDBC
#'
#' @param url
#' @return a data frame
ndbc_read_txt <- function(url) {
  # get column names from top of file
  names <- unlist(strsplit(substring(readr::read_lines(url, n_max = 1), 2),
                  split="[[:space:]]+"))
  NAs <- "MM"
  read.table(url, sep="", col.names=names, skip=2, na.strings=NAs,
             colClasses="character", comment.char="", fill=T)
}

#' Download and read a gzipped historical data file from NDBC
#'
#' @param url
#' @return a data frame
ndbc_read_zip <- function(url) {
  temp <- tempfile()
  download.file(url, temp, method="curl", quiet=T)

  # get column names from top of file
  names <-
    unlist(strsplit(substring(gsub(
      "#", "", readLines(gzfile(temp), 1)
    ), 1),
    split = "[[:space:]]+"))
  nastrings <- c("99.00","999","999.0","99.0","9999.0","")

  dat <- readr::read_table(gzfile(temp),
    col_names = names, col_types = cols(.default = "d"),
    skip = 2, na = nastrings)
  unlink(temp)
  dat
}

## Base R read.table version
# dat <- read.table(gzfile(temp), sep="", col.names=names, skip=2, fill=T,
#                   colClasses="character", na.strings=nastrings,
#                   comment.char="")

#' Get last 5 days' data from a NDBC buoy
#'
#' @param buoy_id
#'
#' @return a data frame
#' @export
#'
#' @examples
ndbc_5d <- function(buoy_id) {
  url <- sprintf("%s/5day2/%s_5day.txt", URLBASE, buoy_id)
  ndbc_read_txt(url)
}

#' Get last 45 days' data from a NDBC buoy
#'
#' @param buoy_id
#'
#' @return a data frame
#' @export
#'
#' @examples
ndbc_45d <- function(buoy_id) {
  url <- sprintf("%s/realtime2/%s.txt", URLBASE, buoy_id)
  ndbc_read_txt(url)
}

#' Get historical (quality-controlled) data from a NDBC buoy,
#' for a given month during the past year.
#'
#' @param buoy_id
#' @param month
#'
#' @return a data frame
#' @export
#'
#' @examples
ndbc_histM <- function(buoy_id, month) {
  months <- c("Jan","Feb","Mar","Apr","May","Jun",
              "Jul","Aug","Sep","Oct","Nov","Dec")
  monthNum <- match(month, months)
  urltail <- paste0(monthNum, format(Sys.Date(), "%Y"), ".txt.gz")
  url <- sprintf("%s/stdmet/%s/%s%s",
                 URLBASE, month, tolower(buoy_id), urltail)
  ndbc_read_zip(url)
}

#' Get historical (quality-controlled) data from a NDBC buoy for a given year.
#'
#' @param buoy_id
#' @param year
#'
#' @return a data frame
#' @export
#' @importFrom magrittr "%<>%"
#'
#' @examples
ndbc_histY <- function(buoy_id, year) {
  url <- sprintf("%s/historical/stdmet/%sh%s.txt.gz",
                 URLBASE, buoy_id, year)
  d <- ndbc_read_zip(url)
  if ("YYYY" %in% names(d))
    d %<>% rename(YY = YYYY)
  if (any(d$YY < 100))
    d %<>% mutate(YY = YY + 1900)
  if (!("mm" %in% names(d)))
    d$mm <- 0
  if ("WD" %in% names(d))
    d %<>% rename(WDIR = WD)
  if ("BAR" %in% names(d))
    d %<>% rename(PRES = BAR)
  d %<>% mutate(
    date = ISOdatetime(YY, MM, DD, hh, mm, 0, tz = "GMT")
  ) %>%
    select(-YY, -MM, -DD, -hh, -mm) %>%
    select(date, everything())
  return(d)
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
