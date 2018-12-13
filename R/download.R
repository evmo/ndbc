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
  names <- unlist(strsplit(substring(readLines(gzfile(temp),1),1),
                           split="[[:space:]]+"))
  nastrings <- c("99.00","999","999.0","99.0","9999.0","")
  dat <- read.table(gzfile(temp), sep="", col.names=names, skip=2, fill=T,
                    colClasses="character", na.strings=nastrings,
                    comment.char="")
  unlink(temp)
  dat
}

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
#'
#' @examples
ndbc_histY <- function(buoy_id, year) {
  url <- sprintf("%s/historical/stdmet/%s/h%d.txt.gz",
                 URLBASE, buoy_id, year)
  ndbc_read_zip(url)
}
