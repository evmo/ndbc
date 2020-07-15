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
    col_types = readr::cols(.default = "d"),
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
  buoy <- toupper(buoy_id)
  print(glue::glue("Reading {buoy} (5-day)"))
  sprintf("%s/5day2/%s_5day.txt", URLBASE, buoy) %>%
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
  buoy <- toupper(buoy_id)
  print(glue::glue("Reading {buoy} (45-day)"))
  sprintf("%s/realtime2/%s.txt", URLBASE, buoy) %>%
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
  print(glue::glue("Reading {toupper(buoy_id)} ({month.name[month]})"))

  sprintf("%s/stdmet/%s/%s%s",
          URLBASE,
          month.name[month],
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
  print(glue::glue("Reading {toupper(buoy_id)} ({year})"))
  ndbc_read(
    sprintf("%s/historical/stdmet/%sh%s.txt.gz",
            URLBASE,
            tolower(buoy_id),
            year)
  ) %>%
    ndbc_fix %>%
    ndbc_munge
}
