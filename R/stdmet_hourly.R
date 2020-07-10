
#' Read one hour of std-met data from NDBC 'hourly2' directory
#'
#' @param hour
#'
#' @return
#' @export
#' @importFrom magrittr '%>%'
ndbc_hourly_hour <- function(hour) {
  hour <- sprintf("%02d", hour)
  url <- glue::glue("https://www.ndbc.noaa.gov/data/hourly2/hour_{hour}.txt")
  readr::read_table(
    url,
    col_names = c("id","YY","MM","DD","hh","mm","WDIR","WSPD","GST","WVHT","DPD",
                  "APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","PTDY","TIDE"),
    col_types = "iiiiiiiddddddddddddd",
    skip = 2,
    na = "MM"
  ) %>%
    # de-dup buoys with multiple readings per hour
    dplyr::filter(!duplicated(id))
}

#' Read one hour of std-met data from NDBC 'hourly2' directory (Base R)
#'
#' @param hour
#' @return data.frame
ndbc_hourly_hour_base <- function(hour) {
  url <- sprintf("https://www.ndbc.noaa.gov/data/hourly2/hour_%02d.txt", hour)
  read.table(
    url,
    sep = "",
    col.names = c(
      "id","YY","MM","DD","hh","mm","WDIR","WSPD","GST","WVHT","DPD",
      "APD","MWD","PRES","ATMP","WTMP","DEWP","VIS","PTDY","TIDE"
    ),
    skip = 2,
    na.strings = "MM",
    colClasses = "character",
    comment.char = ""
  )
}

#' Mutate NDBC hourly data
#'
#' @param data
#'
#' @return
#' @importFrom magrittr '%>%'
#' @export
ndbc_hourly_mut <- function(data) {
  data %>%
    dplyr::mutate(
      date = as.POSIXct(
        paste0(YY, "-", MM, "-", DD, " ", hh, ":", mm, ":00")),
      WNDD = toCardinal(WDIR),
      WAVD = toCardinal(MWD)
    )
}

#' Read all hours of std-met data from NDBC 'hourly2' directory
#'
#' @return
#' @export
#' @importFrom magrittr '%>%'
ndbc_hourly_all <- function() {
  hours <- 0:23
  purrr::map_df(hours, ndbc_hourly_hour) %>%
    ndbc_hourly_mut
}

#' Read some hours of std-met data from NDBC 'hourly2' directory
#'
#' @param hours vector of integers (range 0-23)
#'
#' @return
#' @importFrom magrittr '%>%'
#' @export
ndbc_hourly_some <- function(hours) {
  purrr::map_df(hours, ndbc_hourly_hour) %>%
    ndbc_hourly_mut
}
