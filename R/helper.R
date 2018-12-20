#' Find available historical year data for every NDBC buoy
#'
#' @return a tibble with two columns: "buoy" and "year"
#' @importFrom purrr map
avail_buoy_years <- function() {
  url <- "http://www.ndbc.noaa.gov/data/historical/stdmet/"
  raw <- head(readr::read_lines(url, skip = 9), -4)
  buoy_years <- map(raw, ~ str_extract(.x, "(?<=href=\").{5}h\\d{4}"))
  buoys <- unlist(map(buoy_years, ~ str_extract(.x, "^.{5}")))
  years <- unlist(map(buoy_years, ~ str_extract(.x, "\\d{4}$")))
  tibble::tibble(buoy = buoys, year = as.integer(years))
}

cleanEnv <- function() {
  rm(list=ls(pattern = "b.{5}_\\d{4}", pos=".GlobalEnv"), envir=.GlobalEnv)
}
