#' Find available buoy years
#'
#' For each NDBC buoy, which years are available?
#'
#' @return a tibble with two columns: "buoy" and "year"
#' @export
#' @importFrom purrr map
#' @importFrom stringr str_extract '%>%'
avail_buoy_years <- function() {
  url <- "https://www.ndbc.noaa.gov/data/historical/stdmet/"
  buoy_years <- readr::read_lines(url) %>%  # large char vector
    magrittr::extract(grepl("\\.txt\\.gz", .)) %>%  # lines with txt.gz files
    map(~ str_extract(.x, "(?<=href=\").{5}h\\d{4}"))
  buoys <- unlist(map(buoy_years, ~ str_extract(.x, "^.{5}")))
  years <- unlist(map(buoy_years, ~ str_extract(.x, "\\d{4}$")))
  tibble::tibble(buoy = buoys, year = as.integer(years))
}
