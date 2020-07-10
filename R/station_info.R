#' Get NDBC buoy station meta info
#'
#' @return tibble with 7 columns
#' @export
#' @importFrom dplyr mutate select '%>%'
#' @importFrom stringr str_replace str_split_fixed
ndbc_station_info <- function() {
  url <- "http://www.ndbc.noaa.gov/data/stations/station_table.txt"
  readr::read_delim(url, delim = "|", skip = 2, col_names = F,
                    col_types = "cfffcccfcc") %>%
    select(id = X1, owner = X2, type = X3,
           name = X5, loc = X7, tz = X8) %>%
    mutate(
      # parse 'location' column into separate Lat & Long
      loc = str_replace(loc, " \\(.*\\)", ""),
      lat = as.numeric(str_split_fixed(loc, " ", 4)[, 1]),
      lon = as.numeric(str_split_fixed(loc, " ", 4)[, 3]),
      latD = str_split_fixed(loc, " ", 4)[, 2],
      lonD = str_split_fixed(loc, " ", 4)[, 4],
      # negative coordinates for South & West
      latD = ifelse(latD == 'S', -1, 1),
      lonD = ifelse(lonD == 'S', -1, 1),
      lat = lat * latD,
      lon = lon * lonD,
      id = toupper(id),
    ) %>%
    select(-loc, -latD, -lonD)
}
