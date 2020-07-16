#' Get NDBC buoy station meta info
#'
#' @return tibble with 7 columns
#' @export
#' @importFrom dplyr mutate select '%>%'
#' @importFrom stringr str_replace str_split_fixed
ndbc_station_info <- function() {
  url <- "https://www.ndbc.noaa.gov/data/stations/station_table.txt"
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

# calc distance between two buoys based on id
getDistBtw <- function(ids1, ids2, buoyInfo) {
  info1 <- subset(buoyInfo, id == ids1)
  info2 <- subset(buoyInfo, id == ids2)
  haversine(info1$lat, info1$lon, info2$lat, info2$lon)
}

minDistFromOther <- function(station_info) {
  ids <- station_info$id
  locs <- station_info[, c(1, 6, 7)]
  # all possible combinations of buoy IDs
  c <- as.data.frame(t(combn(ids, 2)))
  names(c) <- c("buoy1", "buoy2")
  # look up lat/lon coords of each buoy
  d <- merge(c, locs, by.x = "buoy1", by.y = "id")
  d <- merge(d, locs, by.x = "buoy2", by.y = "id")
  names(d) <- c("buoy1","buoy2","lat1","lon1","lat2","lon2")
  # calculate distance between the two buoys
  d$dist <- haversine(d$lat1, d$lon1, d$lat2, d$lon2) / 1000
  # buoy ID with the various distance btw itself & others
  i <- d[, c(1, 7)]
  # for each buoy, find min distance from other buoy
  minDists1 <- aggregate(dist ~ buoy1, d, min)
  minDists2 <- aggregate(dist ~ buoy2, d, min)
  minDists <- merge(minDists1, minDists2, by.x = "buoy1", by.y = "buoy2")
  minDists$mindist <- pmin(minDists$dist.x, minDists$dist.y)
  minDists <- select(minDists, id = buoy1, dist = mindist)
  return(minDists)
}

#' Find available buoy years
#'
#' For each NDBC buoy, which years are available?
#'
#' @return a tibble with two columns: "buoy" and "year"
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

#' Find available years for a single buoy
#'
#' @param id
#' @export
#' @return vector of years
avail_years_for_buoy <- function(id) {
  all_buoy_years <- avail_buoy_years()
  if (!(id %in% all_buoy_years$buoy))
    stop(paste0("No historical data available for buoy ", id))
  buoy_years <- subset(all_buoy_years, buoy == id)
  buoy_years$year
}
