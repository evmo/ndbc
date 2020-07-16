#' Get all historical data from a list of multiple NDBC buoys,
#' write each to a local file.
#'
#' @param buoys list of buoy IDs
#' @param dir local directory to write files
#'
#' @return
#' @importFrom magrittr '%>%'
#' @export
ndbc_write_all_hist <- function(buoys, dir = "~/Downloads") {
  purrr::walk(buoys, function(b) {
    ndbc_all_hist(b) %>%
      readr::write_csv(file.path(dir, paste0(toupper(b), ".csv.gz")))
  })
}
