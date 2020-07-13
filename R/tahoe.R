URL <- "https://laketahoe.jpl.nasa.gov/get_met_weather"

#' Take the mean from a min-mean-max string, Lake Tahoe JPL data
#'
#' e.g., "15:15.2:15.6" --> 15.2
#'
#' @param minavgmax
#'
#' @return
getAvg <- function(minavgmax) {
  strsplit(minavgmax, ":")[[1]][2]
}

#' Read data table from JPL Lake Tahoe weather page
#'
#' @return
#' @importFrom rvest html_node html_table '%>%'
#' @export
tahoe_read_table <- function() {
  raw <- xml2::read_html(URL) %>%
    html_node("div#content table") %>%
    html_table(fill = T)

  # transpose the table, set the column names
  transposed <- setNames(data.frame(t(raw[-1, -c(1:2)]),
                                    stringsAsFactors = F),
                         raw[-1, 1])

  # set the row names as the buoy labels
  transposed$buoy <- row.names(transposed)
  row.names(transposed) <- NULL

  return(transposed)
}

#' Munge Lake Tahoe data
#'
#' @param data
#'
#' @return
#' @importFrom dplyr select filter mutate '%>%'
#' @export
tahoe_munge <- function(data) {
  data %>%
    select(
      buoy,
      upd_date = `Last Update Date (UTC)`,
      upd_time = `Last Update Time (UTC)`,
      state = `Data Download State`,
      wspd = `Wind Speed`,
      wdir = `Wind Direction`,
      atmp = `Air Temperature`,
      pres = `Pressure`,
      wtmp = `Water temp.: 0.5m`
  ) %>%
    filter(buoy %in% buoys) %>%
    mutate(
      time = paste(upd_date, upd_time),
      wspd = sapply(wspd, getAvg),
      wdir = sapply(wdir, getAvg),
      atmp = sapply(atmp, getAvg),
      pres = sapply(pres, getAvg),
      wtmp = sapply(wtmp, getAvg)
    ) %>%
    select(-upd_date,-upd_time) %>%
    filter(state == "FRESH")
}
