#' Basetime
#'
#' @param day
#'
#' @return
basetime <- function(day) {
  as.POSIXct(paste0(as.character(day), ' 00:00:01'), tz = 'GMT')
}

#' Munge Met Office data
#'
#' @param data
#' @param day
#'
#' @return
#' @importFrom dplyr select transmute '%>%' as_tibble
metoffice_munge <- function(data, day) {
  data %>%
    as_tibble %>%
    select(-V) %>%
    transmute(
      date = basetime(day) + (as.integer(`$`) / 60) * 3600,
      ATMP = as.numeric(T),
      WTMP = as.numeric(St),
      WDIR = as.factor(D),
      WSPD = as.numeric(S),
      WPER = as.integer(Wp),
      WVHT = as.numeric(Wh),
      PRES = as.integer(P)
    )
}

#' Read from UK Met Office API
#'
#' @param station
#' @param key
#'
#' @return
#' @importFrom magrittr '%>%'
#' @export
metoffice_read <- function(station, key) {
  url <- sprintf(
    "http://datapoint.metoffice.gov.uk/public/data/val/wxmarineobs/all/json/%s?res=%s&key=%s",
    station,
    "hourly",
    key
  )

  today <- as.Date(Sys.time())
  yesterday <- today - 1

  d <- jsonlite::fromJSON(url)$SiteRep$DV$Location$Period$Rep

  if (class(d) == "data.frame")
    d %>% metoffice_munge(today)
  else
    bind_rows(
      d[[1]] %>% metoffice_munge(yesterday),
      d[[2]] %>% metoffice_munge(today)
    )
}

#' Get new Met Office data
#'
#' @param station
#' @param key
#' @param existing_data
#' @importFrom dplyr filter '%>%'
#'
#' @return
#' @export
metoffice_new <- function(existing_data, station, key) {
  metoffice_read(station, key) %>%
    filter(!date %in% existing_data$date)
}
