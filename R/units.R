#' Convert compass degrees to cardinal direction
#'
#' @param x
#'
#' @return
#' @export
toCardinal <- function(x) {
  cut(x, breaks = c(0, seq(11.25, 348.75, by = 22.5)),
      labels = c("N","NNE","NE","ENE","E","ESE","SE","SSE","S",
                 "SSW","SW","WSW","W","WNW","NW","NNW"))
}

#' Friendlier column names for NDBC std-met data
#'
#' @param data data-frame
#'
#' @return
#' @export
#'
#' @examples
recodeFeatures <- function(data) {
  names1 <- c("WVHT","WTMP","ATMP","WSPD","GST","DPD",
              "MWD","PRES","APD","WDIR","PTDY","DEWP")
  names2 <- c("WaveHeight","WaterTemp","AirTemp","WindSpeed","WindGust",
              "DominantWavePeriod","MeanWaveDirection","Pressure",
              "AverageWavePeriod","WindDirection","PressureTendency","DewPoint")

  for (n in 1:length(names1)) {
    if (names1[n] %in% names(data))
      names(data) <- gsub(names1[n], names2[n], names(data))
  }

  return(data)
}
