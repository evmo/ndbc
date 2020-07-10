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
