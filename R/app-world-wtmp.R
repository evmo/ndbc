#' World Sea Temp shiny app
#'
#' @param wtmp_data_file
#' @param zoom_locs_file
#'
#' @return
#' @import shiny
#' @importFrom leaflet leaflet leafletProxy addMarkers clearMarkers setView
#'             addProviderTiles providerTileOptions labelOptions markerOptions
#' @importFrom dplyr mutate if_else filter
#' @export
#'
#' @examples
app_world_wtmp <- function(wtmp_data_file, zoom_locs_file) {

  d <- readRDS(wtmp_data_file) %>%
    mutate(
      WTMP_C = WTMP,
      WTMP_F = WTMP_C * (9 / 5) + 32
    )

  zoomLocs <- read.csv(
    zoom_locs_file,
    colClasses = c("character", "numeric", "numeric", "integer")
  )

  server <- function(input, output, session) {

    rv <- reactiveValues(
      data = filter(d, !duplicated(zoom1))
    )

    output$lastUpdated <- renderText({
      theDate <- format(max(d$date, na.rm = T), "%d %b %H:%M")
      paste("Updated", theDate)
    })

    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(
          'Esri.NatGeoWorldMap',
          options = providerTileOptions(attribution = "")
        )
    })

    observeEvent(input$map_zoom, {
      rv$data <- filter(d, !duplicated(.data[[paste0("zoom", input$map_zoom)]]))
      leafletProxy("map", data = rv$data) %>%
        clearMarkers() %>%
        addMarkers(~lon, ~lat, label = ~WTMP,
          labelOptions = labelOptions(noHide = T),
          options = markerOptions(opacity = 0)
        )
    })

    observeEvent(input$units, {
      if (input$units == "F")
        rv$data$WTMP <- rv$data$WTMP_F
      else
        rv$data$WTMP <- rv$data$WTMP_C

      leafletProxy("map", data = rv$data) %>%
        clearMarkers() %>%
        addMarkers(~lon, ~lat,
          label = ~WTMP,
          labelOptions = labelOptions(noHide = T),
          options = markerOptions(opacity = 0)
        )
    })

    observeEvent(input$zoomTo, {
      findLoc <- function(loc) {
        zoomLocs[zoomLocs$name == loc,]
      }
      choice <- switch(
        input$zoomTo,
        "socal" = findLoc("socal"),
        "nyc" = findLoc("nyc"),
        "norcal" = findLoc("norcal"),
        "usa" = findLoc("usa"),
        "pacnw" = findLoc("pacnw"),
        "eur" = findLoc("eur"),
        "midatl" = findLoc("midatl"),
        "neweng" = findLoc("neweng"),
        "flor" = findLoc("flor"),
        "hawaii" = findLoc("hawaii")
      )

      leafletProxy("map") %>%
        setView(choice$lon, choice$lat, zoom = choice$zoom)
    })
  }

  ui <- bootstrapPage(
    # includeScript("ganalytics.js"),
    title = "World Sea Temperature Map",
    tags$meta(property = "og:site_name", content = "track.rs"),
    tags$meta(property = "og:description",
              content = "Interactive map of real-time sea / water temperatures
                       provided by NOAA National Data Buoy Center"),
    tags$style(type = "text/css", "html, body {width: 100%; height: 100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(top = 5, right = 5,
      selectInput("zoomTo", label = NULL,
        width = "150px", selected = "usa",
        choices = c(
          "Continental U.S." = "usa",
          "Europe" = "eur",
          "So. Calif" = "socal",
          "SF Bay Area" = "norcal",
          "Pacific NW" = "pacnw",
          "NY Metro" = "nyc",
          "Mid-Atlantic" = "midatl",
          "New England" = "neweng",
          "Florida" = "flor",
          "Hawaii" = "hawaii"
        )
      )
    ),
    absolutePanel(top = 45, right = 30,
      div(
        style = "background-color: white; padding: 2px 5px 0 5px;",
        radioButtons("units", label = NULL,
          selected = "C", inline = T,
          choices = c("°C" = "C", "°F" = "F")
        )
      )
    ),
    absolutePanel(bottom = 2, left = 10,
      div(style = "color: red;", textOutput('lastUpdated'))
    )
  )

  shinyApp(ui = ui, server = server)
}
