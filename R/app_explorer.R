#' NDBC Explorer App
#'
#' @return
#' @export
#' @import shiny leaflet
#' @importFrom dplyr filter
ndbc_explorer <- function(wtmp_data_path, station_info_path) {
  stnInfo <- readRDS(wtmp_data_path)
  stnInfo$type <- as.character(stnInfo$type)
  wtmp <- readRDS(station_info_path)

  server <- function(input, output) {

    output$map <- renderLeaflet({
      leaflet(stnInfo) %>%
        addProviderTiles(
          'Esri.NatGeoWorldMap',
          options = providerTileOptions(attribution = "")
        ) %>%
        addMarkers(~lon, ~lat)
    })

    wtmp_filter <- reactive({
      if (input$wtmpOnly == T)
        filter(stnInfo, id %in% wtmp$id)
      else
        stnInfo
    })

    type_filter <- reactive({
      req(input$buoyType)
      if (input$buoyType == "ALL")
        wtmp_filter()
      else
        filter(wtmp_filter(), type == input$buoyType)
    })

    observeEvent(input$wtmpOnly, {
      leafletProxy("map", data = type_filter()) %>%
        clearMarkers() %>%
        addMarkers(~lon, ~lat)
    })

    observeEvent(input$buoyType, {
      leafletProxy("map", data = type_filter()) %>%
        clearMarkers() %>%
        addMarkers(~lon, ~lat)
    })

    output$chooseBuoyType <- renderUI({
      availTypes <- wtmp_filter()$type
      selectInput("buoyType", "Buoy Type", choices = c("ALL", availTypes))
    })
  }

  ui <- fluidPage(sidebarLayout(
    sidebarPanel(
      checkboxInput("wtmpOnly", "WTMP only"),
      uiOutput("chooseBuoyType")
    ),
    mainPanel(
      leafletOutput("map")
    )
  ))

  shinyApp(ui, server)
}
