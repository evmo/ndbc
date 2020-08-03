#' Shiny app to plot snapshot of NDBC marine weather data
#'
#' @return
#' @import shiny
#' @importFrom magrittr '%<>%'
#' @export
#'
#' @examples
snapshotApp <- function() {
  server <- function(input, output) {

    rv <- reactiveValues()

    observeEvent(input$submit, {
      shinyjs::show("tz_adj")
      shinyjs::show("hoursUI")
      shinyjs::hide("daterange")
      date1 <- input$daterange[1]
      date2 <- input$daterange[2]
      cols <- c("date", input$features)

      withProgress(
        message = 'Downloading from NOAA...',
        rv$d <- ndbc_window(input$buoyid, date1, date2)
      )
    })

    output$hoursUI <- renderUI({
      req(rv$d)
      sliderInput(
        "hours",
        label = "Head",
        min = min(rv$d$date),
        max = max(rv$d$date),
        value = c(min(rv$d$date), max(rv$d$date))
      )
    })

    observeEvent(input$hours,
      rv$d %<>% filter(
        date >= input$hours[1],
        date <= input$hours[2]
    ))

    observeEvent(input$tz_adj, {
      req(rv$d)
      rv$d %<>% mutate(
        date = date + lubridate::hours(input$tz_adj)
      )
    })

    output$dt <- renderDataTable({
      req(rv$d)
      rv$d
    }, options = list(pageLength = 10, searching = F, rownames = F))

    output$plotTabs <- renderUI({
      req(rv$d)
      tabsetPanel(
        tabPanel("WTMP", plotOutput("wtmp")),
        tabPanel("ATMP", plotOutput("atmp")),
        tabPanel("WSPD", plotOutput("wspd")),
        tabPanel("WVHT", plotOutput("wvht"))
      )
    })

    output$wtmp <- renderPlot(qplot(date, WTMP, data = rv$d, geom = "line"))
    output$atmp <- renderPlot(qplot(date, ATMP, data = rv$d, geom = "line"))
    output$wspd <- renderPlot(qplot(date, WSPD, data = rv$d, geom = "line"))
    output$wvht <- renderPlot(qplot(date, WVHT, data = rv$d, geom = "line"))
  }

  ui <- fluidPage(shinyjs::useShinyjs(), sidebarLayout(
    sidebarPanel(
      textInput("buoyid", label = "Enter Buoy ID"),
      dateRangeInput("daterange", label = "Choose Date Range"),
      actionButton("submit", "Get Data"),
      shinyjs::hidden(sliderInput(
        "tz_adj", label = "Adjust Timezone",
         min = -12, max = 12, value = 0, step = 1)
      ),
      shinyjs::hidden(uiOutput("hoursUI"))
    ),
    mainPanel(
      uiOutput("plotTabs"),
      dataTableOutput("dt")
    )
  ))

  shinyApp(ui = ui, server = server)
}
