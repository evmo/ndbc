snapshotApp <- function() {
  server <- function(input, output) {

    data <- reactive({
      if (input$submit == 0) { return(NULL) }

      date1 <- input$daterange[1]
      date2 <- input$daterange[2]

      cols <- c("date", input$features)

      withProgress(message = 'Downloading from NOAA...', {
        d <- get_buoy(input$buoyid, scope = "histY", date = year(date1))
      })

      if (year(date1) != year(date2)) {
        y2 <- get_buoy(input$buoyid, scope = "histY", date = year(date2))
        d <- rbind(d, y2)
      }

      d <- subset(d, as.Date(date) >= date1 & as.Date(date) < date2)
      d[c("date", input$features)]
    })

    output$dt <- renderDataTable({
      req(data())
      data()
    }, options = list(pageLength = 10, searching = F, rownames = F))

    output$plotTabs <- renderUI({
      req(data())
      tabsetPanel(
        tabPanel("WTMP", plotOutput("wtmp")),
        tabPanel("ATMP", plotOutput("atmp")),
        tabPanel("WSPD", plotOutput("wspd")),
        tabPanel("WVHT", plotOutput("wvht"))
      )
    })

    output$wtmp <- renderPlot(qplot(date, WTMP, data = data(), geom = "line"))
    output$atmp <- renderPlot(qplot(date, ATMP, data = data(), geom = "line"))
    output$wspd <- renderPlot(qplot(date, WSPD, data = data(), geom = "line"))
    output$wvht <- renderPlot(qplot(date, WVHT, data = data(), geom = "line"))
  }

  ui <- fluidPage(sidebarLayout(
    sidebarPanel(
      textInput("buoyid", label = "Enter Buoy ID"),
      checkboxGroupInput("features", label = "Choose Features", choices =
                           c("WTMP", "ATMP", "WSPD", "WVHT")
      ),
      dateRangeInput("daterange", label = "Choose Date Range"),
      actionButton("submit", "Get Data")
    ),
    mainPanel(
      uiOutput("plotTabs"),
      dataTableOutput("dt")
    )
  ))

  shinyApp(ui = ui, server = server)
}
