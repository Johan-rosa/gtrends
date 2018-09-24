library(shiny)
library(shinymaterial)
library(plotly)
library(gtrendsR) # https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf

server <- function(input, output, session) {
### Get Country Code
  geo <- reactive({
    if (input$geography == "Worldwide") {
      ""
    }

    else{
      countrycode(input$geography, 'country.name', 'iso2c')
    }

  })

  ### Time
  start_date <- reactive({
    if (input$period == "Last five years") {
      "today+5-y"
    }
    else if (input$period == "Past 12 months") {
      "today 12-m"
    }
    else if (input$period == "Past 90 days") {
      "today 3-m"
    }
    else if (input$period == "Past 30 days") {
      "today 1-m"
    }
    else if (input$period == "Last seven days") {
      "now 7-d"
    }
    else if (input$period == "Last  day") {
      "now 1-d"
    }
  })


  out <- reactive({
    if (length(input$vec1) > 0) {
      unlist(strsplit(input$vec1, ","))
    }
  })

  #### Eg : gtrends(keyword = NA, geo = "", time = "today+5-y")
   mk <- reactive({
    if (length(input$vec1 != 0))
      req(input$vec1)
    {
      gtrends(keyword = out(),
              time = start_date(),
              geo = geo())
      }
  })

   #Plot the Trend
  output$gtrends_plot <- renderPlotly({
    plot(mk())
  })

}