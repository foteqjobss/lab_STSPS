library(shiny)
library(dygraphs)

shinyUI(pageWithSidebar(
  headerPanel("Exchange rates"),
  
  sidebarPanel(
    checkboxGroupInput("currency", "Currency", c("USD" = "usd", "EUR" = "eur", "GBP" = "gbp")),
    dateInput("date", "Date:", value = "2016-11-01")
  ),
  
  mainPanel(
    dygraphOutput("dygraph"),
    hr(),
    textOutput("text")
  )
))