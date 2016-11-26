library(shiny)
library(XLConnect)
library(dygraphs)
library(xts)

read.xls <- function(title){
  data <- data.frame(readWorksheet(loadWorkbook(paste("data/", title, ".xls", sep = "")), sheet = "sheet", startRow = 3))
  data[rev(rownames(data)),]
}

readValues <-function(title){
  xls <- read.xls(title)
  x <- xts(xls$Value, as.Date(xls$Date, format='%d.%m.%Y'))
  colnames(x) <- title
  return (x)
}

getData <- function(selected){
  data <- ts()
  
  if(!is.null(selected)){
    for(i in 1:length(selected)){
      if(i == 1){
        data <- readValues(selected[i])
      } else {
        data <- cbind(data, readValues(selected[i]))
      }
    }
  }
  
  return (data)
}

getDataByDate <- function(selected, date){
  tryCatch({
    data <- getData(selected)[date]
    
    if(is.na(data)){
      return ("Choose a currency")
    }
    
    text <- c(paste(format(date, "%d.%m.%Y"), ": ", sep = ""))
    
    for(i in 1:ncol(data)){
      text <- c(text, paste(colnames(data)[i], "=", data[, i], sep = ""))
    }
    
    return (text)
  }, error = function(err){
    return ("We have no values for that date, please, select another date")
  })
}

shinyServer(
  function(input, output) {
    output$dygraph <- renderDygraph({
      dygraph(getData(input$currency)) %>% dyRangeSelector()
    })
    
    output$text <- renderText({
      getDataByDate(input$currency, input$date)
    })
  }
)