pollutantmean <- function(directory, pollutant, id = 1:332){
  ## 'pollutant' mean; either "sulfate" or "nitrate".
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  ## Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)
  
  dir <- paste(getwd(), "/", directory, "/", sep = "")
  
  files <- list.files(dir)
  
  result <- 0
  
  for(i in 1:length(id)){
    connection <- file(paste(getwd(), "/", directory, "/", files[id[i]], sep = ""), "r")
    initial <- read.table(connection, nrows = 100)
    classes <- sapply(initial, class)
    data <- read.table(connection, sep = ",", colClasses = classes)
    close(connection)
    
    names(data) <- c("Date", "sulfate", "nitrate", "id")
    
    pollutantDataNa <- data[pollutant]
    pollutantData <- as.numeric(pollutantDataNa[!is.na(pollutantDataNa)])
    
    result <- result + mean(pollutantData)
  }
  
  result
}