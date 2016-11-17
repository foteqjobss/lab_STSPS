corr <- function(directory, threshold = 0){
  completeData = complete(directory)
  id <- completeData["id"][completeData["nobs"] > threshold]
  
  dir <- paste(getwd(), "/", directory, "/", sep = "")
  files <- list.files(dir)
  
  result <- vector()
  
  if(length(id) == 0){
    return(as.numeric(result))
  }
  
  for(i in 1:length(id)){
    connection <- file(paste(getwd(), "/", directory, "/", files[id[i]], sep = ""), "r")
    initial <- read.table(connection, nrows = 100)
    classes <- sapply(initial, class)
    data <- read.table(connection, sep = ",", colClasses = classes)
    close(connection)
    
    names(data) <- c("Date", "sulfate", "nitrate", "id")
    
    sulfateDataNa <- data["sulfate"]
    nitrateDataNa <- data["nitrate"]
    
    s <- !is.na(sulfateDataNa)
    n <- !is.na(nitrateDataNa)
    
    sulfate <- as.numeric(sulfateDataNa[s & n])
    nitrate <- as.numeric(nitrateDataNa[s & n])
    
    result <- c(result, cor(sulfate, nitrate))
  }
  
  result
}