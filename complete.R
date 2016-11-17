complete <- function(directory, id = 1:332){
  dir <- paste(getwd(), "/", directory, "/", sep = "")
  
  files <- list.files(dir)
  
  ids <- vector()
  nobs <- vector()
  
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
    
    ids <- c(ids, id[i])
    nobs <- c(nobs, as.numeric(length(sulfateDataNa[s & n])))
  }
  
  data.frame("id" = ids, "nobs" = nobs)
}