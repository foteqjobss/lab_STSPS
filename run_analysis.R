analysis <- function(dataDir = "data"){
  combinedData <- combine(dataDir)

  activities <- setNamesOfActivities(combinedData)
  
  variables <- setNamesOfVariables(activities)
  
  meanStd <- getMeanStd(variables)
  
  average <- getAverage(meanStd)
}

#функція для об'єднання даних про тестування та тренування у один набір даних
combine <- function(dataDir = "data"){
  test_y <- getY("test", dataDir)
  train_y <- getY("train", dataDir)
  
  test_x = getX("test", dataDir)
  train_x = getX("train", dataDir)
  
  y <- c(test_y, train_y)
  x <- rbind(test_x, train_x)
  
  data.frame(y, x)
}

#функція для задання назв активностей для назв діальності у наборі даних
setNamesOfActivities <- function(combinedData = combine()){
  activities <- getActivities()
  
  result <- combinedData
  
  result[, 1] <- as.character(result[, 1])
  
  for(i in 1:nrow(result)){
    result[i, 1] <- activities[as.numeric(result[i, 1])]
  }
  
  result
}

#функція для задання назв даних, які описують суть змінних
setNamesOfVariables <- function(activities = setNamesOfActivities(), dataDir = "data"){
  colnames(activities) <- c("activity", getVariables(dataDir))
  
  return (activities)
}

#функція для витягування математичного очікування і дисперсії
getMeanStd <- function(variables = setNamesOfVariables()){
  result <- data.frame(variables[, 1])
  
  cnames <- colnames(variables)
  
  names <- c(cnames[1])
  
  for(i in 2:length(cnames)){
    mean <- grep("mean", cnames[i])
    std <- grep("std", cnames[i])
   
    if(length(mean) > 0 || length(std) > 0){
      result <- data.frame(result, variables[, i])
      
      names <- c(names, cnames[i])
    }
  }
  
  colnames(result) <- names
  
  return (result)
}

#функція для створення набору даних із середнім значенням по кожній змінній за кожним видом діяльностіі по кожному предметі
getAverage <- function(data = getMeanStd()){
  activities <- unique(data[, 1])
  
  total <- matrix(nrow = length(activities), ncol = ncol(data) - 1)
  n <- matrix(nrow = length(activities), ncol = ncol(data) - 1)
  
  total[is.na(total)] <- 0
  n[is.na(n)] <- 0
  
  for(row in 1:nrow(data)){
    for(col in 2:ncol(data)){
      r <- match(data[row, 1], activities)
      c <- col - 1
      
      total[r, c] <- as.numeric(total[r, c]) + as.numeric(data[row, col])
      n[r, c] <- n[r, c] + 1
    }
  }
  
  values <- total / n
  
  result <- data.frame(activities, values)
  
  colnames(result) <- colnames(data)
  
  return (result)
}

getX <- function(dir = "test", dataDir = "data"){
  connection <- file(paste(getDir(dir, dataDir), paste("X_", dir, ".txt", sep = ""), sep = "/"), "r")
  lines <- readLines(connection)
  close(connection)
  
  result <- matrix(ncol = length(getXLine(lines, 1)), nrow = length(lines))
  
  for(linePosition in 1:length(lines)){
    line = getXLine(lines, linePosition)
    
    for(i in 1:length(line)){
      result[linePosition, i] <- line[i]
    }
  }
  
  return (result)
}

getXLine <- function(lines, position = 1){
  splitted <- strsplit(lines[position], split = " ")
  
  temp <- as.numeric(unlist(splitted))
  notNa <- temp[!is.na(temp)]
  line <- notNa[!is.null(notNa)]
}

getY <- function(dir = "test", dataDir = "data"){
  connection <- file(paste(getDir(dir, dataDir), paste("y_", dir, ".txt", sep = ""), sep = "/"), "r")
  lines <- readLines(connection)
  close(connection)
  
  result <- vector()
  
  for(linePosition in 1:length(lines)){
    line <- getYLine(lines, linePosition)
    
    result <- c(result, line)
  }
  
  return (result)
}

getYLine <- function(lines, position = 1){
  splitted <- strsplit(lines[position], split = " ")
  
  temp <- as.numeric(unlist(splitted))
  notNa <- temp[!is.na(temp)]
  line <- notNa[!is.null(notNa)]
}

#функція для отримання шляху до папки із файлами
getDir <- function(dir = "test", dataDir = "data"){
  paste(getwd(), dataDir, dir, sep = "/")
}

#функція для отримання назв активностей
getActivities <- function(dataDir = "data"){
  connection <- file(paste(getwd(), dataDir, "activity_labels.txt", sep = "/"), "r")
  activities <- readLines(connection)
  close(connection)
  
  result <- vector()
  
  for(i in 1:length(activities)){
    result <- c(result, unlist(strsplit(activities[i], split = " "))[2])
  }
  
  result
}

#функція для отримання назв, які описують суть змінних
getVariables <- function(dataDir = "data"){
  connection <- file(paste(getwd(), dataDir, "features.txt", sep = "/"), "r")
  activities <- readLines(connection)
  close(connection)
  
  result <- vector()
  
  for(i in 1:length(activities)){
    result <- c(result, unlist(strsplit(activities[i], split = " "))[2])
  }
  
  result
}