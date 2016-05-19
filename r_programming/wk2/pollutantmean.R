pollutantmean <- function(directory, pollutant, id = 1:332) {
  res <- c()
  for(i in id){
    fileName <- paste(directory, "/", padNumber(toString(i)),".csv",sep='')
    table<- read.csv(fileName,header=TRUE,sep=",")
    cleaned <- table[!is.na(table[,pollutant]),pollutant]
    res <- c(res,cleaned)
  }
  mean(res)
}

padNumber <- function(num){
  if(nchar(num) < 3){
    padNumber(paste('0',num,sep=''))  
  } else {
    num
  }
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)
