pollutantmean <- function(directory, pollutant, id = 1:332) {
  res <- c()
  for(i in id){
    fileName <- paste(directory, "/", padNumber(toString(i)),".csv",sep='')
    table<- read.csv(fileName,header=TRUE)
    table <- na.omit(table)
    res <- c(res,table[,pollutant])
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