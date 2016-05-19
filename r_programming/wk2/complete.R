complete <- function(directory, ids = 1:332) {
  res <- data.frame()
  for(i in ids){
    fileName <- paste(directory, "/", padNumber(toString(i)),".csv",sep='')
    table<- read.csv(fileName,header=TRUE,sep=",")
    nobs <- nrow(na.omit(table))
    res <- rbind(res,c(i,nobs))
  }
  colnames(res) <- c("id","nobs")
  res
}

padNumber <- function(num){
  if(nchar(num) < 3){
    padNumber(paste('0',num,sep=''))  
  } else {
    num
  }
}