source("complete.R")
corr <- function(directory, threshold = 0){
  comp <- complete(directory, 1:length(list.files(directory)))
  abvThres <- comp[comp$nobs > threshold,]$id
  res <- c()
  for(i in abvThres){
    fileName <- paste(directory, "/", padNumber(toString(i)),".csv",sep='')
    table<- read.csv(fileName,header=TRUE,sep=",")
    correlation <- cor(x=table$sulfate,y=table$nitrate,use="complete")
    res <- c(res,correlation)
  }
  res
}