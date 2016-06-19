library(plyr)
best <- function(state, outcome) {
  finalClean <- filterToOneOutcome(state,outcome)
  ndx <- order(finalClean[,3],finalClean$Name)
#  minList <- finalClean[which.min(finalClean[,3]),]
#  minList
  ((finalClean[ndx,])[1,])$Name
  }

rankhospital <- function(state, outcome, num = "best"){
  if(num == "best"){
    best(state,outcome)
  } else{
  cleaned <- filterToOneOutcome(state,outcome)
  ndx <- order(cleaned[,3],cleaned$Name)
  final <- cleaned[ndx,]
  if(num == "worst"){
    final[nrow(final),c("Name")]
  } else if(nrow(final) < num){
    NA
  } else{
    final[num,c("Name")]
  }}
}

rankall <- function(outcome, num = "best"){
  outcomes <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
  corrOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if(!(outcome %in% corrOutcomes)){
    stop("invalid outcome")
  }
  cleaned <- trimData(outcomes)
  finalClean <- c()
  if(outcome == "heart attack"){
    finalClean <- cleaned[c("Name","State","Heart.Attack")]
  } else if(outcome == "heart failure"){
    finalClean <- cleaned[c("Name","State","Heart.Failure")]
  } else {
    finalClean <- cleaned[c("Name","State","Pneumonia")]
  }
  finalClean$Name <- as.character(finalClean$Name)
  finalClean <- finalClean[!is.na(finalClean[,3]),]
  splt <- split(finalClean,finalClean$State)
  dfs <- lapply(splt, function(df){
    ndx <- order(df$State,df[,3],df$Name)
    df <- df[ndx,]
    if(num == "best"){
      df[1,c("Name")]
    } else if(num == "worst"){
      df[nrow(df),c("Name")]
    } else {
      df[num,c("Name")] 
    }
  })
  do.call(rbind.data.frame,dfs)
}



filterToOneOutcome <- function(state,outcome){
  outcomes <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
  corrOutcomes <- c("heart attack", "heart failure", "pneumonia")
  if (!(state %in% levels(outcomes$State))){
    stop("invalid state") 
  }
  if(!(outcome %in% corrOutcomes)){
    stop("invalid outcome")
  }
  cleaned <- trimData(outcomes)
  stateOnly <- cleaned[cleaned$State==state,]
  finalClean <- c()
  if(outcome == "heart attack"){
    finalClean <- stateOnly[c("Name","State","Heart.Attack")]
  } else if(outcome == "heart failure"){
    finalClean <- stateOnly[c("Name","State","Heart.Failure")]
  } else {
    finalClean <- stateOnly[c("Name","State","Pneumonia")]
  }
  finalClean$Name <- as.character(finalClean$Name)
  finalClean <- finalClean[!is.na(finalClean[,3]),]
}

trimData <- function(outcomes) {
  cols <- c("Hospital.Name","State","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  onlyCols <- outcomes[cols]
  rename(onlyCols, c("Hospital.Name"="Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"="Heart.Attack","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"="Heart.Failure","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"="Pneumonia"))
}