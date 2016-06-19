#outcomes <- read.csv("outcome-of-care-measures.csv",na.strings="Not Available")
idaho <- read.csv("idaho_housing.csv")
nonNaVals <- idaho[!is.na(idaho$VAL),]
milHomes <- nonNaVals[nonNaVals$VAL == 24,]
