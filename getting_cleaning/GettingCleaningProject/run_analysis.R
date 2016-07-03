library(dplyr)

create_activity_factor <- function(lst) {
  factor(lst, labels = c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING", "STANDING","LAYING"))
}

keep_columns <- function(read_table){
  features <- tbl_df(read.table("features.txt",stringsAsFactors = F))
  to_keep <- filter(features, grepl('std|mean',V2))
  only_cols <- select(read_table, to_keep$V1)
  for(i in 1:length(to_keep$V2)){
    names(only_cols)[i] <- to_keep[i,2]
  }
  only_cols
}

process_files <- function(tableFP,labelsFP,subjectFP){
  table <- read.table(tableFP)
  labels <- scan(labelsFP)
  subjects <- scan(subjectFP)
  factors <- create_activity_factor(labels)
  cleaned_table <- keep_columns(table)
  mutate(cleaned_table, activity = factors, subject = subjects)
}

getAverages <- function(dt){
  act_sum <- group_by(dt, activity) %>% summarise_each(funs(mean)) %>% select(-subject,-activity)
  subj_sum <- group_by(dt, subject) %>% summarise_each(funs(mean)) %>% select(-subject,-activity)
  merge(act_sum, subj_sum, all=TRUE)
}

getSummary <- function(){
  training <- process_files("train/X_train.txt","train/y_train.txt","train/subject_train.txt")
  testing <- process_files("test/X_test.txt","test/y_test.txt","test/subject_test.txt")
  final <- merge(training,testing, all=TRUE)
}

run <- function(){
  sum <- getSummary()
  avg <- getAverages(sum)
  write.table(sum, "summary.csv")
  write.table(avg, "averages.csv", row.names = FALSE)
}
