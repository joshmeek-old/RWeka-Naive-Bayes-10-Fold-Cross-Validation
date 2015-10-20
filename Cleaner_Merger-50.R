library(dplyr)

jpl_data <- read.csv("~/anomalies/Desc_50_Anomalies.csv")

#Removing CSV index column that it generated in the last script
jpl_data[, 1] <- NULL

#Temp storage
temp <- jpl_data[, (3:length(colnames(jpl_data)))]

#Changing column names of the ranks
for(i in 1:50) {
  colnames(jpl_data)[i+2] <- paste("Rank_", i, sep = '')
}

#Putting the ranks into the data frame
for(i in 1:length(jpl_data[, 1])) {
  jpl_data[i, (3:length(colnames(jpl_data)))] <- rank(-jpl_data[i, (3:length(colnames(jpl_data)))])
}

#Merging the data frames together
jpl_data <- merge(x = jpl_data, y = temp, by = "row.names", all = TRUE)
jpl_data[, 1] <- NULL

#Changing column names of the topics
for(i in 1:50) {
  colnames(jpl_data)[i+52] <- paste("Topic_", i, sep = '')
}

colnames(jpl_data)

new_names <- c("Top1", "Top2", "Top3", "Top4", "Top5")

for(name in new_names) {
  jpl_data[, name] <- NA
}

for(i in 1:length(jpl_data[, 1])) {
  single <- jpl_data[i,(3:52)]
  
  value <- which(single == min(single))
  value <- max(as.numeric(value))
  min_val <- min(single)
  
  for(k in 1:5) {
    
    if(min_val < 20) {
    
    col <- paste("Top", k, sep = '')
    jpl_data[i, col] <- value
    
    single[single == min(single)] <- max(single)
    value <- which(single == min(single))
    value <- max(as.numeric(value))
    min_val <- min(single)
    }
  }
  
}

write.csv(jpl_data, file = ("~/anomalies/Processed_Data_50.csv"))
