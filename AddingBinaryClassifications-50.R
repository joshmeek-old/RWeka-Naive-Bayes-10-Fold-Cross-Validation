
csv_data <- read.csv("~/anomalies_JPL/Processed_JPL_Data_50.csv")

csv_data[, 1] <- NULL
csv_data[, 1] <- NULL
csv_data[, 1] <- NULL

for(name in colnames(csv_data)[2]) {
  
  temp_data <- csv_data
  
  cols <- as.vector(unique(temp_data[, name]))
  cols <- cols[cols != ""]
  cols <- cols[!is.na(cols)]
  
  #Appends the columns
  for(cname in cols) {
    temp_data[, as.character(cname)] <- FALSE
  }
  
  for(j in 1:length(csv_data[, 1])) {
    
    temp <- as.vector(temp_data[j, name])
    
    if(temp != "" && !is.na(temp))
      temp_data[j, as.character(temp)] <- TRUE
  }
  
  write.csv(temp_data, paste("~/anomalies_JPL/binary-classifications/", name, ".csv", sep = ''))
}





