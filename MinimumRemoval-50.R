
csv_data <- read.csv("~/anomalies/Processed_Data_50.csv")
colnames(csv_data)

data <- csv_data

for(i in 1:length(data[, 1])) {
  
  minimum <- min(as.numeric(data[i, 54:103]))
  
  for(k in 54:103) {
    if(as.numeric(data[i, k]) == minimum)
      data[i, k] <- '?'
  }
}

write.csv(data, "~/anomalies/Processed_Data_50.csv")
