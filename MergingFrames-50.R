
raw_data <- read.csv("~/anomalies/Desc_50_Topic_Dists.csv")

mer_data <- read.table("~/anomalies/MER_IDs_and_Classes.txt")
msl_data <- read.table("~/anomalies/MSL_IDs_and_Classes.txt")

frame <- as.data.frame(raw_data[, 1])
colnames(frame) <- c("V1")
mer_data[, 1] <- gsub(",", "", mer_data[, 1])
msl_data[, 1] <- gsub(",", "", msl_data[, 1])

mer_data <- mer_data[!duplicated(mer_data), ]
msl_data <- msl_data[!duplicated(msl_data), ]

frame_mer <- merge(frame, mer_data, by = "V1")
frame_msl <- merge(frame, msl_data, by = "V1")

frame <- rbind(frame_mer, frame_msl)

temp_raw_data <- raw_data
colnames(temp_raw_data)[1] <- "V1"
frame <- merge(frame, temp_raw_data, by = "V1")

colnames(frame)[1:2] <- c("Anomaly_ID", "Classification")

write.csv(file = "~/anomalies/Desc_50_Anomalies.csv", frame)
