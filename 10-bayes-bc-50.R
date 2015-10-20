seeds <- c(121, 555, 99, 402, 678, 101, 52, 741, 222, 832)

library(RWeka)
library(rJava)
library(xtable)

csv_data <- read.csv("~/anomalies/50-topics/Processed_Data_50.csv", na.strings = c("", "NA"))
colnames(csv_data)
csv_data[, 1] <- NULL
csv_data[, 1] <- NULL
csv_data[, 1] <- NULL
outputDir <- "~/anomalies/50-topics/bayes/binary-classifications/"

NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")

all_summaries <- list()

for(iteration in 1:10) {
  
  n <- matrix(nrow = 7, ncol = 23)
  best_performers <- data.frame(n, stringsAsFactors = FALSE)
  x <- 1
  
  all_correct <- vector()
  all_incorrect <- vector()
  all_kappa <- vector()
  
  rank_cor <- vector() 
  rank_inc <- vector() 
  rank_kap <- vector()
  raw_cor <- vector() 
  raw_inc <- vector() 
  raw_kap <- vector()
  top_cor <- vector() 
  top_inc <- vector() 
  top_kap <- vector()
  
  #Shoould be 21:35
  for(fileName in colnames(csv_data)[2]) {
    
    data <- read.csv(paste("~/anomalies/50-topics/binary-classifications/", fileName, ".csv", sep = ''))
    
    data[, 1] <- NULL
    
    #Should be 21 to 35
    for(i in 108:length(colnames(data))) {
      print(colnames(data)[i])
      bleh <- matrix(ncol = 20, nrow = 3)
      test_frame <- data.frame(bleh)
      colnames(test_frame) <- c("pctCorrect", "pctIncorrect", "Kappa", "TP Rate T", "TP Rate F", "FP Rate T", "FP Rate F", 
                                "Precision T", "Precision F", "Recall T", "Recall F", "F-Measure T", "F-Measure F", "MCC T", 
                                "MCC F", "ROC Area T", "ROC Area F", "PRC Area T", "PRC Area F", "Frequency")
      row.names(test_frame) <- c("Ranks", "Topic Numbers", "Top 5")
      name <- ""

      for(test in 1:3) {
        if(test == 1) {
          #Ranks 36 to 185
          m <- matrix(nrow = length(data[, i]), ncol = 51)
          temp_data <- data.frame(m)
          temp_data[1] <- data[, i]
          temp_data[, 2:51] <- data[, 3:52]
          name <- "Ranks"
        }
        if(test == 2) {
          #Raw Topic Values 186 to 335
          m <- matrix(nrow = length(data[, i]), ncol = 51)
          temp_data <- data.frame(m)
          temp_data[1] <- data[, i]
          temp_data[, 2:51] <- data[, 53:102]
          name <- "Values"
        }
        if(test == 3) {
          #Top 5 336 to 340
          m <- matrix(nrow = length(data[, i]), ncol = 6)
          temp_data <- data.frame(m)
          temp_data[1] <- data[, i]
          temp_data[, 2:6] <- data[, 103:107]
          name <- "Top.5"
        }

        colnames(temp_data)[1] <- "Col1"
        
        j <- NB(Col1~., temp_data, na.action = NULL)#[, 2:length(colnames(temp_data))])
        classifier <- evaluate_Weka_classifier(j, numFolds = 2, class = TRUE, seed = seeds[iteration])
        print(classifier)
        
        row_true <- strsplit(as.character(as.vector(strsplit(as.character(classifier[[1]]), "\n"))[[1]][19]), " ")[[1]]
        row_true <- row_true[row_true != ""]
        
        row_false <- strsplit(as.character(as.vector(strsplit(as.character(classifier[[1]]), "\n"))[[1]][20]), " ")[[1]]
        row_false <- row_false[row_false != ""]

        test_frame[test, 1] <- as.character(classifier[[2]])[1]
        test_frame[test, 2] <- as.character(classifier[[2]])[2]
        test_frame[test, 3] <- as.character(classifier[[2]])[4]
        test_frame[test, 4] <- row_true[1]
        test_frame[test, 5] <- row_false[1]
        test_frame[test, 6] <- row_true[2]
        test_frame[test, 7] <- row_false[2]
        test_frame[test, 8] <- row_true[3]
        test_frame[test, 9] <- row_false[3]
        test_frame[test, 10] <- row_true[4]
        test_frame[test, 11] <- row_false[4]
        test_frame[test, 12] <- row_true[5]
        test_frame[test, 13] <- row_false[5]
        test_frame[test, 14] <- row_true[6]
        test_frame[test, 15] <- row_false[6]
        test_frame[test, 16] <- row_true[7]
        test_frame[test, 17] <- row_false[7]
        test_frame[test, 18] <- row_true[8]
        test_frame[test, 19] <- row_false[8]
        test_frame[test, 20] <- sum(temp_data[, 1] == "TRUE") / length(temp_data[, 1])
        
        if(test == 1) {
          rank_cor <- c(rank_cor, as.numeric(classifier[[2]])[1])
          rank_inc <- c(rank_inc, as.numeric(classifier[[2]])[2])
          rank_kap <- c(rank_kap, as.numeric(classifier[[2]])[4])
        }
        if(test == 2) {
          raw_cor <- c(raw_cor, as.numeric(classifier[[2]])[1])
          raw_inc <- c(raw_inc, as.numeric(classifier[[2]])[2])
          raw_kap <- c(raw_kap, as.numeric(classifier[[2]])[4])
        }
        if(test == 3) {
          top_cor <- c(top_cor, as.numeric(classifier[[2]])[1])
          top_inc <- c(top_inc, as.numeric(classifier[[2]])[2])
          top_kap <- c(top_kap, as.numeric(classifier[[2]])[4])
        }
        
        all_correct <- c(all_correct, as.numeric(classifier[[2]])[1])
        all_incorrect <- c(all_incorrect, as.numeric(classifier[[2]])[2])
        all_kappa <- c(all_kappa, as.numeric(classifier[[2]])[4])

        write.table(as.data.frame(classifier[4]), paste(outputDir, fileName, colnames(data)[i], name, ".CM[", iteration, "].txt", sep = ""), sep = "\t")
        
      }
      
      write.csv(test_frame, paste(outputDir, fileName, colnames(data)[i], "[", iteration, "].csv", sep = ""), sep = '')
      
      output_csv <- read.csv(paste(outputDir, fileName, colnames(data)[i], "[", iteration, "].csv", sep = ""))
      
      #Read it here right after writing it
      #20 cols, 100 rows
      
      tie <- FALSE
      amt_best <- 1
      
      output_csv <- output_csv[output_csv$Kappa == max(as.numeric(output_csv$Kappa)), ]
      
      if(length(output_csv$Kappa) > 1) {
        tie <- TRUE
        output_csv[which.max(output_csv$F.Measure), ]
      }
      
      if(length(output_csv$Kappa) > 1) {
        amt_best <- length(output_csv$Kappa)
        output_csv <- output_csv[1, ]
      }
      output_csv
      best_performers[x, ] <- c(toString(output_csv$X), output_csv[, 2:21], tie, amt_best)
      row.names(best_performers)[x] <- paste(fileName, '-', colnames(data)[i], sep = '')
      x <- x + 1
      
      colnames(best_performers) <- colnames(output_csv)
      colnames(best_performers)[1] <- "Input Variable"
      colnames(best_performers)[22:23] <- c("Tie", "# Best Performers")
      
      all_summaries[[iteration]] <- best_performers
    }
    
  }
  
}

all_means <- best_performers

# for(row in 1:length(row.names(all_means))) {
#   cells <- vector()
#   for(col in 2:length(colnames(all_means)) - 2) {
#     for(layer in 1:10) {
#       cells <- c(cells, all_summaries[[layer]][row, col])
#     }
#     
#     all_means[row, col] <- mean(cells)
#     
#     cells <- vector()
#   }
# }

write.csv(all_means, "~/anomalies/50-topics/bayes/summaries/bc-all-summary-50-2fold.csv")

# mean_table <- read.csv("~/anomalies/10-bayes/50-topics/summaries/bc-all-summary-50-2fold.csv")
# xtable(mean_table)

# colnames(best_performers) <- colnames(output_csv)
# colnames(best_performers)[1] <- "Input Variable"
# colnames(best_performers)[22:23] <- c("Tie", "# Best Performers")
# 
# write.csv(best_performers, paste("~/anomalies/lro_50_topics/naive-bayes-summaries/bc-sixty-six.csv", sep = ""), sep = '')
# 
# print(xtable(best_performers))
# 
# bleh <- output_csv[1]
# toString(bleh$X)
# as.character(as.vector(output_csv))
# as.
# 
# c(as.character(output_csv[, 1]), output_csv[, 2:21])
# a
# 
# 
# 
# 
# 
# mean(all_correct)
# mean(all_incorrect)
# mean(all_kappa)
# # 
# # mean(top_cor)
# # mean(top_inc)
# # mean(top_kap)
# # mean(raw_cor)
# # mean(raw_inc)
# # mean(raw_kap)
# # mean(rank_cor)
# # mean(rank_inc)
# # mean(rank_kap)
# # 
# m <- matrix(nrow = 3, ncol = 3)
# mean_frame <- data.frame(m)
# colnames(mean_frame) <- c("Mean Correct", "Mean Incorrect", "Mean Kappa")
# row.names(mean_frame) <- c("Ranks", "Topic Numbers", "Top 5")
# mean_frame[1, 1] <- mean(rank_cor)
# mean_frame[1, 2] <- mean(rank_inc)
# mean_frame[1, 3] <- mean(rank_kap)
# mean_frame[2, 1] <- mean(raw_cor)
# mean_frame[2, 2] <- mean(raw_inc)
# mean_frame[2, 3] <- mean(raw_kap)
# mean_frame[3, 1] <- mean(top_cor)
# mean_frame[3, 2] <- mean(top_inc)
# mean_frame[3, 3] <- mean(top_kap)
# 
# write.csv(mean_frame, paste("~/anomalies/lro_50_topics/naive-bayes-summaries/bc-sixty-six-means.csv", sep = ""), sep = '')
# 
# print(xtable(mean_frame))
# 
# #write.table(mean_frame, paste(outputDir, "Means", ".txt", sep = ""), sep = "\t")
