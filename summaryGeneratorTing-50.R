library(xtable)
library(dplyr)

####DONT TOUCH
lro_frame <- final_frame
#fermi_frame <- final_frame
#swift_frame <- final_frame
####DONT TOUCH

temp_lro <- lro_frame
#temp_fermi <- fermi_frame
#temp_swift <- swift_frame

colnames(temp_lro) <- c("20", "20 ", "30", "30 ", "40", "40 ", "50", "50 ", "60", "60 ", "70", "70 ", "80", "80 ", 
                        "90", "90 ", "100", "100 ", "150", "150 ", "175", "175 ", "200", "200 ", "225", "225 ")
#colnames(temp_fermi) <- c("20", "20 ", "30", "30 ", "40", "40 ", "50", "50 ", "60", "60 ", "70", "70 ", "80", "80 ", 
#                        "90", "90 ", "100", "100 ", "150", "150 ", "175", "175 ", "200", "200 ", "225", "225 ")
#colnames(temp_swift) <- c("20", "20 ", "30", "30 ", "40", "40 ", "50", "50 ", "60", "60 ", "70", "70 ", "80", "80 ", 
#                        "90", "90 ", "100", "100 ", "150", "150 ", "175", "175 ", "200", "200 ", "225", "225 ")

lro_min <- which.min(lro_frame[, 27])
#fermi_min <- which.min(fermi_frame[, 27])
#swift_min <- which.min(swift_frame[, 27])

num_lro <- abs(sapply(temp_lro[lro_min, 3:26], as.numeric))
#num_fermi <- abs(sapply(temp_fermi[fermi_min, 3:26], as.numeric))
#num_swift <- abs(sapply(temp_swift[swift_min, 3:26], as.numeric))

lro_col <- names(which(num_lro == min(num_lro), arr.ind = TRUE))
#fermi_col <- names(which(num_fermi == min(num_fermi), arr.ind = TRUE))
#swift_col <- names(which(num_swift == min(num_swift), arr.ind = TRUE))

#Need to be able to find column name for the min delta
lro_frame
#fermi_frame
#swift_frame

m <- matrix(ncol = 4, nrow = 1)
summary_frame <- data.frame(m)

row.names(summary_frame) <- c("JPL")#, "Fermi", "Swift")
colnames(summary_frame) <- c("Corpa", "Topics", "Perplex", "Delta")

#Assigning the row name of the lowest
summary_frame[1, 1] <- row.names(lro_frame[lro_min, ])
#summary_frame[2, 1] <- row.names(fermi_frame[fermi_min, ])
#summary_frame[3, 1] <- row.names(swift_frame[swift_min, ])

#Assigning the topic values
summary_frame[1, 2] <- lro_col
#summary_frame[2, 2] <- fermi_col
#summary_frame[3, 2] <- swift_col

#Assigning the lowest perplexity value
summary_frame[1, 3] <- lro_frame[lro_min, 27]
#summary_frame[2, 3] <- fermi_frame[fermi_min, 27]
#summary_frame[3, 3] <- swift_frame[swift_min, 27]

#Assigning the relative change
summary_frame[1, 4] <- lro_frame[lro_min, 28]
#summary_frame[2, 4] <- fermi_frame[fermi_min, 28]
#summary_frame[3, 4] <- swift_frame[swift_min, 28]

print(xtable(summary_frame), type = "latex", floating = FALSE, only.contents = TRUE, include.colnames = FALSE, hline.after = FALSE)

