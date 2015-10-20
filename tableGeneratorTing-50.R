#test <- as.data.frame(PFR_test[["Gibbs"]])
#test
#lunch

library(xtable)

d <- as.data.frame(PFR_test[["Gibbs"]])
#dc <- as.data.frame(PFR_test[["Gibbs"]])
#dcc <- as.data.frame(PFR_test[["Gibbs"]])
#dci <- as.data.frame(PFR_test[["Gibbs"]])
#dcci <- as.data.frame(PFR_test[["Gibbs"]])

m <- matrix(nrow = 1, ncol = 28)
final_frame <- as.data.frame(m)
row.names(final_frame) <- c("Desc")#, "")#, "Desc_Cause", "Desc_Cause_Cor", "Desc_Cause_Inv", "Desc_Cause_Cor_Inv", "")
topics <- row.names(d)

options(digits = 4)

#needs to be on odds 1,3,5,7,9...25

k <- 1
for(i in 1:26) {
  if(i %% 2 == 1) {
    final_frame[1, i] <- mean(as.numeric(d[k, ]))
    k <- k + 1
  }
}

# k <- 1
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     final_frame[2, i] <- mean(as.numeric(dc[k, ]))
#     k <- k + 1
#   }
# }
# 
# k <- 1
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     final_frame[3, i] <- mean(as.numeric(dcc[k, ]))
#     k <- k + 1
#   }
# }
# 
# k <- 1
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     final_frame[4, i] <- mean(as.numeric(dci[k, ]))
#     k <- k + 1
#   }
# }
# 
# k <- 1
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     final_frame[5, i] <- mean(as.numeric(dcci[k, ]))
#     k <- k + 1
#   }
# }

#final_frame <- as.data.frame(sapply(final_frame, as.numeric))

temp_d <- final_frame[1, ]
# temp_dc <- final_frame[2, ]
# temp_dcc <- final_frame[3, ]
# temp_dci <- final_frame[4, ]
# temp_dcci <- final_frame[5, ]

v1 <- vector()
# v2 <- vector()
# v3 <- vector()
# v4 <- vector()
# v5 <- vector()

k <- 2
for(i in 1:26) {
  if(i %% 2 == 1) {
    if(i == 1) {
      v1 <- c(format(final_frame[1, i], digits = 4), '-')
    } else {
      v1 <- c(v1, final_frame[1, i], format((temp_d[i] - temp_d[i - 2]) / 
                                              (as.numeric(topics[k]) - as.numeric(topics[k - 1])), digits = 2))
      k <- k + 1
    }
  }
}

# k <- 2
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     if(i == 1) {
#       v2 <- c(format(final_frame[2, i], digits = 4), '-')
#     } else {
#       v2 <- c(v2, final_frame[2, i], format((temp_dc[i] - temp_dc[i - 2]) / 
#                                               (as.numeric(topics[k]) - as.numeric(topics[k - 1])), digits = 2))
#       k <- k + 1
#     }
#   }
# }
# 
# k <- 2
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     if(i == 1) {
#       v3 <- c(format(final_frame[3, i], digits = 4), '-')
#     } else {
#       v3 <- c(v3, final_frame[3, i], format((temp_dcc[i] - temp_dcc[i - 2]) / 
#                                               (as.numeric(topics[k]) - as.numeric(topics[k - 1])), digits = 2))
#       k <- k + 1
#     }
#   }
# }
# 
# k <- 2
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     if(i == 1) {
#       v4 <- c(format(final_frame[4, i], digits = 4), '-')
#     } else {
#       v4 <- c(v4, final_frame[4, i], format((temp_dci[i] - temp_dci[i - 2]) / 
#                                               (as.numeric(topics[k]) - as.numeric(topics[k - 1])), digits = 2))
#       k <- k + 1
#     }
#   }
# }
# 
# k <- 2
# for(i in 1:26) {
#   if(i %% 2 == 1) {
#     if(i == 1) {
#       v5 <- c(format(final_frame[5, i], digits = 4), '-')
#     } else {
#       v5 <- c(v5, final_frame[5, i], format((temp_dcci[i] - temp_dcci[i - 2]) / 
#                                               (as.numeric(topics[k]) - as.numeric(topics[k - 1])), digits = 2))
#       k <- k + 1
#     }
#   }
# }

final_frame[1, ] <- v1
# final_frame[2, ] <- v2
# final_frame[3, ] <- v3
# final_frame[4, ] <- v4
# final_frame[5, ] <- v5

#Row means
row_mean <- as.numeric(vector())
for(i in 1) {
  for(k in 1:25) {
    if(k %% 2 == 1) {
      row_mean <- c(row_mean, format(final_frame[i, k], digits = 4))
    }
  }
  final_frame[i, 27] <- format(mean(as.numeric(row_mean), digits = 4))
  row_mean <- as.numeric(vector())
}

#Row mins
row_min <- as.numeric(vector())
for(i in 1) {
  for(k in 4:26) {
    if(k %% 2 == 0) {
      row_min <- c(row_min, abs(as.numeric(final_frame[i, k], digits = 2)))
    }
  }
  final_frame[i, 28] <- min(as.numeric(row_min))
  row_min <- as.numeric(vector())
}

#final_frame[2, 2] <- '-'

#Column summaries
# for(i in 1:28) {
#   if(i %% 2 == 1) {
#     final_frame[2, i] <- mean(as.numeric(final_frame[1:5, i]))
#   }
#   else {
#     if(i != 2)
#       final_frame[2, i] <- min(abs(as.numeric(final_frame[1:5, i])))
#   }
# }

row.names(final_frame) <- c("Desc")#, " ")

print(xtable(final_frame[1:1, ]), type = "latex", floating = FALSE, only.contents = TRUE, include.colnames = FALSE, hline.after = FALSE)

