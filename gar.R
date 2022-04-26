library(readr)
library(dplyr)
#### 1 ####
CASE_COV_select_wNA <- read_csv("TCHCData/CASE_COV_select_RFimp.csv")

this.data <- CASE_COV_select_wNA
this.data <- dplyr::select(this.data, -c(1, sbp, dbp))
#this.data <- unique(this.data)
this.data <- filter(this.data, time == 1) %>% select(-time)

tmp <- CASE_COV_select_wNA[, c("MRN", "sbp", "dbp", "time")]
tmp1 <- dplyr::filter(tmp, time == 1)
colnames(tmp1)[2:3] <- c("sbp_d", "dbp_d")
tmp2 <- dplyr::filter(tmp, time == 2)
colnames(tmp2)[2:3] <- c("sbp_n", "dbp_n")
newtmp <- cbind(tmp1, tmp2)[, c(1:3, 6:7)]
dim(newtmp)
dim(this.data)

#View(newtmp)
result <- newtmp %>% inner_join(this.data, by = "MRN")
#View(result)

#length(unique(result$MRN))
d <- apply(result, 2, is.na)
d2 <- apply(d*1, 2, sum)

View(round(d2/524, 3) * 100)

?write.csv()
#the data is CASE_COV_select_RFimp.csv, ,not CASE_COV_select_wNA_1p1r.csv
write.csv(result, file = "TCHCData/CASE_COV_select_wNA_1p1r.csv")




#### 2 #####
d <- read_csv("TCHCData/VISIT_COV_select_RFimp.csv")
myID <- 3 # 1, 2, 3 (depend on visit)
this.d <- filter(d, visit == myID)
this.data <- this.d
this.data <- dplyr::select(this.data, -c(1, sbp, dbp))
#this.data <- unique(this.data)
this.data <- filter(this.data, time == 1) %>% select(-time)
#View(this.data)

tmp <- this.d[, c("MRN", "sbp", "dbp", "time")]
tmp1 <- dplyr::filter(tmp, time == 1)
colnames(tmp1)[2:3] <- c("sbp_d", "dbp_d")
tmp2 <- dplyr::filter(tmp, time == 2)
colnames(tmp2)[2:3] <- c("sbp_n", "dbp_n")
newtmp <- cbind(tmp1, tmp2)[, c(1:3, 6:7)]
dim(newtmp)
dim(this.data)

#result1 <- newtmp %>% inner_join(this.data, by = "MRN")
#result2 <- newtmp %>% inner_join(this.data, by = "MRN")
result3 <- newtmp %>% inner_join(this.data, by = "MRN")


result <- rbind(result1, result2, result3)
result <- arrange(result, MRN)
result <- result[, c(1, 6, 2:5, 7:dim(result)[2])]

#the data is VISIT_COV_select_RFimp.csv, not Visit_COV_select_wNA_1p1r.csv
write.csv(result, file = "TCHCData/Visit_COV_select_wNA_1p1r.csv")


#### 3 ####
CNTr <- read.csv("TCHCData/CASE_NOcov_hos_Train.csv")#880
CNTr <- CNTr[,-1]
CNTe <- read.csv("TCHCData/CASE_NOcov_hos_Test.csv")#222
CNTe <- CNTe[,-1]
CNTre <- rbind(CNTr, CNTe)
this.data <- CNTre
this.data <- dplyr::select(this.data, -c(sys, dia, time))
this.data <- unique(this.data)

tmp <- CNTre[, c("MRN", "sys", "dia", "time")]
tmp1 <- dplyr::filter(tmp, time == 1)
colnames(tmp1)[2:3] <- c("sys_d", "dia_d")
tmp2 <- dplyr::filter(tmp, time == 2)
colnames(tmp2)[2:3] <- c("sys_n", "dia_n")
newtmp <- cbind(tmp1, tmp2)[, c(1:3, 6:7)]
dim(newtmp)
dim(this.data)

result <- newtmp %>% inner_join(this.data, by = "MRN")
View(result)
result <- result[, c(1, 6, 2:5, 7:dim(result)[2])]
write.csv(result, file = "TCHCData/CASE_NOcov_hos_TrainTest_1p1r.csv")
#
#### 4 ####
VNTr.12 <- read.csv("TCHCData/VISIT_NOcov_hos_V12Train.csv")#324
VNTr.12 <- VNTr.12[,-1]
VNTe.3 <- read.csv("TCHCData/VISIT_NOcov_hos_V3Test.csv")#162
VNTe.3 <- VNTe.3[,-1]
VNTre <- rbind(VNTr.12, VNTe.3)
View(VNTre)


myID <- 3 # 1, 2, 3 (depend on visit)
this.d <- filter(VNTre, visit == myID)
this.data <- this.d
this.data <- dplyr::select(this.data, -c(sys, dia, time))
this.data <- unique(this.data)
#View(this.data)

tmp <- this.d[, c("MRN", "sys", "dia", "time")]
tmp1 <- dplyr::filter(tmp, time == 1)
colnames(tmp1)[2:3] <- c("sys_d", "dia_d")
tmp2 <- dplyr::filter(tmp, time == 2)
colnames(tmp2)[2:3] <- c("sys_n", "dia_n")
newtmp <- cbind(tmp1, tmp2)[, c(1:3, 6:7)]
dim(newtmp)
dim(this.data)

#result1 <- newtmp %>% inner_join(this.data, by = "MRN")
#result2 <- newtmp %>% inner_join(this.data, by = "MRN")
result3 <- newtmp %>% inner_join(this.data, by = "MRN")


result <- rbind(result1, result2, result3)
dim(result)
View(result)
result <- arrange(result, MRN)
result <- result[, c(1, 6, 2:5, 7:dim(result)[2])]
write.csv(result, file = "TCHCData/VISIT_NOcov_hos_V12TrainTest_1p1r.csv")



#### 1 and 3 ####
#dir <- "TCHCData/CASE_COV_select_wNA_1p1r.csv" #1
dir <- "TCHCData/CASE_NOcov_hos_TrainTest_1p1r.csv" #3
d <- read_csv(dir)
ratio <- 0.8
train_ind <- sample(1:dim(d)[1], round(dim(d)[1] * ratio), replace = FALSE)
train <- d[train_ind, ]
test <- d[-train_ind, ]

#1
#write.csv(train, file = "Div/CASE_COV_select_wNA_1p1r_Train.csv")
#write.csv(test, file = "Div/CASE_COV_select_wNA_1p1r_Test.csv")

#3
write.csv(train, file = "Div/CASE_NOcov_hos_TrainTest_1p1r_Train.csv")
write.csv(test, file = "Div/CASE_NOcov_hos_TrainTest_1p1r_Test.csv")
#

#### 2 and 4 ####

#name <- "Visit_COV_select_wNA_1p1r" #2
#name <- "VISIT_NOcov_hos_V12TrainTest_1p1r" #4
d <- read_csv(paste0("TCHCData/", name, ".csv"))
div_rule <- list(1, 2, c(1, 2), 3)
for(i in div_rule){
  print(i)
  d %>% filter(visit %in% i) %>%
    write.csv(file = paste0("Div/", name, 
                            paste0("_visit", toString(i)), ".csv"))
}


#








