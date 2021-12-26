#從 845筆有Y的去做 =>收縮 舒張 早上 晚上 平均 =>取4個
#先抓平均完的complete case 去做模型(完整case會變多) 看效果
# case-wise 隨機切割ID 用ID所有資料去訓練 測試
# visit-wise V1 預測 V2 ； V1 V2 預測 V3
#再對X先做隨機森林插補 再抓4個平均 再跑模型 看效果 
#做完再去做 對ABPM插補 再算Y 再跑模型
library(dplyr)
dip<-read.csv("TCHCData/hbp_dip_byx.csv")
na<-which(is.na(dip$dipping.status))
dip <- dip[-na,]#845

colnames(dip)
FourAvg <- dip[,c(2,4,6,8,9,11,12,125)]
sum(complete.cases(FourAvg))#830筆
FourAvg <- FourAvg[complete.cases(FourAvg),]

# case-wise 隨機切割ID 用ID所有資料去訓練 測試
#Split Train test
length(unique(FourAvg$MRN))#564人
people <-as.data.frame(unique(FourAvg$MRN))
set.seed(1226)
trainID <- as.data.frame(sample(people$`unique(FourAvg$MRN)`,nrow(people)*0.8))
colnames(trainID)[1] <- "MRN"
Train <- merge(trainID,FourAvg,by = "MRN",all.x = T)
Train <-  Train[,-1]
Train <- arrange(Train,Train$Mrn_Vis)
Test <- rbind(Train, FourAvg)
Test <- Test[!(duplicated(Test) | duplicated(Test, fromLast = TRUE)), ]
#Train Model


# visit-wise 所有人的V1 預測所有人的 V2 ； V1 V2 預測 V3