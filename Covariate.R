#### 整理 covariate ####
library(readr)
library(eeptools)
library(stringr)
#投藥1/0=> Visit Treatment
cVT <- read_csv("TCHCData/cVT.csv")
Drug <- cVT[,c(3:21)]
colN<-colnames(Drug)
Drug <- Drug[,-c(grep("_DD$",colN),grep("_Unit$",colN))]

#hba1c 心跳 => Visit vital
VVS <- read_csv("TCHCData/VVS.csv")
Hb_HR <- VVS[,c("Mrn_Vis","MRN","visit_vital_signs","HbA1C","HR")]
Hb_HR[which(Hb_HR$HR>=999),"HR"] <- NA
Hb_HR[which(Hb_HR$HbA1C>=999),"HbA1C"] <- NA

#年齡性別=> demo 裡面
uDE <- read_csv("TCHCData/uDE.csv")
Age_Sex <- uDE[,c("MRN","Visit Date","Birthday","Gender")]
Age_Sex["BirDay"] <- as.Date(Age_Sex$Birthday,"%m/%d/%Y")
Age_Sex <- Age_Sex[-which(Age_Sex$BirDay > "2010-01-01"),]
Age_Sex <- Age_Sex[-which(Age_Sex$BirDay < "1915-01-01"),]
Age_Sex["VisDate"] <- as.Date(Age_Sex$`Visit Date`,"%m/%d/%Y")
Age_Sex <- Age_Sex[-which(Age_Sex$VisDate < "2011-01-01"),]
Age_Sex <- Age_Sex[-which(Age_Sex$VisDate > "2018-01-01"),]
Age_Sex <- Age_Sex[-which(is.na(Age_Sex$BirDay)|is.na(Age_Sex$VisDate)),]
Age_Sex["Age"] <- floor(age_calc(Age_Sex$BirDay, enddate = Age_Sex$VisDate, units = "years"))

Cov.raw <- list(Age_Sex=Age_Sex, Drug=Drug, Hb_HR=Hb_HR)
#covariate合併成一個data frame
age.sex <- Age_Sex[,c("MRN","Gender","Age")]
Drug_Binary <- Drug
for(r in 1:nrow(Drug_Binary)){
  for(c in 5:ncol(Drug_Binary)){
    if (is.na(Drug_Binary[r,c])){
      Drug_Binary[r,c] <- 0
    }else{
      Drug_Binary[r,c] <- 1
    }
  }
}
Drug_Binary <- Drug_Binary[,-c(3,4)]
sum(is.na(Hb_HR$HR))/dim(Hb_HR)[1]
sum(is.na(Hb_HR$HbA1C))/dim(Hb_HR)[1]
Hb.HR <- Hb_HR[,-3]

Cov.canproc <- list(Age_Sex=age.sex, Drug=Drug_Binary, Hb_HR=Hb.HR)
write.csv(age.sex, file = "Cov_AgeSex.csv")
write.csv(Drug_Binary, file = "Cov_Drug.csv")
write.csv(Hb.HR, file = "Cov_HbHR.csv" )

#HBP血壓變異程度 sd arv cv 
####加入醫院變數做校正####
#怎麼分組
#V1 - V3 的樣本 n=486
Hospital.ad1 <- rbind(Train.V12V3,Test.V12V3)
table(Hospital.ad1$visit)
#取MRN 先去掉重複 再撈英文(醫院名) 看個醫院的
Hos <- as.data.frame(Hospital.ad1$MRN)
Hos <- unique(Hos)
colnames(Hos)[1] <- "MRN"
Hos["Hospital"] <- str_extract(Hos$MRN,"[A-Z]+")
table(Hos$Hospital)
#CGMHLK 林口長庚 45 #CMUH 中國附醫 1
#EDA 義大 3 #KMUH 高醫 32
#發現長庚最多 其他都在中南且加起來比長庚少
#所以分成長庚和非長庚進行校正 (1/0)
df <- read.csv("TCHCData/noCOVdata.csv")
df <- df[,-1]
df["hos"] <- str_extract(df$MRN,"[A-Z]+")
df["HOS"] <- ifelse(df$hos=="CGMHLK",1,0)
df <- df[,c(1:7,9,10,8)]
write.csv(df,"TCHCData/noCOVplusHOS.csv")

covdf <- read.csv("TCHCData/yesCOVdata.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
covdf["HOS"] <- ifelse(covdf$hos=="CGMHLK",1,0)
covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/yesCOVplusHOS.csv")
#BY CASE的樣本數多 所以不按照是不是長庚 按照北中南校正

####MAP平均動脈壓 變數 MAP = 2/3舒張dia + 1/3收縮sys####
#### 後面切割時間的時候參數要改
df <- read.csv("TCHCData/noCOVplusHOS.csv")#已加醫院
df <- df[,-1]
df$sys <- as.numeric(df$sys)
df$dia <- as.numeric(df$dia)
df["MAP"] <- round(2/3*df[,"dia"]+1/3*df[,"sys"],0)
df <- df[,c(1:9,11,10)]
write.csv(df,"TCHCData/noCOV+MAP.csv")
dim(df)

covdf <- read.csv("TCHCData/yesCOVplusHOS.csv")#已加醫院
covdf <- covdf[,-1]
covdf$sys <- as.numeric(covdf$sys)
covdf$dia <- as.numeric(covdf$dia)
covdf["MAP"] <- round(2/3*covdf[,"dia"]+1/3*covdf[,"sys"],0)
covdf <- covdf[,c(1:14,16,15)]
write.csv(covdf,"TCHCData/yesCOV+MAP.csv")
dim(covdf)3



#### 有沒有糖尿病####
#參考欄位 demographic
#1-6. Diabetes	CV_risk_DM
#2-9. 糖尿病	DM =>以這個為參考變數
uDE <- read_csv("TCHCData/uDE.csv")
uDE <- uDE[,-1]
ude.proc <- uDE[,c("MRN","CV_risk_DM","DM")]
table(ude.proc$CV_risk_DM)# TRUE 444 NA 2362
table(ude.proc$DM)#1無1980 2有574 3不詳68 
sum(is.na(ude.proc$DM))#NA 184
table(ude.proc$CV_risk_DM,ude.proc$DM)
ude.proc <- ude.proc[,-which(colnames(ude.proc) %in% "CV_risk_DM")] 
# 如果是NA 取代成3
ude.proc[which(is.na(ude.proc$DM)),2] <- 3
table(ude.proc$DM)
Diabetes <- ude.proc
write.csv(Diabetes,"TCHCData/COV_Diabetes.csv")
library(visdat)
library(ggplot2)
vis_miss(df, show_perc = F) + coord_flip()
#### 用藥種類數 ####
#參考資料 Visit Treatment
#每種藥物先標記成有沒有用
#再sum 起來 變成用藥數量
cVT <- read_csv("TCHCData/cVT.csv")
cVT <- cVT[,-c(1,2)]
Drug <- cVT[,c(5:19)]
colN<-colnames(Drug)
Drug <- Drug[,-c(grep("_DD$",colN),grep("_Unit$",colN))]
Drug <- cbind(cVT[,c(1,2,4)],Drug)
list(table(Drug$CCB),sum(table(Drug$CCB)),sum(is.na(Drug$CCB)))
list(table(Drug$ACEI_ARB),sum(table(Drug$ACEI_ARB)),sum(is.na(Drug$ACEI_ARB)))
list(table(Drug$Diuretics),sum(table(Drug$Diuretics)),sum(is.na(Drug$Diuretics)))
list(table(Drug$Beta_blockers),sum(table(Drug$Beta_blockers)),sum(is.na(Drug$Beta_blockers)))
list(table(Drug$Alpha_blockers),sum(table(Drug$Alpha_blockers)),sum(is.na(Drug$Alpha_blockers)))
Drug_Binary <- Drug
for(r in 1:nrow(Drug_Binary)){
  for(c in 4:ncol(Drug_Binary)){
    if (is.na(Drug_Binary[r,c])){
      Drug_Binary[r,c] <- 0
    }else{
      Drug_Binary[r,c] <- 1
    }
  }
}
for (x in 1 : nrow(Drug_Binary)){
  Drug_Binary[x,"Drug_conut"] <- sum(Drug_Binary[x,c(4:8)])
}
table(Drug_Binary$Drug_conut)

Drug_sum <- Drug_Binary
Drug_sum <- Drug_sum[,c(1,2,3,9)]
write.csv(Drug_sum,"TCHCData/COV_DrugCount.csv")



