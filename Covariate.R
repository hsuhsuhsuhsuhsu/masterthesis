#### 整理 covariate ####
library(readr)
library(eeptools)
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
Cov.proc <- merge(Drug_Binary,Hb.HR,by="Mrn_Vis")
all(Cov.proc$MRN.x==Cov.proc$MRN.y)
Cov.proc <- Cov.proc[,-which(colnames(Cov.proc)==c("MRN.y"))]
Cov.proc <- merge(Cov.proc, age.sex,by.x = "MRN.x",by.y = "MRN")
colnames(Cov.proc)[1] <- "MRN"
Cov.ok <- Cov.proc
write.csv(Cov.ok,file="TCHCData/Covariate.csv")
#血壓變異程度 sd arv cv => PDF檔有公式自己算 #暫時先pass


