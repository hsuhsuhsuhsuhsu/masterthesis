#糖尿病和用藥變數 換過之後 重新跑
#set source
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#set library
library(stringr)
#set parameter
#run fun 的時候直接大家一起綁定參數 改上面就好

drug <- "TCHCData/COV_DrugCount.csv"
d <- read.csv(diab)
diab <- "TCHCData/COV_Diabetes.csv"
formula = "dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS"
seed = 123
#for time split
reCol.AM <- c("HBP_d_PM_systolic","HBP_d_PM_diastolic")
reCol.PM <- c("HBP_d_AM_systolic","HBP_d_AM_diastolic")

#####add cov####
cov.org <- read.csv("TCHCData/addCOV_NoImp.csv")#761 * 15
cov.org <- cov.org[,-1]
addCov <- PlusCov(data = cov.org, Covlist = drug ,
                     IDname = "Mrn_Vis", Cov = c("Drug_conut"),
                     Yname = "dip")
addCov1 <- PlusCov(data = addCov$AddCov.df, Covlist = diab ,
                  IDname = "MRN", Cov = c("DM"),
                  Yname = "dip")
cov.change <- addCov1$AddCov.df#761*16
cov.change <- cov.change[,-which(colnames(cov.change) %in% c("HbA1C","CCB"))]
table(cov.change$visit_HBP_Dmode,cov.change$Drug_conut)
df <-cov.change[which(cov.change$visit_HBP_Dmode %in% 1:3),]#727 *14
library(visdat)
library(ggplot2)
p <- vis_miss(df, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)

write.csv(df,"TCHCData/COV_change_noimp.csv")#727 *14
dim(df)
####complete data after time split####
cmplt.cov <- AMPM[which(complete.cases(AMPM)),]#666 *13
write.csv(cmplt.cov,"TCHCData/COVchange_cmplt_data.csv")
#### imputation #### 
df <- read.csv("TCHCData/COV_change_noimp.csv")
df <- df[,-1]
tmp <- df[,c(1,2,8)]
df <- df[,-c(1,2,8)]
df$visit_HBP_Dmode <- as.factor(df$visit_HBP_Dmode)
df$HBP_d_AM_systolic <- as.numeric(df$HBP_d_AM_systolic)
df$HBP_d_PM_systolic <- as.numeric(df$HBP_d_PM_systolic)
df$HBP_d_AM_diastolic <- as.numeric(df$HBP_d_AM_diastolic)
df$HBP_d_PM_diastolic <- as.numeric(df$HBP_d_PM_diastolic)
df$Gender <- as.factor(df$Gender)
df$Drug_conut <- as.factor(df$Drug_conut)
df$DM <- as.factor(df$DM)
df$HR <- as.numeric(df$HR)
df$dip <- as.factor(df$dip)
str(df)
RF.impute.HH <- missForest(df,verbose=T)#iter =6 
rfimp <- RF.impute.HH$ximp
vis_miss(rfimp, show_perc = F) + coord_flip()
rfimp.proc <- cbind(tmp,rfimp)#727*14
write.csv(rfimp.proc,file = "TCHCData/RFimp.csv ")

####time split ####
rf <- read.csv("TCHCData/RFimp.csv ")
rf <- rf[,-1]
reCol.AM <- c("HBP_d_PM_systolic","HBP_d_PM_diastolic")
reCol.PM <- c("HBP_d_AM_systolic","HBP_d_AM_diastolic")
data <- rf
cnames <- colnames(data)
AM <- select(data,-reCol.AM)
AM[,"time"] <- 1
PM <- select(data,-reCol.PM)
PM[,"time"] <- 2
colnames(AM) <- 1:(dim(AM)[2])
colnames(PM) <- 1:(dim(PM)[2])
AMPM <- rbind(AM, PM)
colnames(AMPM) <- c(cnames[1:3],"visit","sbp","dbp",cnames[9:14],"time")
AMPM$sbp <- as.numeric(AMPM$sbp)
AMPM$dbp <- as.numeric(AMPM$dbp)
AMPM <- arrange(AMPM, Mrn_Vis, visit)
y.col <- which(colnames(AMPM) %in% "dip")
n <- ncol(AMPM)
col.names <- c(seq(n)[-which(seq(n)==y.col)],y.col)
AMPM <- AMPM[,col.names]
write.csv(AMPM,"TCHCData/COV_change_data.csv")#1454*13

 
### data read ####
df.cov <- read.csv("TCHCData/COV_change_data.csv")
df.cov <- df.cov[,-1]#1454*13
df.cov <- df.cov[which(df.cov$visit %in% 1),]#1048*13
dim(df.cov)
df <- read.csv("TCHCData/noCOVdata.csv")
df <- df[,-1]#1634*8
df <- df[which(df$visit %in% 1),]#1102*8
dim(df)
#### Train Test split by=Case ####
Case.V1 <- TrainTest(data = df.cov, VisitOrCase = "Case",
                      nfixed = T, Train = NULL,
                      Test = NULL, seed = seed,
                      removeCategory = NULL, Trainper = 0.8)
C.V1.Train <- Case.V1$`Training set`#838*13
C.V1.Test <- Case.V1$`Test set`#210*13
write.csv(C.V1.Train,"TCHCData/COV_case_V1Train.csv")
write.csv(C.V1.Test,"TCHCData/COV_case_V1Test.csv")

Case.NO.V1 <- TrainTest(data = df, VisitOrCase = "Case",
                     nfixed = T, Train = NULL,
                     Test = NULL, seed = seed,
                     removeCategory = NULL, Trainper = 0.8)
CN.V1.Train <- Case.NO.V1$`Training set`#880*8
CN.V1.Test <- Case.NO.V1$`Test set`#222*8
write.csv(CN.V1.Train,"TCHCData/CASE_V1Train.csv")
write.csv(CN.V1.Test,"TCHCData/CASE_V1Test.csv")
#### +HOS 校正 ####
#### COV ####
library(stringr)
covdf <- read.csv("TCHCData/COV_case_V1Train.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
table(covdf$hos)
#BY CASE的樣本數多 所以不按照是不是長庚 按照北中南校正
for(r in 1 :nrow(covdf)){
  if(covdf[r,"hos"]=="CGMHLK"| covdf[r,"hos"]=="FE"|covdf[r,"hos"]=="NTUH"){
    covdf[r,"HOS"] <- 1
  }else if (covdf[r,"hos"]=="CMUH"){
    covdf[r,"HOS"] <- 2
  }else{
    covdf[r,"HOS"] <- 3
  }
}
table(covdf$HOS)
covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/CASE_COV_hos_Train.csv")

#####TEST DATA####
covdf <- read.csv("TCHCData/COV_case_V1Test.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
table(covdf$hos)

#BY CASE的樣本數多 所以不按照是不是長庚 按照北中南校正
for(r in 1 :nrow(covdf)){
  if(covdf[r,"hos"]=="CGMHLK"| covdf[r,"hos"]=="FE"|covdf[r,"hos"]=="NTUH"){
    covdf[r,"HOS"] <- 1
  }else if (covdf[r,"hos"]=="CMUH"){
    covdf[r,"HOS"] <- 2
  }else{
    covdf[r,"HOS"] <- 3
  }
}
table(covdf$HOS)

covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/CASE_COV_hos_Test.csv")
#### NO COV ####
df <- read.csv("TCHCData/CASE_V1Train.csv")
df <- df[,-1]#880*8
df["hos"] <- str_extract(df$MRN,"[A-Z]+")
table(df$hos)
#BY CASE的樣本數多 所以不按照是不是長庚 按照北中南校正
for(r in 1 :nrow(df)){
  if(df[r,"hos"]=="CGMHLK"| df[r,"hos"]=="FE"|df[r,"hos"]=="NTUH"){
    df[r,"HOS"] <- 1
  }else if (df[r,"hos"]=="CMUH"){
    df[r,"HOS"] <- 2
  }else{
    df[r,"HOS"] <- 3
  }
}
table(df$HOS)
df <- df[,c(1:7,9,10,8)]
write.csv(df,"TCHCData/CASE_NOcov_hos_Train.csv")#880*10
#### TEST DATA####
df <- read.csv("TCHCData/CASE_V1Test.csv")
df <- df[,-1]#222*8
df["hos"] <- str_extract(df$MRN,"[A-Z]+")
table(df$hos)
#BY CASE的樣本數多 所以不按照是不是長庚 按照北中南校正
for(r in 1 :nrow(df)){
  if(df[r,"hos"]=="CGMHLK"| df[r,"hos"]=="FE"|df[r,"hos"]=="NTUH"){
    df[r,"HOS"] <- 1
  }else if (df[r,"hos"]=="CMUH"){
    df[r,"HOS"] <- 2
  }else{
    df[r,"HOS"] <- 3
  }
}
table(df$HOS)
df <- df[,c(1:7,9,10,8)]
write.csv(df,"TCHCData/CASE_NOcov_hos_Test.csv")
#### Train Test split by=Visit ####
df.cov <- read.csv("TCHCData/COV_change_data.csv")
df.cov <- df.cov[,-1]#1454*13
table(df.cov$visit)
Visit.V12 <- TrainTest(data = df.cov, VisitOrCase = "Visit",
                     nfixed = T, Train = 1:2,
                     Test = 3, seed = seed,
                     removeCategory = NULL, Trainper = 0.8)
V.V12.Train <- Visit.V12$`Training set`
V.V3.Test <- Visit.V12$`Test set`
Train.V12V3_uncomplete <- V.V12.Train %>% group_by(MRN) %>% filter(n()!=4)
V.V12.Train <- V.V12.Train[-which(V.V12.Train$MRN %in% Train.V12V3_uncomplete$MRN),]#324
V.V3.Test <- V.V3.Test[-which(V.V3.Test$MRN %in% Train.V12V3_uncomplete$MRN),]#162
dim(V.V12.Train)#284*13
dim(V.V3.Test)#142*13
View(V.V12.Train)
View(V.V3.Test)
write.csv(V.V12.Train,"TCHCData/COV_VISIT_V12Train.csv")
write.csv(V.V3.Test,"TCHCData/COV_VISIT_V3Test.csv")

V.V1.Train <- V.V12.Train[which(V.V12.Train$visit==1),]
V.V2.Test <- V.V12.Train[which(V.V12.Train$visit==2),]
dim(V.V1.Train)#142
dim(V.V2.Test)#142
write.csv(V.V1.Train,"TCHCData/COV_VISIT_V1Train.csv")
write.csv(V.V2.Test,"TCHCData/COV_VISIT_V2Test.csv")
#### +HOS 校正 ####
library(stringr)
covdf <- read.csv("TCHCData/COV_VISIT_V12Train.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
table(covdf$hos)
covdf["HOS"] <- ifelse(covdf$hos=="CGMHLK",1,0)
table(covdf$HOS)
covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/VISIT_COV_hos_V12Train.csv")

covdf <- read.csv("TCHCData/COV_VISIT_V3Test.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
table(covdf$hos)
covdf["HOS"] <- ifelse(covdf$hos=="CGMHLK",1,0)
table(covdf$HOS)
covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/VISIT_COV_hos_V3Test.csv")

covdf <- read.csv("TCHCData/COV_VISIT_V1Train.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
table(covdf$hos)
covdf["HOS"] <- ifelse(covdf$hos=="CGMHLK",1,0)
table(covdf$HOS)
covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/VISIT_COV_hos_V1Train.csv")

covdf <- read.csv("TCHCData/COV_VISIT_V2Test.csv")
covdf <- covdf[,-1]
covdf["hos"] <- str_extract(covdf$MRN,"[A-Z]+")
table(covdf$hos)
covdf["HOS"] <- ifelse(covdf$hos=="CGMHLK",1,0)
table(covdf$HOS)
covdf <- covdf[,c(1:12,14,15,13)]
write.csv(covdf,"TCHCData/VISIT_COV_hos_V2Test.csv")


#### 考慮age要不要分組####
##### 70% Training####
Case.V1.70<- TrainTest(data = df.cov, VisitOrCase = "Case",
                       nfixed = T, Train = NULL,
                       Test = NULL, seed = seed,
                       removeCategory = NULL, Trainper = 0.7)
C.v1.70Trian <- Case.V1.70$`Training set`#732
C.v1.70Test <- Case.V1.70$`Test set`#316
write.csv(C.v1.70Trian,"TCHCData/COV_case_V1Train70.csv")
write.csv(C.v1.70Test,"TCHCData/COV_case_V1Test70.csv")
#

