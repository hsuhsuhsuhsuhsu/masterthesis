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
df.cov <- read.csv("TCHCData/yesCOVplusHOS.csv")
df.cov <- df.cov[,-1]#1454*14
df.cov <- df.cov[which(df.cov$visit %in% 1),]#1048*13
dim(df.cov)
#### Train Test split by=Case ####
Case.V1 <- TrainTest(data = df.cov, VisitOrCase = "Case",
                      nfixed = T, Train = NULL,
                      Test = NULL, seed = seed,
                      removeCategory = NULL, Trainper = 0.8)
C.V1.Train <- Case.V1$`Training set`#838*13
C.V1.Test <- Case.V1$`Test set`#210*13
write.csv(C.V1.Train,"TCHCData/COV_case_V1Train.csv")
write.csv(C.V1.Test,"TCHCData/COV_case_V1Test.csv")
#### 70% Training####
Case.V1.70<- TrainTest(data = df.cov, VisitOrCase = "Case",
                     nfixed = T, Train = NULL,
                     Test = NULL, seed = seed,
                     removeCategory = NULL, Trainper = 0.7)
C.v1.70Trian <- Case.V1.70$`Training set`#732
C.v1.70Test <- Case.V1.70$`Test set`#316
write.csv(C.v1.70Trian,"TCHCData/COV_case_V1Train70.csv")
write.csv(C.v1.70Test,"TCHCData/COV_case_V1Test70.csv")
####set class of each variable ####
#C.V1.Train <- read.csv("TCHCData/COV_case_V1Train.csv")
#C.V1.Test <- read.csv("TCHCData/COV_case_V1Test.csv")
#C.V1.Train <- C.V1.Train[,-1]
#C.V1.Test <- C.V1.Test[,-1]
C.V1.Train$visit <- as.factor(C.V1.Train$visit)
C.V1.Train$sbp <- as.numeric(C.V1.Train$sbp)
C.V1.Train$dbp <- as.numeric(C.V1.Train$dbp)
C.V1.Train$Gender <- as.factor(C.V1.Train$Gender)
C.V1.Train$Drug_conut <- as.factor(C.V1.Train$Drug_conut)
C.V1.Train$DM <- as.factor(C.V1.Train$DM)
C.V1.Train$dip <- as.factor(C.V1.Train$dip)
C.V1.Train$MRN <- as.factor(C.V1.Train$MRN)
str(C.V1.Train)
C.V1.Test$visit <- as.factor(C.V1.Test$visit)
C.V1.Test$sbp <- as.numeric(C.V1.Test$sbp)
C.V1.Test$dbp <- as.numeric(C.V1.Test$dbp)
C.V1.Test$Gender <- as.factor(C.V1.Test$Gender)
C.V1.Test$Drug_conut <- as.factor(C.V1.Test$Drug_conut)
C.V1.Test$DM <- as.factor(C.V1.Test$DM)
C.V1.Test$dip <- as.factor(C.V1.Test$dip)
C.V1.Test$MRN <- as.factor(C.V1.Test$MRN)

#### model Build ####
TR1 <- C.V1.Train
TE1 <- C.V1.Test
########### NO COV ###########
########## With COV ###########
#### 考慮age要不要分組####