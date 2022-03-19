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
formula = ""
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
cov.change <- addCov1$AddCov.df
cov.change <- cov.change[,-which(colnames(cov.change) %in% c("HbA1C","CCB"))]
table(cov.change$visit_HBP_Dmode,cov.change$Drug_conut)
df <-cov.change[which(cov.change$visit_HBP_Dmode %in% 1:3),]
library(visdat)
library(ggplot2)
vis_miss(df, show_perc = F) + coord_flip()
write.cscv(df,"TCHCData/COV_change_noimp.csv")
dim(df)
#### data ####
reCol.AM <- c("HBP_d_PM_systolic","HBP_d_PM_diastolic")
reCol.PM <- c("HBP_d_AM_systolic","HBP_d_AM_diastolic")
data <- df
cnames <- colnames(data)
AM <- select(data,-reCol.AM)
AM[,"time"] <- 1
PM <- select(data,-reCol.PM)
PM[,"time"] <- 2
colnames(AM) <- 1:(dim(AM)[2])
colnames(PM) <- 1:(dim(PM)[2])
AMPM <- rbind(AM, PM)
colnames(AMPM) <- c(cnames[1:3],"sbp","dbp",cnames[8:14],"time")
AMPM$sys <- as.numeric(AMPM$sys)
AMPM$dia <- as.numeric(AMPM$dia)
AMPM <- arrange(AMPM, Mrn_Vis, visit)
y.col <- which(colnames(AMPM) %in% "dip")
n <- ncol(AMPM)
col.names <- c(seq(n)[-which(seq(n)==y.col)],y.col)
AMPM <- AMPM[,col.names]
write.csv(AMPM,"TCHCData/COV_change_data.csv")
#### imputation #### 
#complete data
cmplt.cov <- AMPM[which(complete.cases(AMPM)),]#666 *13
write.csv(cmplt.cov,"TCHCData/COVchange_cmplt_data.csv")
#### model Build ####
########### NO COV ###########
########## With COV ###########
