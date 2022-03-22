CCTr <- read.csv("TCHCData/CASE_COV_hos_Train.csv")#838
CCTr <- CCTr[,-1]
CCTe <- read.csv("TCHCData/CASE_COV_hos_Test.csv")#210
CCTe <- CCTe[,-1]
CNTr <- read.csv("TCHCData/CASE_NOcov_hos_Train.csv")#880
CNTr <- CNTr[,-1]
CNTe <- read.csv("TCHCData/CASE_NOcov_hos_Test.csv")#222
CNTe <- CNTe[,-1]

VNTr.12 <- read.csv("TCHCData/VISIT_NOcov_hos_V12Train.csv")#324
VNTr.12 <- VNTr.12[,-1]
VNTe.3 <- read.csv("TCHCData/VISIT_NOcov_hos_V3Test.csv")#162
VNTe.3 <- VNTe.3[,-1]
VNTr.1 <- read.csv("TCHCData/VISIT_NOcov_hos_V1Train.csv")#162
VNTr.1 <- VNTr.1[,-1]
VNTe.2 <- read.csv("TCHCData/VISIT_NOcov_hos_V2Test.csv")#162
VNTe.2 <- VNTe.2[,-1]

VCTr.12 <- read.csv("TCHCData/VISIT_COV_hos_V12Train.csv")#284
VCTr.12 <- VCTr.12[,-1]
VCTe.3 <- read.csv("TCHCData/VISIT_COV_hos_V3Test.csv")#142
VCTe.3 <- VCTe.3[,-1]
VCTr.1 <- read.csv("TCHCData/VISIT_COV_hos_V1Train.csv")#142
VCTr.1 <- VCTr.1[,-1]
VCTe.2 <- read.csv("TCHCData/VISIT_COV_hos_V2Test.csv")#284
VCTe.2 <- VCTe.2[,-1]

#other file
#for testing imputation consistency
cmplt <- "TCHCData/COVchange_cmplt_data.csv"
rfimp <- "TCHCData/RFimp.csv "

####set class of each variable ####
colnames(CCTr)
CCTr$visit <- as.factor(CCTr$visit)
CCTr$sbp <- as.numeric(CCTr$sbp)
CCTr$dbp <- as.numeric(CCTr$dbp)
CCTr$Gender <- as.factor(CCTr$Gender)
CCTr$Drug_conut <- factor(CCTr$Drug_conut, levels = c(0,1,2,3,4,5))
CCTr$DM <- as.factor(CCTr$DM)
CCTr$dip <- as.factor(CCTr$dip)
CCTr$MRN <- as.factor(CCTr$MRN)
CCTr$HOS <- as.factor(CCTr$HOS)
str(CCTr)

CCTe$visit <- as.factor(CCTe$visit)
CCTe$sbp <- as.numeric(CCTe$sbp)
CCTe$dbp <- as.numeric(CCTe$dbp)
CCTe$Gender <- as.factor(CCTe$Gender)
CCTe$Drug_conut <- factor(CCTe$Drug_conut, levels = c(0,1,2,3,4,5))
CCTe$DM <- as.factor(CCTe$DM)
CCTe$dip <- as.factor(CCTe$dip)
CCTe$MRN <- as.factor(CCTe$MRN)
CCTe$HOS <- as.factor(CCTe$HOS)
str(CCTe)

colnames(CNTr)[4] <- "sbp"
colnames(CNTr)[5] <- "dbp"
CNTr$visit <- as.factor(CNTr$visit)
CNTr$sbp <- as.numeric(CNTr$sbp)
CNTr$dbp <- as.numeric(CNTr$dbp)
CNTr$dip <- as.factor(CNTr$dip)
CNTr$MRN <- as.factor(CNTr$MRN)
CNTr$HOS <- as.factor(CNTr$HOS)
str(CNTr)

colnames(CNTe)[4] <- "sbp"
colnames(CNTe)[5] <- "dbp"
CNTe$visit <- as.factor(CNTe$visit)
CNTe$sbp <- as.numeric(CNTe$sbp)
CNTe$dbp <- as.numeric(CNTe$dbp)
CNTe$dip <- as.factor(CNTe$dip)
CNTe$MRN <- as.factor(CNTe$MRN)
CNTe$HOS <- as.factor(CNTe$HOS)
str(CNTe)

colnames(VNTr.12)[4] <- "sbp"
colnames(VNTr.12)[5] <- "dbp"
VNTr.12$visit <- factor(VNTr.12$visit, levels = c(1,2,3))
VNTr.12$sbp <- as.numeric(VNTr.12$sbp)
VNTr.12$dbp <- as.numeric(VNTr.12$dbp)
VNTr.12$dip <- as.factor(VNTr.12$dip)
VNTr.12$MRN <- as.factor(VNTr.12$MRN)
VNTr.12$HOS <- as.factor(VNTr.12$HOS)
str(VNTr.12)

colnames(VNTe.3)[4] <- "sbp"
colnames(VNTe.3)[5] <- "dbp"
VNTe.3$visit <- factor(VNTe.3$visit, levels = c(1,2,3))
VNTe.3$sbp <- as.numeric(VNTe.3$sbp)
VNTe.3$dbp <- as.numeric(VNTe.3$dbp)
VNTe.3$dip <- as.factor(VNTe.3$dip)
VNTe.3$MRN <- as.factor(VNTe.3$MRN)
VNTe.3$HOS <- as.factor(VNTe.3$HOS)
str(VNTe.3)

colnames(VNTr.1)[4] <- "sbp"
colnames(VNTr.1)[5] <- "dbp"
VNTr.1$visit <- as.factor(VNTr.1$visit)
VNTr.1$sbp <- as.numeric(VNTr.1$sbp)
VNTr.1$dbp <- as.numeric(VNTr.1$dbp)
VNTr.1$dip <- as.factor(VNTr.1$dip)
VNTr.1$MRN <- as.factor(VNTr.1$MRN)
VNTr.1$HOS <- as.factor(VNTr.1$HOS)
str(VNTr.1)

colnames(VNTe.2)[4] <- "sbp"
colnames(VNTe.2)[5] <- "dbp"
VNTe.2$visit <- as.factor(VNTe.2$visit)
VNTe.2$sbp <- as.numeric(VNTe.2$sbp)
VNTe.2$dbp <- as.numeric(VNTe.2$dbp)
VNTe.2$dip <- as.factor(VNTe.2$dip)
VNTe.2$MRN <- as.factor(VNTe.2$MRN)
VNTe.2$HOS <- as.factor(VNTe.2$HOS)
str(VNTe.2)



VCTr.12$visit <- factor(VCTr.12$visit, levels = c(1,2,3))
VCTr.12$sbp <- as.numeric(VCTr.12$sbp)
VCTr.12$dbp <- as.numeric(VCTr.12$dbp)
VCTr.12$Gender <- as.factor(VCTr.12$Gender)
VCTr.12$Drug_conut <- factor(VCTr.12$Drug_conut, levels = c(0,1,2,3,4,5))
VCTr.12$DM <- as.factor(VCTr.12$DM)
VCTr.12$dip <- as.factor(VCTr.12$dip)
VCTr.12$MRN <- as.factor(VCTr.12$MRN)
VCTr.12$HOS <- as.factor(VCTr.12$HOS)
str(VCTr.12)


VCTe.3$visit <- factor(VCTe.3$visit, levels = c(1,2,3))
VCTe.3$sbp <- as.numeric(VCTe.3$sbp)
VCTe.3$dbp <- as.numeric(VCTe.3$dbp)
VCTe.3$Gender <- as.factor(VCTe.3$Gender)
VCTe.3$Drug_conut <- factor(VCTe.3$Drug_conut, levels = c(0,1,2,3,4,5))
VCTe.3$DM <- as.factor(VCTe.3$DM)
VCTe.3$dip <- as.factor(VCTe.3$dip)
VCTe.3$MRN <- as.factor(VCTe.3$MRN)
VCTe.3$HOS <- as.factor(VCTe.3$HOS)
str(VCTe.3)


VCTr.1$visit <- as.factor(VCTr.1$visit)
VCTr.1$sbp <- as.numeric(VCTr.1$sbp)
VCTr.1$dbp <- as.numeric(VCTr.1$dbp)
VCTr.1$Gender <- as.factor(VCTr.1$Gender)
VCTr.1$Drug_conut <- factor(VCTr.1$Drug_conut, levels = c(0,1,2,3,4,5))
VCTr.1$DM <- as.factor(VCTr.1$DM)
VCTr.1$dip <- as.factor(VCTr.1$dip)
VCTr.1$MRN <- as.factor(VCTr.1$MRN)
VCTr.1$HOS <- as.factor(VCTr.1$HOS)
str(VCTr.1)


VCTe.2$visit <- as.factor(VCTe.2$visit)
VCTe.2$sbp <- as.numeric(VCTe.2$sbp)
VCTe.2$dbp <- as.numeric(VCTe.2$dbp)
VCTe.2$Gender <- as.factor(VCTe.2$Gender)
VCTe.2$Drug_conut <- factor(VCTe.2$Drug_conut, levels = c(0,1,2,3,4,5))
VCTe.2$DM <- as.factor(VCTe.2$DM)
VCTe.2$dip <- as.factor(VCTe.2$dip)
VCTe.2$MRN <- as.factor(VCTe.2$MRN)
VCTe.2$HOS <- as.factor(VCTe.2$HOS)
str(VCTe.2)