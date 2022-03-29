### COV SELECT
CCTr <- read.csv("TCHCData/CASE_COV_select_Train.csv")#838
CCTr<- CCTr[,-1]
CCTe <- read.csv("TCHCData/CASE_COV_select_Test.csv")#210
CCTe<- CCTe[,-1]

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


VCTr.12 <- read.csv("TCHCData/VISIT_COV_select_V12Train.csv")#284
VCTr.12<- VCTr.12[,-1]
VCTe.3 <- read.csv("TCHCData/VISIT_COV_select_V3Test.csv")#142
VCTe.3<- VCTe.3[,-1]

VCTr.1 <- read.csv("TCHCData/VISIT_COV_select_V1Train.csv")#142
VCTr.1<- VCTr.1[,-1]
VCTe.2 <- read.csv("TCHCData/VISIT_COV_select_V2Test.csv")#142
VCTe.2<- VCTe.2[,-1]

####set class of each variable ####
str(CCTr)
CCTr$sbp <- as.numeric(CCTr$sbp)
CCTr$dbp <- as.numeric(CCTr$dbp)
CCTr$HR <- as.numeric(CCTr$HR)
CCTr$BMI <- as.numeric(CCTr$BMI)
CCTr$Waist <- as.numeric(CCTr$Waist)
CCTr$office_peri_L_sys <- as.numeric(CCTr$office_peri_L_sys)
CCTr$office_peri_L_dia <- as.numeric(CCTr$office_peri_L_dia)
CCTr$HR <- as.numeric(CCTr$HR)

CCTr$HOS <- factor(CCTr$HOS,levels=c(1:3))
CCTr$Gender <- as.factor(CCTr$Gender)
CCTr$Drug_conut <- factor(CCTr$Drug_conut,levels=c(0:5))
CCTr$DM <- factor(CCTr$DM,levels=c(1:3))
CCTr$Walk_TM_week <- factor(CCTr$Walk_TM_week,levels=c(0:7))
CCTr$anti_HP <- factor(CCTr$anti_HP,levels=c(1:13))

str(CCTe)
CCTe$sbp <- as.numeric(CCTe$sbp)
CCTe$dbp <- as.numeric(CCTe$dbp)
CCTe$HR <- as.numeric(CCTe$HR)
CCTe$BMI <- as.numeric(CCTe$BMI)
CCTe$Waist <- as.numeric(CCTe$Waist)
CCTe$office_peri_L_sys <- as.numeric(CCTe$office_peri_L_sys)
CCTe$office_peri_L_dia <- as.numeric(CCTe$office_peri_L_dia)
CCTe$HR <- as.numeric(CCTe$HR)

CCTe$HOS <- factor(CCTe$HOS,levels=c(1:3))
CCTe$Gender <- as.factor(CCTe$Gender)
CCTe$Drug_conut <- factor(CCTe$Drug_conut,levels=c(0:5))
CCTe$DM <- factor(CCTe$DM,levels=c(1:3))
CCTe$Walk_TM_week <- factor(CCTe$Walk_TM_week,levels=c(0:7))
CCTe$anti_HP <- factor(CCTe$anti_HP,levels=c(1:13))





colnames(CNTr)[4] <- "sbp"
colnames(CNTr)[5] <- "dbp"
CNTr$visit <- as.factor(CNTr$visit)
CNTr$sbp <- as.numeric(CNTr$sbp)
CNTr$dbp <- as.numeric(CNTr$dbp)
#CNTr$dip <- as.factor(CNTr$dip)
CNTr$MRN <- as.factor(CNTr$MRN)
CNTr$HOS <- as.factor(CNTr$HOS)
str(CNTr)

colnames(CNTe)[4] <- "sbp"
colnames(CNTe)[5] <- "dbp"
CNTe$visit <- as.factor(CNTe$visit)
CNTe$sbp <- as.numeric(CNTe$sbp)
CNTe$dbp <- as.numeric(CNTe$dbp)
#CNTe$dip <- as.factor(CNTe$dip)
CNTe$MRN <- as.factor(CNTe$MRN)
CNTe$HOS <- as.factor(CNTe$HOS)
str(CNTe)

colnames(VNTr.12)[4] <- "sbp"
colnames(VNTr.12)[5] <- "dbp"
VNTr.12$visit <- factor(VNTr.12$visit, levels = c(1,2,3))
VNTr.12$sbp <- as.numeric(VNTr.12$sbp)
VNTr.12$dbp <- as.numeric(VNTr.12$dbp)
#VNTr.12$dip <- as.factor(VNTr.12$dip)
VNTr.12$MRN <- as.factor(VNTr.12$MRN)
VNTr.12$HOS <- as.factor(VNTr.12$HOS)
str(VNTr.12)

colnames(VNTe.3)[4] <- "sbp"
colnames(VNTe.3)[5] <- "dbp"
VNTe.3$visit <- factor(VNTe.3$visit, levels = c(1,2,3))
VNTe.3$sbp <- as.numeric(VNTe.3$sbp)
VNTe.3$dbp <- as.numeric(VNTe.3$dbp)
#VNTe.3$dip <- as.factor(VNTe.3$dip)
VNTe.3$MRN <- as.factor(VNTe.3$MRN)
VNTe.3$HOS <- as.factor(VNTe.3$HOS)
str(VNTe.3)

colnames(VNTr.1)[4] <- "sbp"
colnames(VNTr.1)[5] <- "dbp"
VNTr.1$visit <- as.factor(VNTr.1$visit)
VNTr.1$sbp <- as.numeric(VNTr.1$sbp)
VNTr.1$dbp <- as.numeric(VNTr.1$dbp)
#VNTr.1$dip <- as.factor(VNTr.1$dip)
VNTr.1$MRN <- as.factor(VNTr.1$MRN)
VNTr.1$HOS <- as.factor(VNTr.1$HOS)
str(VNTr.1)

colnames(VNTe.2)[4] <- "sbp"
colnames(VNTe.2)[5] <- "dbp"
VNTe.2$visit <- as.factor(VNTe.2$visit)
VNTe.2$sbp <- as.numeric(VNTe.2$sbp)
VNTe.2$dbp <- as.numeric(VNTe.2$dbp)
#VNTe.2$dip <- as.factor(VNTe.2$dip)
VNTe.2$MRN <- as.factor(VNTe.2$MRN)
VNTe.2$HOS <- as.factor(VNTe.2$HOS)
str(VNTe.2)

str(VCTr.12)
colnames(VCTr.12)
VCTr.12$MRN <- as.factor(VCTr.12$MRN)
VCTr.12$visit <- factor(VCTr.12$visit,levels=c(1:3))
VCTr.12$sbp <- as.numeric(VCTr.12$sbp)
VCTr.12$dbp <- as.numeric(VCTr.12$dbp)
VCTr.12$Gender <- as.factor(VCTr.12$Gender)
VCTr.12$HR <- as.numeric(VCTr.12$HR)
VCTr.12$Drug_conut <- factor(VCTr.12$Drug_conut,levels=c(0:5))
VCTr.12$DM <- factor(VCTr.12$DM,levels=c(1:3))
VCTr.12$HOS <- factor(VCTr.12$HOS,levels=c(1:3))
VCTr.12$BMI <- as.numeric(VCTr.12$BMI)
VCTr.12$Waist <- as.numeric(VCTr.12$Waist)
VCTr.12$Eco_child<- factor(VCTr.12$Eco_child,levels=c(1:4))
VCTr.12$T_pain<- factor(VCTr.12$T_pain,levels=c(1:3))






