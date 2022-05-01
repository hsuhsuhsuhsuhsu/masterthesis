#read file
getwd()
setwd("Div/")
CC1RTr <- read.csv("CASE_COV_select_wNA_1p1r_Train.csv")
CC1RTr <- CC1RTr[,-c(1,2)]#419*18

CC1RTe <- read.csv("CASE_COV_select_wNA_1p1r_Test.csv")
CC1RTe <- CC1RTe[,-c(1,2)]#105*18


CN1RTr <- read.csv("CASE_NOcov_hos_TrainTest_1p1r_Train.csv")
CN1RTr <- CN1RTr[,-c(1,2)]
CN1RTe <- read.csv("CASE_NOcov_hos_TrainTest_1p1r_Test.csv")
CN1RTe <- CN1RTe[,-c(1,2)]


VC1RTr12 <- read.csv("Visit_COV_select_wNA_1p1r_visit1, 2.csv")
VC1RTr12 <- VC1RTr12[,-c(1,2)]#142*18
VC1RTe3 <- read.csv("Visit_COV_select_wNA_1p1r_visit3.csv")
VC1RTe3 <- VC1RTe3[,-c(1,2)]#71*18

VC1RTr1 <- read.csv("Visit_COV_select_wNA_1p1r_visit1.csv")
VC1RTr1 <- VC1RTr1[,-c(1,2)]#71*18
VC1RTe2 <- read.csv("Visit_COV_select_wNA_1p1r_visit2.csv")
VC1RTe2 <- VC1RTe2[,-c(1,2)]#71*18


VN1RTr12 <- read.csv("VISIT_NOcov_hos_V12TrainTest_1p1r_visit1, 2.csv")
VN1RTr12 <- VN1RTr12[,-c(1,2)]
VN1RTe3 <- read.csv("VISIT_NOcov_hos_V12TrainTest_1p1r_visit3.csv")
VN1RTe3 <- VN1RTe3[,-c(1,2)]


VN1RTr1 <- read.csv ("VISIT_NOcov_hos_V12TrainTest_1p1r_visit1.csv")
VN1RTr1 <- VN1RTr1[,-c(1,2)]
VN1RTe2 <- read.csv("VISIT_NOcov_hos_V12TrainTest_1p1r_visit2.csv")
VN1RTe2 <- VN1RTe2[,-c(1,2)]
#set variable type
CC1RTr$sbp_d <- as.numeric(CC1RTr$sbp_d)
CC1RTr$dbp_d <- as.numeric(CC1RTr$dbp_d)
CC1RTr$sbp_n <- as.numeric(CC1RTr$sbp_n)
CC1RTr$dbp_n <- as.numeric(CC1RTr$dbp_n)
CC1RTr$HR <- as.numeric(CC1RTr$HR)
CC1RTr$BMI <- as.numeric(CC1RTr$BMI)
CC1RTr$Waist <- as.numeric(CC1RTr$Waist)
CC1RTr$office_peri_L_sys <- as.numeric(CC1RTr$office_peri_L_sys)
CC1RTr$office_peri_L_dia <- as.numeric(CC1RTr$office_peri_L_dia)
CC1RTr$HR <- as.numeric(CC1RTr$HR)

CC1RTr$HOS <- factor(CC1RTr$HOS,levels=c(1:3))
CC1RTr$Gender <- as.factor(CC1RTr$Gender)
CC1RTr$Drug_conut <- factor(CC1RTr$Drug_conut,levels=c(0:5))
CC1RTr$DM <- factor(CC1RTr$DM,levels=c(1:3))
CC1RTr$Walk_TM_week <- factor(CC1RTr$Walk_TM_week,levels=c(0:7))
CC1RTr$anti_HP <- factor(CC1RTr$anti_HP,levels=c(1:13))


CC1RTe$sbp_d <- as.numeric(CC1RTe$sbp_d)
CC1RTe$dbp_d <- as.numeric(CC1RTe$dbp_d)
CC1RTe$sbp_n <- as.numeric(CC1RTe$sbp_n)
CC1RTe$dbp_n <- as.numeric(CC1RTe$dbp_n)
CC1RTe$HR <- as.numeric(CC1RTe$HR)
CC1RTe$BMI <- as.numeric(CC1RTe$BMI)
CC1RTe$Waist <- as.numeric(CC1RTe$Waist)
CC1RTe$office_peri_L_sys <- as.numeric(CC1RTe$office_peri_L_sys)
CC1RTe$office_peri_L_dia <- as.numeric(CC1RTe$office_peri_L_dia)
CC1RTe$HR <- as.numeric(CC1RTe$HR)

CC1RTe$HOS <- factor(CC1RTe$HOS,levels=c(1:3))
CC1RTe$Gender <- as.factor(CC1RTe$Gender)
CC1RTe$Drug_conut <- factor(CC1RTe$Drug_conut,levels=c(0:5))
CC1RTe$DM <- factor(CC1RTe$DM,levels=c(1:3))
CC1RTe$Walk_TM_week <- factor(CC1RTe$Walk_TM_week,levels=c(0:7))
CC1RTe$anti_HP <- factor(CC1RTe$anti_HP,levels=c(1:13))

VC1RTr12$MRN <- as.factor(VC1RTr12$MRN)
VC1RTr12$visit <- factor(VC1RTr12$visit,levels=c(1:3))
VC1RTr12$sbp_d <- as.numeric(VC1RTr12$sbp_d)
VC1RTr12$dbp_d <- as.numeric(VC1RTr12$dbp_d)
VC1RTr12$sbp_n <- as.numeric(VC1RTr12$sbp_n)
VC1RTr12$dbp_n <- as.numeric(VC1RTr12$dbp_n)
VC1RTr12$Gender <- as.factor(VC1RTr12$Gender)
VC1RTr12$HR <- as.numeric(VC1RTr12$HR)
VC1RTr12$Drug_conut <- factor(VC1RTr12$Drug_conut,levels=c(0:5))
VC1RTr12$DM <- factor(VC1RTr12$DM,levels=c(1:3))
VC1RTr12$HOS <- factor(VC1RTr12$HOS,levels=c(0:1))
VC1RTr12$BMI <- as.numeric(VC1RTr12$BMI)
VC1RTr12$Waist <- as.numeric(VC1RTr12$Waist)
VC1RTr12$Eco_child<- factor(VC1RTr12$Eco_child,levels=c(1:4))
VC1RTr12$T_pain<- factor(VC1RTr12$T_pain,levels=c(1:3))

VC1RTe3$MRN <- as.factor(VC1RTe3$MRN)
VC1RTe3$visit <- factor(VC1RTe3$visit,levels=c(1:3))
VC1RTe3$sbp_d <- as.numeric(VC1RTe3$sbp_d)
VC1RTe3$dbp_d <- as.numeric(VC1RTe3$dbp_d)
VC1RTe3$sbp_n <- as.numeric(VC1RTe3$sbp_n)
VC1RTe3$dbp_n <- as.numeric(VC1RTe3$dbp_n)
VC1RTe3$Gender <- as.factor(VC1RTe3$Gender)
VC1RTe3$HR <- as.numeric(VC1RTe3$HR)
VC1RTe3$Drug_conut <- factor(VC1RTe3$Drug_conut,levels=c(0:5))
VC1RTe3$DM <- factor(VC1RTe3$DM,levels=c(1:3))
VC1RTe3$HOS <- factor(VC1RTe3$HOS,levels=c(0:1))
VC1RTe3$BMI <- as.numeric(VC1RTe3$BMI)
VC1RTe3$Waist <- as.numeric(VC1RTe3$Waist)
VC1RTe3$Eco_child<- factor(VC1RTe3$Eco_child,levels=c(1:4))
VC1RTe3$T_pain<- factor(VC1RTe3$T_pain,levels=c(1:3))


VC1RTr1$MRN <- as.factor(VC1RTr1$MRN)
VC1RTr1$visit <- factor(VC1RTr1$visit,levels=c(1:3))
VC1RTr1$sbp_d <- as.numeric(VC1RTr1$sbp_d)
VC1RTr1$dbp_d <- as.numeric(VC1RTr1$dbp_d)
VC1RTr1$sbp_n <- as.numeric(VC1RTr1$sbp_n)
VC1RTr1$dbp_n <- as.numeric(VC1RTr1$dbp_n)
VC1RTr1$Gender <- as.factor(VC1RTr1$Gender)
VC1RTr1$HR <- as.numeric(VC1RTr1$HR)
VC1RTr1$Drug_conut <- factor(VC1RTr1$Drug_conut,levels=c(0:5))
VC1RTr1$DM <- factor(VC1RTr1$DM,levels=c(1:3))
VC1RTr1$HOS <- factor(VC1RTr1$HOS,levels=c(0:1))
VC1RTr1$BMI <- as.numeric(VC1RTr1$BMI)
VC1RTr1$Waist <- as.numeric(VC1RTr1$Waist)
VC1RTr1$Eco_child<- factor(VC1RTr1$Eco_child,levels=c(1:4))
VC1RTr1$T_pain<- factor(VC1RTr1$T_pain,levels=c(1:3))

VC1RTe2$MRN <- as.factor(VC1RTe2$MRN)
VC1RTe2$visit <- factor(VC1RTe2$visit,levels=c(1:3))
VC1RTe2$sbp_d <- as.numeric(VC1RTe2$sbp_d)
VC1RTe2$dbp_d <- as.numeric(VC1RTe2$dbp_d)
VC1RTe2$sbp_n <- as.numeric(VC1RTe2$sbp_n)
VC1RTe2$dbp_n <- as.numeric(VC1RTe2$dbp_n)
VC1RTe2$Gender <- as.factor(VC1RTe2$Gender)
VC1RTe2$HR <- as.numeric(VC1RTe2$HR)
VC1RTe2$Drug_conut <- factor(VC1RTe2$Drug_conut,levels=c(0:5))
VC1RTe2$DM <- factor(VC1RTe2$DM,levels=c(1:3))
VC1RTe2$HOS <- factor(VC1RTe2$HOS,levels=c(0:1))
VC1RTe2$BMI <- as.numeric(VC1RTe2$BMI)
VC1RTe2$Waist <- as.numeric(VC1RTe2$Waist)
VC1RTe2$Eco_child<- factor(VC1RTe2$Eco_child,levels=c(1:4))
VC1RTe2$T_pain<- factor(VC1RTe2$T_pain,levels=c(1:3))
