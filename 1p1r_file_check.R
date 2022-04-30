#read file
getwd()
#setwd("Div/")
CC1RTr <- read.csv("CASE_COV_select_wNA_1p1r_Train.csv")
CC1RTr <- C1RTr[,-c(1,2)]#419*18

CC1RTe <- read.csv("CASE_COV_select_wNA_1p1r_Test.csv")
CC1RTe <- C1RTe[,-c(1,2)]#105*18


CN1RTr <- read.csv("CASE_NOcov_hos_TrainTest_1p1r_Train.csv")
CN1RTr <- CN1RTr[,-c(1,2)]
CN1RTe <- read.csv("CASE_NOcov_hos_TrainTest_1p1r_Test.csv")
CN1RTe <- CN1RTe[,-c(1,2)]


VC1RTr12 <- read.csv("Visit_COV_select_wNA_1p1r_visit1, 2.csv")
VC1RTe3 <- read.csv("Visit_COV_select_wNA_1p1r_visit3.csv")

VC1RTr1 <- read.csv("Visit_COV_select_wNA_1p1r_visit1.csv")
VC1RTe2 <- read.csv("Visit_COV_select_wNA_1p1r_visit2.csv")

VN1RTr12 <- read.csv("VISIT_NOcov_hos_V12TrainTest_1p1r_visit1, 2.csv")
VN1RTe3 <- read.csv("VISIT_NOcov_hos_V12TrainTest_1p1r_visit3.csv")

VN1RTr1 <- read.csv ("VISIT_NOcov_hos_V12TrainTest_1p1r_visit1.csv")
VN1RTe2 <- read.csv("VISIT_NOcov_hos_V12TrainTest_1p1r_visit2.csv")

#set variable type
