CCTr <- read.csv("TCHCData/CASE_COV_hos_Train.csv")#838
CCTe <- read.csv("TCHCData/CASE_COV_hos_Test.csv")#210
CNTr <- read.csv("TCHCData/CASE_NOcov_hos_Train.csv")#880
CNTe <- read.csv("TCHCData/CASE_NOcov_hos_Test.csv")#222

VNTr.12 <- read.csv("TCHCData/VISIT_NOcov_hos_V12Train.csv")#324
VNTe.3 <- read.csv("TCHCData/VISIT_NOcov_hos_V3Test.csv")#162
VNTr.1 <- read.csv("TCHCData/VISIT_NOcov_hos_V1Train.csv")#162
VNTe.2 <- read.csv("TCHCData/VISIT_NOcov_hos_V2Test.csv")#162

VCTr.12 <- read.csv("TCHCData/VISIT_COV_hos_V12Train.csv")#284
VCTe.3 <- read.csv("TCHCData/VISIT_COV_hos_V3Test.csv")#142
VCTr.1 <- read.csv("TCHCData/VISIT_COV_hos_V1Train.csv")#142
VCTe.2 <- read.csv("TCHCData/VISIT_COV_hos_V2Test.csv")#284





#other file
#for testing imputation consistency
cmplt <- "TCHCData/COVchange_cmplt_data.csv"
rfimp <- "TCHCData/RFimp.csv "