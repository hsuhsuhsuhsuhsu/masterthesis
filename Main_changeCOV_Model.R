#糖尿病和用藥變數 換過之後 重新跑
#set source
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
source("Train_Test_file_check.r")
#set parameter
formula1 = dip ~ sbp+dbp+time+Gender+Age+HR+Drug_conut+DM+visit+HOS
formula2 = dip ~ sbp+dbp+time+visit+HOS
seed = 123
####data read####
colnames(CCTr)
data <- list(a = CCTr, b = CCTe, c = CNTr,
             d = CNTe, e = VNTr.12, f = VNTe.3,
             g = VNTr.1, h = VNTe.2, i = VCTr.12,
             j = VCTe.3, k = VCTr.1, l = VCTe.2 )
#### model Build ####
########### CASE ############
########### NO COV ###########

########## With COV ###########
#
########### VISIT ############
########### NO COV ###########
########### V12 V3 ###########
########### V1 V2 ###########
########## With COV ###########
########### V12 V3 ###########
VC.12.3  <- BiMMforest1(traindata = VCTr.12, testdata = VCTe.3,
                         formula = formula1,
                         random = "+(1|MRN)",
                         seed = seed, glmControl = "maxfun")
VC.12.3$`model summary`
VC.12.3$`CM of Train data`
VC.12.3$`Train acc sen spe`#0.9507042 0.9687500 0.8833333
VC.12.3$`CM of Test data`
VC.12.3$`Test acc sen spe`#0.7605634 0.9339623 0.2500000
VC.12.3$`lme.CM of Test data`
VC.12.3$`lme.Test acc sen spe`#0.69014085 0.90566038 0.05555556
########### V1 V2 ###########
VC.1.2  <- BiMMforest1(traindata = VCTr.1, testdata = VCTe.2,
                        formula = formula1,
                        random = "+(1|MRN)",
                        seed = seed, glmControl = "maxfun")

#####TEMP ####
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