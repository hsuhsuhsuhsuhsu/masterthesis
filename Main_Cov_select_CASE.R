source("Train_Test_file_cov_select.r")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#CCTr,CCTe,CNTr,CNTe
F1 <- dip~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia
F2 <- dip~sbp+dbp+time+HOS
seed = 123

CC.1 <- BiMMforest1(traindata = CCTr, testdata = CCTe,
                      formula = F1, random = "+(1|MRN)",
                      seed = seed, glmControl = "maxfun")
CC.1$`model summary`  
CC.1$`CM of Train data`
CC.1$`Train acc sen spe`
CC.1$`CM of Test data`
CC.1$`Test acc sen spe`
CC.1$`lme.CM of Test data`
CC.1$`lme.Test acc sen spe`



CC.rf <-RF(traindata = CCTr, testdata = CCTe,
                  formula = F1, classwt = NULL,seed = seed)
CC.rf$`CM of Train data`
CC.rf$`Train acc sen spe`
CC.rf$`CM of Test data`
CC.rf$`Test acc sen spe`
CC.rf$`Var importance`




CN.1 <- BiMMforest1(traindata = CNTr, testdata = CNTe,
                    formula = F2, random = "+(1|MRN)",
                    seed = seed, glmControl = "maxfun")
CN.1$`model summary`
CN.1$`CM of Train data`
CN.1$`Train acc sen spe`
CN.1$`CM of Test data`
CN.1$`Test acc sen spe`
CN.1$`lme.CM of Test data`
CN.1$`lme.Test acc sen spe`



CN.rf <- RF(traindata = CNTr, testdata = CNTe,
            formula = F2, classwt = NULL,seed = seed)
CN.rf$`CM of Train data`
CN.rf$`Train acc sen spe`
CN.rf$`CM of Test data`
CN.rf$`Test acc sen spe`
CN.rf$`Var importance`
