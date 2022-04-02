source("Train_Test_file_cov_select.r")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#CCTr,CCTe,CNTr,CNTe
F1 <- dip~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia
F2 <- dip~sbp+dbp+time+HOS
seed = 123


CC.1 <- BiMMforest1(traindata = CCTr, testdata = CCTe,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
CC.1$`model summary`  
CC.1$`CM of Train data`
CC.1$`Train acc sen spe`# 0.9892601 0.9952978 0.9700000
CC.1$`CM of Test data`
CC.1$`Test acc sen spe`#0.80952381 1.00000000 0.04761905
CC.1$`lme.CM of Test data`
CC.1$`lme.Test acc sen spe`#0.6809524 0.7500000 0.4047619

any(is.na(CCTr))
any(is.na(CCTe))
CC.H1 <- BiMMforestH1(traindata = CCTr, testdata = CCTe,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
CC.H1$iter#1000
CC.H1$RF#OB estimate of  error rate: 3.82%
CC.H1$`model summary`
CC.H1$`CM of Train data`
CC.H1$`Train acc sen spe`# 0.9749403 0.9968652 0.9050000
CC.H1$`CM of Test data`
CC.H1$`Test acc sen spe`#0.80952381 1.00000000 0.04761905
CC.H1$`lme.CM of Test data`
CC.H1$`lme.Test acc sen spe`#0.6857143 0.7857143 0.2857143
CC.H1$`RF OOBerror`

CC.H2 <- BiMMforestH2(traindata = CCTr, testdata = CCTe,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
CC.H2$iter#1000
CC.H2$RF#OOB estimate of  error rate: 4.53%
CC.H2$`model summary`
CC.H2$`CM of Train data`
CC.H2$`Train acc sen spe`# 0.9844869 0.9890282 0.9700000
CC.H2$`CM of Test data`
CC.H2$`Test acc sen spe`#0.78571429 0.97023810 0.04761905
CC.H2$`lme.CM of Test data`
CC.H2$`lme.Test acc sen spe`#0.6714286 0.7440476 0.3809524
CC.H2$`RF OOBerror`

CC.H3 <- BiMMforestH3(traindata = CCTr, testdata = CCTe,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")

CC.H3$iter#100
CC.H3$RF#OB estimate of  error rate: 5.49%
CC.H3$`model summary`
CC.H3$`CM of Train data`
CC.H3$`Train acc sen spe`# 0.9844869 0.9905956 0.9650000
CC.H3$`CM of Test data`
CC.H3$`Test acc sen spe`#0.80000000 0.98809524 0.04761905
CC.H3$`lme.CM of Test data`
CC.H3$`lme.Test acc sen spe`#0.6857143 0.7559524 0.4047619
CC.H3$`RF OOBerror`

CC.rf <- RF(traindata = CCTr, testdata = CCTe,
                  formula = F1, classwt = NULL,seed = seed)
CC.rf$`CM of Train data`
CC.rf$`Train acc sen spe`#0.9057279 1.0000000 0.5863874
CC.rf$`CM of Test data`
CC.rf$`Test acc sen spe`#0.9095238 1.0000000 0.6274510
CC.rf$`Var importance`
### 找mtry
library(caret)
#for RF only mtry can be tuned by caret
#reasons :its effect on the final accuracy and that it must be found empirically for a dataset.
# Random Search
seed = 123
x <- CCTr
dataset = CCTr
metric <- "Accuracy"
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3,
                        search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(factor(dip)~., data=dataset,
                   method="rf", metric=metric,
                   tuneLength=15, trControl=control)
print(rf_random)#最好的mtry = 
plot(rf_random)

#Grid Search
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3,
                        search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(factor(dip)~., data=dataset,
                       method="rf", metric=metric,
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)#最好的mtry =
plot(rf_gridsearch)
###

CN.1 <- BiMMforest1(traindata = CNTr, testdata = CNTe,
                    formula = F2, random = "+(1|time)",
                    seed = seed, glmControl = "maxfun")
CN.1$`model summary`
CN.1$`CM of Train data`
CN.1$`Train acc sen spe`# 1 1 1
CN.1$`CM of Test data`
CN.1$`Test acc sen spe`#0.71621622 0.92352941 0.03846154
CN.1$`lme.CM of Test data`
CN.1$`lme.Test acc sen spe`# 0.765 1 0

CN.H1 <- BiMMforestH1(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")

CN.H2 <- BiMMforestH2(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")

CN.H3 <- BiMMforestH3(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")

CN.rf <- RF(traindata = CNTr, testdata = CNTe,
            formula = F2, classwt = NULL,seed = seed)
CN.rf$`CM of Train data`
CN.rf$`Train acc sen spe`
CN.rf$`CM of Test data`
CN.rf$`Test acc sen spe`
CN.rf$`Var importance`
