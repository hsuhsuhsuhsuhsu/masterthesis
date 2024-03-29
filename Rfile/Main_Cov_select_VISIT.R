source("Train_Test_file_cov_select.r")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#VNTr.12 VNTe.3 VNTr.1 VNTe.2
#VCTr.12 VCTe.3 VCTr.1 VCTe.2
F5 <- dip~visit+sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Eco_child+T_pain
F6 <- dip~visit+sbp+dbp+time+HOS
F7 <- factor(dip)~visit+sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Eco_child+T_pain
F8 <- factor(dip)~visit+sbp+dbp+time+HOS
F9 <- dip~sbp+dbp+time+HOS
F10 <- factor(dip)~sbp+dbp+time+HOS
F11 <- dip~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Eco_child+T_pain

seed = 123

VC3 <- BiMMforest1(traindata = VCTr.12, testdata = VCTe.3,
                  formula = F5, random = "+(1|MRN)",
                  seed = seed, glmControl = "maxfun")
VC3$RF#OOB estimate of  error rate: 8.8%
VC3$`model summary`
VC3$`CM of Train data`
VC3$`Train acc sen spe`#0.9260563 0.9553571 0.8166667
VC3$`CM of Test data`
VC3$`Test acc sen spe`#0.7887324 0.9245283 0.3888889
VC3$`lme.CM of Test data`
VC3$`lme.Test acc sen spe`#0.6478873 0.8207547 0.1388889

# VVC3 <- BiMMforest1(traindata = VCTr.12, testdata = VCTe.3,
#                    formula = F5, random = "+(1+time|MRN)",
#                    seed = seed, glmControl = "maxfun")
# VVC3$RF
# VVC3$`model summary`
# VVC3$`CM of Train data`
# VVC3$`Train acc sen spe`
# VVC3$`Test acc sen spe`

VC3.H1 <- BiMMforestH1(traindata = VCTr.12, testdata = VCTe.3,
                       formula = F5, random = "+(1|MRN)",
                       seed = seed, glmControl = "maxfun")

VC3.H1$RF
VC3.H1$`model summary`
VC3.H1$`CM of Train data`
VC3.H1$`Train acc sen spe`#0.9119718 0.9821429 0.6500000
VC3.H1$`CM of Test data`
VC3.H1$`Test acc sen spe`#0.7323944 0.9245283 0.1666667
VC3.H1$`lme.CM of Test data`
VC3.H1$`lme.Test acc sen spe`#0.69014085 0.89622642 0.08333333


VC3.H2 <- BiMMforestH2(traindata = VCTr.12, testdata = VCTe.3,
                   formula = F5, random = "+(1|MRN)",
                   seed = seed, glmControl = "maxfun")
VC3.H2$RF#OOB estimate of  error rate: 7.39%
VC3.H2$`model summary`
VC3.H2$`CM of Train data`
VC3.H2$`Train acc sen spe`#0.9436620 0.9598214 0.8833333
VC3.H2$`CM of Test data`
VC3.H2$`Test acc sen spe`#0.7535211 0.8773585 0.3888889
VC3.H2$`lme.CM of Test data`
VC3.H2$`lme.Test acc sen spe`#0.65492958 0.84905660 0.08333333



VC3.H3 <- BiMMforestH3(traindata = VCTr.12, testdata = VCTe.3,
                       formula = F5, random = "+(1|MRN)",
                       seed = seed, glmControl = "maxfun")
VC3.H3$RF#OOB estimate of  error rate: 7.39%
VC3.H3$`model summary`
VC3.H3$`CM of Train data`
VC3.H3$`Train acc sen spe`#0.9436620 0.9598214 0.8833333
VC3.H3$`CM of Test data`
VC3.H3$`Test acc sen spe`#0.7535211 0.8773585 0.3888889
VC3.H3$`lme.CM of Test data`
VC3.H3$`lme.Test acc sen spe`#0.65492958 0.84905660 0.08333333

VC3.RF <- RF(traindata = VCTr.12, testdata = VCTe.3,
             formula = F7, classwt = NULL,seed = seed)
VC3.RF$RF#OOB estimate of  error rate: 7.75%
VC3.RF$`CM of Train data`
VC3.RF$`Train acc sen spe`#0.9225352 0.9821429 0.7000000
VC3.RF$`CM of Test data`
VC3.RF$`Test acc sen spe`#0.7887324 0.9245283 0.3888889

library(caret)
#Grid Search
metric="accuracy"
dataset = VCTr.12
seed = 123
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3,
                        search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:14))
rf_gridsearch <- train(F7, data=dataset,
                       method="rf", metric=metric,
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)#最好的mtry =10
p1<- plot(rf_gridsearch)
gridExtra::marrangeGrob(list(p1), top = "",
                        nrow = 1, ncol = 1)

VC3.RF10 <- RF(traindata = VCTr.12, testdata = VCTe.3,
               formula = F7, classwt = NULL,seed = seed,mtry=10)
VC3.RF10$RF#OOB estimate of  error rate: 6.34%
VC3.RF10$`CM of Train data`
VC3.RF10$`Train acc sen spe`#0.9366197 0.9821429 0.7666667
VC3.RF10$`CM of Test data`
VC3.RF10$`Test acc sen spe`#0.7676056 0.8962264 0.3888889


VN3 <- BiMMforest1(traindata = VNTr.12, testdata = VNTe.3,
                   formula = F6, random = "+(1|MRN)",
                   seed = seed, glmControl = "maxfun")
VN3$RF#OOB estimate of  error rate: 23.46%
VN3$`model summary`
VN3$`CM of Train data`
VN3$`Train acc sen spe`#0.9012346 0.9842520 0.6000000
VN3$`CM of Test data`
VN3$`Test acc sen spe`#0.73456790 0.99152542 0.04545455
VN3$`lme.CM of Test data`
VN3$`lme.Test acc sen spe`#0.728 1 0

VN3.H1 <- BiMMforestH1(traindata = VNTr.12, testdata = VNTe.3,
                    formula = F6, random = "+(1|MRN)",
                    seed = seed, glmControl = "maxfun")
#"all of the binary outcomes are the same"


VN3.H2 <- BiMMforestH2(traindata = VNTr.12, testdata = VNTe.3,
                       formula = F6, random = "+(1|MRN)",
                       seed = seed, glmControl = "maxfun")
VN3.H2$iter#2
VN3.H2$RF
VN3.H2$`model summary`
VN3.H2$`CM of Train data`
VN3.H2$`Train acc sen spe`#0.9012346 0.9842520 0.6000000
VN3.H2$`CM of Test data`
VN3.H2$`Test acc sen spe`#0.73456790 0.99152542 0.04545455
VN3.H2$`lme.CM of Test data`
VN3.H2$`lme.Test acc sen spe`#0.7098765 0.8644068 0.2954545


VN3.H3 <- BiMMforestH3(traindata = VNTr.12, testdata = VNTe.3,
                       formula = F6, random = "+(1|MRN)",
                       seed = seed, glmControl = "maxfun")
VN3.H3$iter#2
VN3.H3$RF
VN3.H3$`model summary`
VN3.H3$`CM of Train data`
VN3.H3$`Train acc sen spe`#0.9012346 0.9842520 0.6000000
VN3.H3$`CM of Test data`
VN3.H3$`Test acc sen spe`#0.73456790 0.99152542 0.04545455
VN3.H3$`lme.CM of Test data`
VN3.H3$`lme.Test acc sen spe`#0.7098765 0.8644068 0.2954545

VN3.RF <- RF(traindata = VNTr.12, testdata = VNTe.3,
             formula = F8, classwt = NULL,seed = seed)
VN3.RF$RF
VN3.RF$`CM of Train data`
VN3.RF$`Train acc sen spe`#0.7345679 0.9094488 0.1000000
VN3.RF$`CM of Test data`
VN3.RF$`Test acc sen spe`#0.6975309 0.8559322 0.2727273



VC2ds <- DataScale(data = VCTr.1, NumVar = c("sbp","dbp","Age","BMI","HR","Waist"),
                      Yname = "dip")
I <- VC2ds$scale.df

VC2 <- BiMMforest1(traindata = VCTr.1 , testdata = VCTe.2 ,
                   formula = F5, random = "+(1|MRN)",
                   seed = seed, glmControl = "tolPwrss")
VC2$`model summary`
VC2$model
VC2$`CM of Train data`
VC2$`Train acc sen spe`#1 1 1
VC2$`CM of Test data`
VC2$`Test acc sen spe`# 0.7887324 0.9181818 0.3437500
VC2$`lme.CM of Test data`
VC2$`lme.Test acc sen spe`#0.6338028 0.7727273 0.1562500

dsVC2 <- BiMMforest1(traindata = I , testdata = VCTe.2 ,
                   formula = F5, random = "+(1|MRN)",
                   seed = seed, glmControl = "tolPwrss")
dsVC2$RF
dsVC2$`model summary`
dsVC2$`CM of Train data`
dsVC2$`Train acc sen spe`#1 1 1
dsVC2$`CM of Test data`
dsVC2$`Test acc sen spe`#0.8169014 0.9636364 0.3125000
dsVC2$`lme.CM of Test data`
dsVC2$`lme.Test acc sen spe`#0.6619718 0.8090909 0.1562500


VC2.H1 <- BiMMforestH1(traindata = VCTr.1 , testdata = VCTe.2 ,
                       formula = F5, random = "+(1|MRN)",
                       seed = seed, glmControl = "optimx")

VC2.H1$iter#2
VC2.H1$RF#OOB estimate of  error rate: 5.63%
VC2.H1$`model summary`
VC2.H1$`CM of Train data`
VC2.H1$`Train acc sen spe`#1 1 1
VC2.H1$`CM of Test data`
VC2.H1$`Test acc sen spe`#0.7887324 0.9181818 0.3437500
VC2.H1$`lme.CM of Test data`
VC2.H1$`lme.Test acc sen spe`# 0.6338028 0.7727273 0.1562500

dsVC2.H1 <- BiMMforestH1(traindata = I , testdata = VCTe.2 ,
                         formula = F5, random = "+(1|MRN)",
                         seed = seed, glmControl = "tolPwrss")
dsVC2.H1$iter
dsVC2.H1$RF
dsVC2.H1$`model summary`
dsVC2.H1$`CM of Train data`
dsVC2.H1$`Train acc sen spe`#1 1 1
dsVC2.H1$`CM of Test data`
dsVC2.H1$`Test acc sen spe`#0.8028169 0.9454545 0.3125000
dsVC2.H1$`lme.CM of Test data`
dsVC2.H1$`lme.Test acc sen spe`#0.6478873 0.7909091 0.1562500




VC2.H2 <- BiMMforestH2(traindata = VCTr.1 , testdata = VCTe.2 ,
                       formula = F5, random = "+(1|MRN)",
                       seed = seed, glmControl = "tolPwrss")
VC2.H2$iter#5
VC2.H2$RF#OOB estimate of  error rate: 3.52%
VC2.H2$`model summary`
VC2.H2$`CM of Train data`
VC2.H2$`Train acc sen spe`#1 1 1
VC2.H2$`CM of Test data`
VC2.H2$`Test acc sen spe`#0.7887324 0.9272727 0.3125000
VC2.H2$`lme.CM of Test data`
VC2.H2$`lme.Test acc sen spe`#0.6549296 0.8000000 0.1562500


dsVC2.H2 <- BiMMforestH2(traindata = I , testdata = VCTe.2 ,
                       formula = F5, random = "+(1|MRN)",
                       seed = seed, glmControl = "tolPwrss")

dsVC2.H2$RF
dsVC2.H2$iter
dsVC2.H2$`model summary`
dsVC2.H2$`CM of Train data`
dsVC2.H2$`Train acc sen spe`#1 1 1
dsVC2.H2$`CM of Test data`
dsVC2.H2$`Test acc sen spe`#0.8169014 0.9636364 0.3125000
dsVC2.H2$`lme.CM of Test data`
dsVC2.H2$`lme.Test acc sen spe`#0.6619718 0.8090909 0.1562500


VC2.H3 <- BiMMforestH3(traindata = VCTr.1 , testdata = VCTe.2 ,
                       formula = F5, random = "+(1|MRN)",
                       seed = seed, glmControl = "tolPwrss")
VC2.H3$iter#5
VC2.H3$RF#OOB estimate of  error rate: 3.52%
VC2.H3$`model summary`
VC2.H3$`CM of Train data`
VC2.H3$`Train acc sen spe`#1 1 1
VC2.H3$`CM of Test data`
VC2.H3$`Test acc sen spe`#0.7887324 0.9272727 0.3125000
VC2.H3$`lme.CM of Test data`
VC2.H3$`lme.Test acc sen spe`#0.6549296 0.8000000 0.1562500


dsVC2.H3 <- BiMMforestH3(traindata = I , testdata = VCTe.2 ,
                         formula = F5, random = "+(1|MRN)",
                         seed = seed, glmControl = "tolPwrss")
dsVC2.H3$iter#2
dsVC2.H3$RF#OOB estimate of  error rate: 4.93%
dsVC2.H3$`model summary`
dsVC2.H3$`CM of Train data`
dsVC2.H3$`Train acc sen spe`#1 1 1
dsVC2.H3$`CM of Test data`
dsVC2.H3$`Test acc sen spe`#0.8098592 0.9636364 0.2812500
dsVC2.H3$`lme.CM of Test data`
dsVC2.H3$`lme.Test acc sen spe`#0.6619718 0.8090909 0.1562500


VC2.RF <- RF(traindata = VCTr.1, testdata = VCTe.2,
             formula = F7, classwt = NULL,seed = seed)
VC2.RF$RF
VC2.RF$`CM of Train data`
VC2.RF$`Train acc sen spe`#0.9577465 1.0000000 0.7857143
VC2.RF$`CM of Test data`
VC2.RF$`Test acc sen spe`#0.7887324 0.9181818 0.3437500

dsVC2.RF <- RF(traindata = I, testdata = VCTe.2,
             formula = F7, classwt = NULL,seed = seed,mtry=12)

dsVC2.RF$`Train acc sen spe`#0.9788732 0.9912281 0.9285714
dsVC2.RF$`Test acc sen spe`#0.8028169 0.9454545 0.3125000


library(caret)
#Grid Search
metric="accuracy"
dataset = I
seed = 123
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3,
                        search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:14))
rf_gridsearch <- train(F7, data=dataset,
                       method="rf", metric=metric,
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)#最好的mtry 12
p1<- plot(rf_gridsearch)
gridExtra::marrangeGrob(list(p1), top = "",
                        nrow = 1, ncol = 1)





levels(VNTr.1$MRN)
levels(VNTe.2$MRN)
VN2 <- BiMMforest1(traindata = VNTr.1 , testdata = VNTe.2 ,
                   formula = F9, random = "+(1|MRN)",
                   seed = seed, glmControl = "tolPwrss")
VN2$RF
VN2$`model summary`
VN2$`CM of Train data`
VN2$`Train acc sen spe`#1 1 1
VN2$`Test acc sen spe`
VN2$`lme.CM of Test data`#0.7592593 0.9672131 0.1250000
VN2$`lme.Test acc sen spe`


VN2.H1 <- BiMMforestH1(traindata = VNTr.1 , testdata = VNTe.2 ,
                       formula = F9, random = "+(1|MRN)",
                       seed = seed, glmControl = "tolPwrss")
#"all of the binary outcomes are the same"

VN2.H2 <- BiMMforestH2(traindata = VNTr.1 , testdata = VNTe.2 ,
                       formula = F9, random = "+(1|MRN)",
                       seed = seed, glmControl = "tolPwrss")
VN2.H2$RF
VN2.H2$`model summary`
VN2.H2$`CM of Train data`#1 1 1
VN2.H2$`Train acc sen spe`
VN2.H2$`CM of Test data`
VN2.H2$`Test acc sen spe`#0.7592593 0.9672131 0.1250000



VN2.H3 <- BiMMforestH3(traindata = VNTr.1 , testdata = VNTe.2 ,
                       formula = F9, random = "+(1|MRN)",
                       seed = seed, glmControl = "tolPwrss")
VN2.H3$RF
VN2.H3$`model summary`
VN2.H3$`CM of Train data`
VN2.H3$`Train acc sen spe`#1 1 1
VN2.H3$`CM of Test data`
VN2.H3$`Test acc sen spe`#0.7592593 0.9672131 0.1250000



VN2.RF <- RF(traindata = VNTr.1, testdata = VNTe.2,
             formula = F10, classwt = NULL,seed = seed)

VN2.RF$`CM of Train data`
VN2.RF$`Train acc sen spe`#0.7901235 0.9318182 0.1666667
VN2.RF$`CM of Test data`
VN2.RF$`Test acc sen spe`#0.7407407 0.9262295 0.1750000
