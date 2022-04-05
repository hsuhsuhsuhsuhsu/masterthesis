source("Train_Test_file_cov_select.r")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#VNTr.12 VNTe.3 VNTr.1 VNTe.2
#VCTr.12 VCTe.3 VCTr.1 VCTe.2
F5 <- dip~visit+sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Eco_child+T_pain
F6 <- dip~visit+sbp+dbp+time+HOS
F7 <- factor(dip)~visit+sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Eco_child+T_pain
F8 <- factor(dip)~visit+sbp+dbp+time+HOS
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



colnames(VCTr.1)
VCTR1 <- DataScale(data = VCTr.1, NumVar = c("sbp", "dbp", "Age", "BMI", "HR", "Waist"),
                      Yname = "dip")
tr <- VCTR1$scale.df
VCTE1 <- DataScale(data = VCTe.2, NumVar = c("sbp", "dbp", "Age", "BMI", "HR", "Waist"),
                   Yname = "dip")
te <- VCTE1$scale.df
View(tr)
VC2 <- BiMMforest1(traindata = tr, testdata = VCTe.2,
                   formula = F5, random = "+(1|MRN)",
                   seed = seed, glmControl = "optimx")
VC2$`model summary`
VC2$model
VC2$`Train acc sen spe`
#unable to evaluate scaled gradient
#Hessian is numerically singular: parameters are not uniquely determined


