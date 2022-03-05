source("DataProcFunctions.r")
source("ModelBuildFunction.r")
library(stringr)
#
#### setting parameter ####
file <- "TCHCData/hbp_dip_byx.csv"
CovAS <- "TCHCData/Cov_AgeSex.csv"
CovD <- "TCHCData/Cov_Drug.csv"
CovHH <- "TCHCData/Cov_HbHR.csv" 
#for time split
reCol.AM <- c("HBP_d_PM_systolic","HBP_d_PM_diastolic")
reCol.PM <- c("HBP_d_AM_systolic","HBP_d_AM_diastolic")

aab <- read.csv(file)
#### data processing ####
result.22 <- myRead(file, removeNa = T, category = c("Non dipper", "Reverse dipper"),
                 newVar = "dip")
a <- result.22$myData
dim(a)
table(a$visit_HBP_Dmode,a$dip)

#### add cov ####
aaa<- result.22$myData
addCov.22 <- PlusCov(data = result.22$myData, Covlist = CovAS ,
                                 IDname = "MRN", Cov = c("Gender","Age"),
                                 Yname = "dip")
addCov.22.1 <- PlusCov(data = addCov.22$AddCov.df, Covlist = CovHH ,
                     IDname = "Mrn_Vis",Cov = c("HbA1C","HR"),
                     Yname = "dip")
addCov.22.2 <- PlusCov(data = addCov.22.1$AddCov.df, Covlist = CovD ,
                       IDname = "Mrn_Vis", Cov = c("CCB"),
                       Yname = "dip")
A <- addCov.22.2$AddCov.df #761 * 14
table(A$visit_HBP_Dmode,A$dip)

#### cov imputation & na plot ####
df <- addCov.22.2$AddCov.df
tmp <- df[,c(1,2,8)]
library(visdat)
library(ggplot2)
vis_miss(df, show_perc = F) + coord_flip()
library(missForest)
df <- df[,-c(1,2,8)]
df$CCB <- as.factor(df$CCB)
df$HBP_d_AM_systolic <- as.numeric(df$HBP_d_AM_systolic)
df$HBP_d_PM_systolic <- as.numeric(df$HBP_d_PM_systolic)
df$HBP_d_AM_diastolic <- as.numeric(df$HBP_d_AM_diastolic)
df$HBP_d_PM_diastolic <- as.numeric(df$HBP_d_PM_diastolic)
RF.impute.HH <- missForest(df,verbose=T)
rfimp <- RF.impute.HH$ximp
rfimp.proc <- cbind(tmp,rfimp)#761 * 14




######### WITH COV ########
#### time split ####
timeSplit.cov <- timeSplit(data = rfimp.proc, 
                           removeCol.AM = reCol.AM,
                           removeCol.PM = reCol.PM)#1522 * 13
#### V12 V3 ####
cov.V12V3 <- TrainTest(data = timeSplit.cov, VisitOrCase = "Visit", nfixed = T, Train = 1:2,
                        Test = 3, seed = 123, removeCategory = NULL, Trainper = 0.8)

covTrain.V12V3 <- cov.V12V3$`Training set`
covTest.V12V3 <- cov.V12V3$`Test set`
covTrain.V12V3_uncomplete <- covTrain.V12V3 %>% group_by(MRN) %>% filter(n()!=4)
covTrain.V12V3 <- covTrain.V12V3[-which(covTrain.V12V3$MRN %in% covTrain.V12V3_uncomplete$MRN),]
covTest.V12V3 <- covTest.V12V3[-which(covTest.V12V3$MRN %in% covTrain.V12V3_uncomplete$MRN),]
#### Model Building ####
cov.V12Train <- covTrain.V12V3#284
cov.V3Test <- covTest.V12V3#142
write.csv(cov.V12Train,"TCHCData/cov_V12Train.csv")
write.csv(cov.V3Test,"TCHCData/cov_V3Test.csv")
#### random = "+(1|MRN)" ####
covV12V3  <- BiMMforest1(traindata = cov.V12Train, testdata = cov.V3Test,
                        formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "maxfun")
covV12V3$`model summary`
covV12V3$`CM of Train data`
covV12V3$`Train acc sen spe`#0.9647887 0.9821429 0.9
covV12V3$`CM of Test data`
covV12V3$`Test acc sen spe`#0.7394366 0.9245283 0.1944444

tryy  <- BiMMforest1(traindata = cov.V12Train, testdata = cov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "maxfun")
aa <- as.data.frame(tryy$test.preds)
table(cov.V12Train$dip)
table(cov.V3Test$dip)
table(aa$`tryy$test.preds`)

covV12V3.H1<-BiMMforestH1(traindata = cov.V12Train, testdata = cov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")
covV12V3.H1$`model summary`
covV12V3.H1$`CM of Train data`
covV12V3.H1$`Train acc sen spe`#0.9119718 0.9776786 0.6666667
covV12V3.H1$`CM of Test data`
covV12V3.H1$`Test acc sen spe`#0.73943662 0.97169811 0.05555556

covV12V3.H1.max<-BiMMforestH1(traindata = cov.V12Train, testdata = cov.V3Test,
                          formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                          random = "+(1|MRN)",
                          seed = 123, glmControl = "maxfun")
covV12V3.H1.max$iter
covV12V3.H1.max$`run time`
covV12V3.H1.max$`model summary`
covV12V3.H1.max$`CM of Train data`
covV12V3.H1.max$`Train acc sen spe`#0.9119718 0.9776786 0.6666667
covV12V3.H1.max$`CM of Test data`
covV12V3.H1.max$`Test acc sen spe`#0.73943662 0.97169811 0.05555556

covV12V3.H3 <- BiMMforestH3(traindata = cov.V12Train, testdata = cov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")

covV12V3.H3$iter
covV12V3.H3$`model summary`
covV12V3.H3$`Train acc sen spe`#0.9718310 0.9821429 0.9333333
covV12V3.H3$`Test acc sen spe`#0.6901408 0.8301887 0.2777778
covV12V3.H3$`CM of Train data`
covV12V3.H3$`CM of Test data`

try <- BiMMforestH3(traindata = cov.V12Train, testdata = cov.V3Test,
                   formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                   random = "+(1|MRN)",
                   seed = 123, glmControl = "maxfun")
try$iter
try$`model summary`
try$`CM of Train data`
try$`Train acc sen spe`#0.9718310 0.9821429 0.9333333
try$`CM of Test data`
try$`Test acc sen spe`#0.7183099 0.8679245 0.2777778

covV12V3.H2 <- BiMMforestH2(traindata = cov.V12Train, testdata = cov.V3Test,
                           formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                           random = "+(1|MRN)",
                           seed = 123, glmControl = "maxfun")
covV12V3.H2$iter
covV12V3.H2$`run time`
covV12V3.H2$`model summary`
covV12V3.H2$`Train acc sen spe`#0.9718310 0.9821429 0.9333333
covV12V3.H2$`Test acc sen spe`#0.7183099 0.8679245 0.2777778
covV12V3.H2$`CM of Train data`
covV12V3.H2$`CM of Test data`

#### V1 V2 樣本與V12V3相同####
#V12V3
cov.V12V3 <- TrainTest(data = timeSplit.cov, VisitOrCase = "Visit", nfixed = T, Train = 1:2,
                       Test = 3, seed = 123, removeCategory = NULL, Trainper = 0.8)

covTrain.V12V3 <- cov.V12V3$`Training set`
covTest.V12V3 <-cov.V12V3$`Test set`
covTrain.V12V3_uncomplete <- covTrain.V12V3 %>% group_by(MRN) %>% filter(n()!=4)
covTrain.V12V3 <- covTrain.V12V3[-which(covTrain.V12V3$MRN %in% covTrain.V12V3_uncomplete$MRN),]
covTest.V12V3 <- covTest.V12V3[-which(covTest.V12V3$MRN %in% covTrain.V12V3_uncomplete$MRN),]
#接續上面做V1V2
covTrain.V1V2 <- covTrain.V12V3[which(covTrain.V12V3$visit==1),]#142
covTest.V1V2 <- covTrain.V12V3[which(covTrain.V12V3$visit==2),]#142
#### Data scale (standardization) ####
ds.covTrain.V1V2 <- DataScale(data = covTrain.V1V2,NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"))
ds.covTest.V1V2 <- DataScale(data = covTest.V1V2,NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"))
#### Model Building ####
cov.V1Train <-ds.covTrain.V1V2$scale.df #142
cov.V2Test <- ds.covTest.V1V2$scale.df#142
#### random = "+(1|MRN)" ####
table(cov.V1Train$dip)
table(cov.V2Test$dip)
table(cov.V3Test$dip)
covV1V2  <- BiMMforest1(traindata = cov.V1Train, testdata = cov.V2Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")
covV1V2$`model summary`
covV1V2$`CM of Train data`
covV1V2$`Train acc sen spe`#1 1 1
covV1V2$`CM of Test data`
covV1V2$`Test acc sen spe`#0.7746479 0.9818182 0.0625


covV1V2.H1 <- BiMMforestH1(traindata = cov.V1Train, testdata = cov.V2Test,
                          formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                          random = "+(1|MRN)",
                          seed = 123, glmControl = "tolPwrss")
covV1V2.H1$`model summary`
covV1V2.H1$`CM of Train data`
covV1V2.H1$`Train acc sen spe`#1 1 1 #資料標準化後 有error
covV1V2.H1$`CM of Test data`
covV1V2.H1$`Test acc sen spe`#0.7746479 1 0

covV1V2.H3 <- BiMMforestH3(traindata = cov.V1Train, testdata = cov.V2Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")
covV1V2.H3$iter
covV1V2.H3$`model summary`
covV1V2.H3$`Train acc sen spe`#1 1 1
covV1V2.H3$`Test acc sen spe`#0.7464789 0.9090909 0.1875
covV1V2.H3$`CM of Train data`
covV1V2.H3$`CM of Test data`

tryV1V2.H3 <- BiMMforestH3(traindata = cov.V1Train, testdata = cov.V2Test,
                    formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                    random = "+(1|MRN)",
                    seed = 123, glmControl = "maxfun")
#"GLMM did not converge or all of the outcomes are the same"


covV1V2.H2 <- BiMMforestH2(traindata = cov.V1Train, testdata = cov.V2Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")
covV1V2.H2$iter
covV1V2.H2$`run time`
covV1V2.H2$`model summary`
covV1V2.H2$`Train acc sen spe`#1 1 1
covV1V2.H2$`Test acc sen spe`# 0.7464789 0.9090909 0.1875
covV1V2.H2$`CM of Train data`
covV1V2.H2$`CM of Test data`



######### NO COV ########
##### timeSplit裡有把sys dia轉成數值 ####
timeSplit.22 <- timeSplit(data = result.22$myData, 
                      removeCol.AM = reCol.AM,
                      removeCol.PM = reCol.PM)
table(timeSplit.22$visit)
#### V1 V2####
TrainTest.22 <- TrainTest(data = timeSplit.22, VisitOrCase = "Visit", nfixed = T, Train = 1,
                      Test = 2, seed = NULL, removeCategory = NULL, Trainper = 0.8)
####MRN有沒有改成factor結果一樣####
Train.22 <- TypeChange(data = TrainTest.22$`Training set`, variable = "MRN", type = "factor")
Test.22 <- TypeChange(data = TrainTest.22$`Test set`, variable = "MRN", type = "factor")


#### model Building ####
V1Train <- Train.22 #276
V2Test <- Test.22 #276
#### random = "+(1+time|sys)+(1+time|dia)" ####
V1V2 <- BiMMforest1(traindata = V1Train, testdata = V2Test,
                  formula = dip ~ sys+dia+time,
                  random = "+(1+time|sys)+(1+time|dia)",
                  seed = 123)
V1V2$`CM of Train data`
V1V2$`CM of Test data`
V1V2$`Train acc sen spe`# 0.8405797 1.0000000 0.3333333
V1V2$`Test acc sen spe`# 0.7536232 1.0000000 0.0000000
V1V2$`model summary`

H1 <- BiMMforestH1(traindata = V1Train, testdata = V2Test,
                    formula = dip ~ sys+dia+time,
                    random = "+(1+time|sys)+(1+time|dia)",
                    seed = 123)
#"all of the binary outcomes are the same"

H3 <- BiMMforestH3(traindata = V1Train, testdata = V2Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1+time|sys)+(1+time|dia)",
                   seed = 123, glmControl = "maxfun")
H3$iter
H3$`model summary`
H3$`Train acc sen spe`#0.8405797 1 0.3333333 same as V1V2
H3$`Test acc sen spe`#0.7536232 1 0 same as V1V2
H3$`CM of Train data`
H3$`CM of Test data`

H2 <- BiMMforestH2(traindata = V1Train, testdata = V2Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1+time|sys)+(1+time|dia)",
                   seed = 123, glmControl = "maxfun")
H2$iter
H2$`model summary`
H2$`Train acc sen spe`#0.8405797 1 0.3333333 same as V1V2
H2$`Test acc sen spe`#0.7536232 1 0 same as V1V2
H2$`CM of Train data`
H2$`CM of Test data`


#### random = "+(1|MRN)" ####
try <- BiMMforest1(traindata = V1Train, testdata = V2Test,
                  formula = dip ~ sys+dia+time,
                  random = "+(1|MRN)",
                  seed = 123)
try$`model summary`
try$`CM of Train data`
try$`Train acc sen spe`#1 1 1
try$`CM of Test data`
try$`Test acc sen spe`#same as V1V2


V1V2H1<-BiMMforestH1(traindata = V1Train, testdata = V2Test,
               formula = dip ~ sys+dia+time,
               random = "+(1+time|MRN)",
               seed = 123)
#"all of the binary outcomes are the same"

V1V2H3<-BiMMforestH3(traindata = V1Train, testdata = V2Test,
                     formula = dip ~ sys+dia+time,
                     random = "+(1|MRN)",
                     seed = 123)
V1V2H3$iter
V1V2H3$`Train acc sen spe`#1 1 1
V1V2H3$`Test acc sen spe`#0.7536232 1 0 same as V1V2
V1V2H3$`CM of Train data`
V1V2H3$`CM of Test data`

V1V2H2 <- BiMMforestH2(traindata = V1Train, testdata = V2Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1|MRN)",
                   seed = 123)
V1V2H2$iter
V1V2H2$`model summary`
V1V2H2$`Train acc sen spe`# 1 1 1
V1V2H2$`Test acc sen spe`#0.7536232 1 0 same as V1V2
V1V2H2$`CM of Train data`
V1V2H2$`CM of Test data`

#### V12 V3####
TT.22.V12V3<- TrainTest(data = timeSplit.22, VisitOrCase = "Visit", nfixed = T, Train = 1:2,
                        Test = 3, seed = NULL, removeCategory = NULL, Trainper = 0.8)

Train.V12V3 <- TT.22.V12V3$`Training set`
Test.V12V3 <-TT.22.V12V3$`Test set`
Train.V12V3_uncomplete <- Train.V12V3 %>% group_by(MRN) %>% filter(n()!=4)
Train.V12V3 <- Train.V12V3[-which(Train.V12V3$MRN %in% Train.V12V3_uncomplete$MRN),]
Test.V12V3 <- Test.V12V3[-which(Test.V12V3$MRN %in% Train.V12V3_uncomplete$MRN),]
#### model Building ####
V12Train <- Train.V12V3 #324
V3Test <- Test.V12V3 #162
#### random = "+(1+time|sys)+(1+time|dia)" ####
V12V3 <- BiMMforest1(traindata = V12Train, testdata = V3Test,
                    formula = dip ~ sys+dia+time,
                    random = "+(1+time|sys)+(1+time|dia)",
                    seed = 123)
V12V3$`CM of Train data`
V12V3$`CM of Test data`
V12V3$`Train acc sen spe`# 0.7839506 1 0 
V12V3$`Test acc sen spe`# 0.7283951 1.0000000 0.0000000
V12V3$`model summary`


V12V3H1 <- BiMMforestH1(traindata = V12Train, testdata = V3Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1+time|sys)+(1+time|dia)",
                   seed = 123)
#"all of the binary outcomes are the same"

V12V3H3 <- BiMMforestH3(traindata = V12Train, testdata = V3Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1+time|sys)+(1+time|dia)",
                   seed = 123)
V12V3H3$iter
V12V3H3$`Train acc sen spe`#0.7839506 1 0  same as V12V3
V12V3H3$`Test acc sen spe`#0.7283951 1.0000000 0.0000000
V12V3H3$`CM of Train data`
V12V3H3$`CM of Test data`

V12V3H2 <- BiMMforestH2(traindata = V12Train, testdata = V3Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1+time|sys)+(1+time|dia)",
                   seed = 123)
V12V3H2$iter
V12V3H2$`model summary`
V12V3H2$`Train acc sen spe`#0.7839506 1 0  same as V12V3
V12V3H2$`Test acc sen spe`#0.7536232 1 0 same as V1V2
V12V3H2$`CM of Train data`
V12V3H2$`CM of Test data`


#### random = "+(1|MRN)" ####

V12V3.1 <- BiMMforest1(traindata = V12Train, testdata = V3Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1|MRN)",
                   seed = 123)
V12V3.1$`model summary`
V12V3.1$`CM of Train data`
V12V3.1$`Train acc sen spe`#0.9043210 0.9527559 0.7285714
V12V3.1$`CM of Test data`
V12V3.1$`Test acc sen spe`#0.7283951 1.0000000 0.0000000


V12V3.H1<-BiMMforestH1(traindata = V12Train, testdata = V3Test,
                     formula = dip ~ sys+dia+time,
                     random = "+(1|MRN)",
                     seed = 123)
#"all of the binary outcomes are the same"

V12V3.H3<-BiMMforestH3(traindata = V12Train, testdata = V3Test,
                     formula = dip ~ sys+dia+time,
                     random = "+(1|MRN)",
                     seed = 123)
V12V3.H3$iter
V12V3.H3$`Train acc sen spe`#0.9043210 0.9527559 0.7285714
V12V3.H3$`Test acc sen spe`#0.7283951 1.0000000 0.0000000
V12V3.H3$`CM of Train data`
V12V3.H3$`CM of Test data`

V12V3.H2 <- BiMMforestH2(traindata = V12Train, testdata = V3Test,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123)
V12V3.H2$iter
V12V3.H2$`model summary`
V12V3.H2$`Train acc sen spe`# 0.9043210 0.9527559 0.7285714
V12V3.H2$`Test acc sen spe`#0.7283951 1 0 same as H3 1iter
V12V3.H2$`CM of Train data`
V12V3.H2$`CM of Test data`

#### 預測new cases #### paper已經說比較不好 就不用做

#### V1 V2 樣本與V12V3相同####
#V12V3
TT.22.V12V3<- TrainTest(data = timeSplit.22, VisitOrCase = "Visit", nfixed = T, Train = 1:2,
                        Test = 3, seed = NULL, removeCategory = NULL, Trainper = 0.8)

Train.V12V3 <- TT.22.V12V3$`Training set`
Test.V12V3 <-TT.22.V12V3$`Test set`
Train.V12V3_uncomplete <- Train.V12V3 %>% group_by(MRN) %>% filter(n()!=4)
Train.V12V3 <- Train.V12V3[-which(Train.V12V3$MRN %in% Train.V12V3_uncomplete$MRN),]#324
Test.V12V3 <- Test.V12V3[-which(Test.V12V3$MRN %in% Train.V12V3_uncomplete$MRN),]#162
#從V12V3接續
Train.V1V2 <- Train.V12V3[which(Train.V12V3$visit==1),]#162
Test.V1V2 <- Train.V12V3[which(Train.V12V3$visit==2),]#162
#### model Building ####
V1Train.V3 <- Train.V1V2 
V2Test.V3 <- Test.V1V2
#### random = "+(1|MRN)" ####

V1V2.V3.1 <- BiMMforest1(traindata = V1Train.V3, testdata = V2Test.V3,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123)
V1V2.V3.1$`model summary`
V1V2.V3.1$`CM of Train data`
V1V2.V3.1$`Train acc sen spe`#1 1 1
V1V2.V3.1$`CM of Test data`
V1V2.V3.1$`Test acc sen spe`#0.753 1 0


V1V2.V3.H1<-BiMMforestH1(traindata = V1Train.V3, testdata = V2Test.V3,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123)
#"all of the binary outcomes are the same"

V1V2.V3.H3<-BiMMforestH3(traindata = V1Train.V3, testdata = V2Test.V3,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123, glmControl = "tolPwrss")
V1V2.V3.H3$iter
V1V2.V3.H3$`model summary`
V1V2.V3.H3$`Train acc sen spe`#1 1 1
V1V2.V3.H3$`Test acc sen spe`#0.753 1 0
V1V2.V3.H3$`CM of Train data`
V1V2.V3.H3$`CM of Test data`

V1V2.V3.H2 <- BiMMforestH2(traindata = V1Train.V3, testdata = V2Test.V3,
                         formula = dip ~ sys+dia+time,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")
V1V2.V3.H2$iter
V1V2.V3.H2$`model summary`
V1V2.V3.H2$`Train acc sen spe`#1 1 1
V1V2.V3.H2$`Test acc sen spe`#0.753 1 0
V1V2.V3.H2$`CM of Train data`
V1V2.V3.H2$`CM of Test data`


#### 測試sys dia time 22交互作用####
#### V12 V3 data ####
InterV12V3 <- Interact(data1 = Train.V12V3, data2 = Test.V12V3,
                       formula = dip ~ sys + dia*time + (1|MRN), scaleCol = c(4,5), seed = 123)
InterV12V3$`model summary`# dia*time 0.93 不顯著

InterV12V3.1 <- Interact(data1 = Train.V12V3, data2 = Test.V12V3,
                         formula = dip ~ sys*time +dia + (1|MRN),scaleCol = c(4,5), seed = 123)
InterV12V3.1$`model summary`#sys*time 0.86 不顯著

InterV12V3.2 <- Interact(data1 = Train.V12V3, data2 = Test.V12V3,
                         formula = dip ~ sys*dia + time + (1|MRN),scaleCol = c(4,5), seed = 123)
InterV12V3.2$`model summary`#sys*dia 0.13 不顯著
#### V1V2 data ####
InterV12 <- Interact(data1 = Train.V1V2, data2 = Test.V1V2,
                       formula = dip ~ sys + dia*time + (1|MRN), scaleCol = c(4,5), seed = 123)
InterV12$`model summary`# dia*time 0.87 不顯著

InterV12.1 <- Interact(data1 = Train.V1V2, data2 = Test.V1V2,
                     formula = dip ~ dia + sys*time + (1|MRN), scaleCol = c(4,5), seed = 123)
InterV12.1$`model summary`#sys*time 0.78 不顯著

InterV12.2 <- Interact(data1 = Train.V1V2, data2 = Test.V1V2,
                     formula = dip ~ time + dia*sys + (1|MRN), scaleCol = c(4,5), seed = 123)
InterV12.2$`model summary`#dia*sys顯著 p-value = 0.0494














#####小結####



####對醫院做校正####
#V1 -V3 的樣本 n=486
Hospital.ad1 <- rbind(Train.V12V3,Test.V12V3)
table(Hospital.ad1$visit)
#取MRN 先去掉重複 再撈英文(醫院名) 看個醫院的
Hos <- as.data.frame(Hospital.ad1$MRN)
Hos <- unique(Hos)
colnames(Hos)[1] <- "MRN"
Hos["Hospital"] <- str_extract(Hos$MRN,"[A-Z]+")
table(Hos$Hospital)
#CGMHLK 林口長庚 45 #CMUH 中國附醫 1
#EDA 義大 3 #KMUH 高醫 32
#分成北和中南進行校正 HOW??

#### 對樣本下降做隨機性檢定 ####

