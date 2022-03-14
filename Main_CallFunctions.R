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


#### data processing ####
result.22 <- myRead(file, removeNa = T, category = c("Non dipper", "Reverse dipper"),
                 newVar = "dip")
#### add cov ####

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
df$visit_HBP_Dmode <- as.factor(df$visit_HBP_Dmode)
df$CCB <- as.factor(df$CCB)
df$HBP_d_AM_systolic <- as.numeric(df$HBP_d_AM_systolic)
df$HBP_d_PM_systolic <- as.numeric(df$HBP_d_PM_systolic)
df$HBP_d_AM_diastolic <- as.numeric(df$HBP_d_AM_diastolic)
df$HBP_d_PM_diastolic <- as.numeric(df$HBP_d_PM_diastolic)
RF.impute.HH <- missForest(df,verbose=T)
rfimp <- RF.impute.HH$ximp
vis_miss(rfimp, show_perc = F) + coord_flip()
rfimp.proc <- cbind(tmp,rfimp)#761 * 14
write.csv(rfimp.proc,file = "TCHCData/RFimp.csv ")
rf <- read.csv("TCHCData/RFimp.csv ")


#


######### WITH COV ########
#### time split ####
RF.imp.ok <- read.csv("TCHCData/RFimp.csv ")
RF.imp.ok <- RF.imp.ok[,-1]
timeSplit.cov <- timeSplit(data = RF.imp.ok, 
                           removeCol.AM = reCol.AM,
                           removeCol.PM = reCol.PM)#1522 * 13
write.csv(timeSplit.cov,"TCHCData/yesCOVdata.csv")
#### 重新讀csv進來的話要再factor一次####
timeSplit.cov$visit <- as.factor(timeSplit.cov$visit)
#### V12 V3 ####
timeSplit.cov1 <- read.csv("TCHCData/yesCOVplusHOS.csv")
timeSplit.cov1 <- timeSplit.cov1[,-1]
cov.V12V3 <- TrainTest(data = timeSplit.cov1, VisitOrCase = "Visit", nfixed = T, Train = 1:2,
                        Test = 3, seed = 123, removeCategory = NULL, Trainper = 0.8)

covTrain.V12V3 <- cov.V12V3$`Training set`
covTest.V12V3 <- cov.V12V3$`Test set`
covTrain.V12V3_uncomplete <- covTrain.V12V3 %>% group_by(MRN) %>% filter(n()!=4)
covTrain.V12V3 <- covTrain.V12V3[-which(covTrain.V12V3$MRN %in% covTrain.V12V3_uncomplete$MRN),]
covTest.V12V3 <- covTest.V12V3[-which(covTest.V12V3$MRN %in% covTrain.V12V3_uncomplete$MRN),]

#### Model Building ####
cov.V12Train <- covTrain.V12V3#284
cov.V3Test <- covTest.V12V3#142
cov.V12Train$visit <- factor(cov.V12Train$visit ,levels=c("1","2","3"))
cov.V3Test$visit <- factor(cov.V3Test$visit ,levels=c("1","2","3"))
cov.V12Train$HOS <- factor(cov.V12Train$HOS)
cov.V3Test$HOS <- factor(cov.V3Test$HOS)
cov.V12Train$CCB <- factor(cov.V12Train$CCB)
cov.V3Test$CCB <- factor(cov.V3Test$CCB)
#write.csv(cov.V12Train,"TCHCData/cov_V12Train.csv")
#write.csv(cov.V3Test,"TCHCData/cov_V3Test.csv")
#### random = "+(1|MRN)" +Visit(3 levels) ####
covV12V3  <- BiMMforest1(traindata = cov.V12Train, testdata = cov.V3Test,
                        formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "maxfun")

covV12V3$`model summary`
covV12V3$`CM of Train data`
covV12V3$`Train acc sen spe`#0.9823944 0.9910714 0.9500000
covV12V3$`CM of Test data`
covV12V3$`Test acc sen spe`#0.7535211 0.9433962 0.1944444
covV12V3$`lme.CM of Test data`
covV12V3$`lme.Test acc sen spe`#0.6478873 0.8207547 0.1388889
#輸出成一個table
cov1 <- cbind(covV12V3$`Train acc sen spe`,covV12V3$`Test acc sen spe`,covV12V3$`lme.Test acc sen spe`)
colnames(cov1)<- c("Train","Test","lme.test")
rownames(cov1)<- c("Acc","Sen","Spe")
cov1
tryy  <- BiMMforest1(traindata = cov.V12Train, testdata = cov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")
tryy$RF
tryy$`model summary`
tryy$`CM of Train data`
tryy$`Train acc sen spe`#0.9823944 0.9910714 0.9500000
tryy$`CM of Test data`
tryy$`Test acc sen spe`#0.7535211 0.9433962 0.1944444
tryy$`lme.CM of Test data`
tryy$`lme.Test acc sen spe`#0.6478873 0.8207547 0.1388889

aa <- as.data.frame(tryy$test.preds)
table(cov.V12Train$dip)
table(cov.V3Test$dip)
table(aa$`tryy$test.preds`)

#cov.V12Train$HOS <- as.numeric(cov.V12Train$HOS)
#cov.V3Test$HOS <- as.numeric(cov.V3Test$HOS)
#cov.V12Train$CCB <- as.numeric(cov.V12Train$CCB)
#cov.V3Test$CCB <- as.numeric(cov.V3Test$CCB)
#把time 變成factor後 有機會變成all of the binary outcomes are the same
covV12V3.H1 <- BiMMforestH1(traindata = cov.V12Train, testdata = cov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")
covV12V3.H1$iter #1000
covV12V3.H1$`model summary`
covV12V3.H1$`CM of Train data`
covV12V3.H1$`Train acc sen spe`#0.9119718 0.9866071 0.6333333
covV12V3.H1$`CM of Test data` 
covV12V3.H1$`Test acc sen spe`#0.74647887 0.98113208 0.05555556
covV12V3.H1$`lme.CM of Test data`
covV12V3.H1$`lme.Test acc sen spe`#0.7323944 0.9245283 0.1666667
cov3 <- cbind(covV12V3.H1$`Train acc sen spe`,covV12V3.H1$`Test acc sen spe`,covV12V3.H1$`lme.Test acc sen spe`)
colnames(cov3)<- c("Train","Test","lme.test")
rownames(cov3)<- c("Acc","Sen","Spe")
cov3

covV12V3.H1.max <- BiMMforestH1(traindata = cov.V12Train, testdata = cov.V3Test,
                          formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                          random = "+(1|MRN)",
                          seed = 123, glmControl = "maxfun")
covV12V3.H1.max$iter#1000
covV12V3.H1.max$`run time`
covV12V3.H1.max$`model summary`
covV12V3.H1.max$`CM of Train data`
covV12V3.H1.max$`Train acc sen spe`# 0.9014085 0.9732143 0.6333333
covV12V3.H1.max$`CM of Test data`
covV12V3.H1.max$`Test acc sen spe`#0.75356338 0.99 0.05555556
covV12V3.H1.max$`lme.CM of Test data`
covV12V3.H1.max$`lme.Test acc sen spe`#0.7253521 0.9056604 0.1944444
cov4 <- cbind(covV12V3.H1.max$`Train acc sen spe`,covV12V3.H1.max$`Test acc sen spe`,covV12V3.H1.max$`lme.Test acc sen spe`)
colnames(cov4)<- c("Train","Test","lme.test")
rownames(cov4)<- c("Acc","Sen","Spe")
cov4

covV12V3.H3 <- BiMMforestH3(traindata = cov.V12Train, testdata = cov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")

covV12V3.H3$iter#9
covV12V3.H3$`model summary`
covV12V3.H3$`Train acc sen spe`#0.9683099 0.9821429 0.9166667
covV12V3.H3$`Test acc sen spe`#0.6971831 0.8679245 0.1944444
covV12V3.H3$`CM of Train data`
covV12V3.H3$`CM of Test data`
covV12V3.H3$`lme.CM of Test data`
covV12V3.H3$`lme.Test acc sen spe`#0.7042254 0.8867925 0.1666667
cov5 <- cbind(covV12V3.H3$`Train acc sen spe`,covV12V3.H3$`Test acc sen spe`,covV12V3.H3$`lme.Test acc sen spe`)
colnames(cov5)<- c("Train","Test","lme.test")
rownames(cov5)<- c("Acc","Sen","Spe")
cov5

try <- BiMMforestH3(traindata = cov.V12Train, testdata = cov.V3Test,
                   formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                   random = "+(1|MRN)",
                   seed = 123, glmControl = "maxfun")
try$iter#7
try$`model summary`
try$`CM of Train data`
try$`Train acc sen spe`#0.9683099 0.9821429 0.9166667
try$`CM of Test data`
try$`Test acc sen spe`#0.6971831 0.8679245 0.1944444
try$`lme.CM of Test data`
try$`lme.Test acc sen spe`#0.7042254 0.8867925 0.1666667
cov2 <- cbind(try$`Train acc sen spe`,try$`Test acc sen spe`,try$`lme.Test acc sen spe`)
colnames(cov2)<- c("Train","Test","lme.test")
rownames(cov2)<- c("Acc","Sen","Spe")
cov2

covV12V3.H2 <- BiMMforestH2(traindata = cov.V12Train, testdata = cov.V3Test,
                           formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                           random = "+(1|MRN)",
                           seed = 123, glmControl = "maxfun")
covV12V3.H2$iter#7
covV12V3.H2$`run time`
covV12V3.H2$`model summary`
covV12V3.H2$`Train acc sen spe`#0.9683099 0.9821429 0.9166667
covV12V3.H2$`Test acc sen spe`#
covV12V3.H2$`CM of Train data`
covV12V3.H2$`CM of Test data`
covV12V3.H2$`lme.CM of Test data`
covV12V3.H2$`lme.Test acc sen spe`#0.7042254 0.8867925 0.1666667
cov6 <- cbind(covV12V3.H2$`Train acc sen spe`,covV12V3.H2$`Test acc sen spe`,covV12V3.H2$`lme.Test acc sen spe`)
colnames(cov6)<- c("Train","Test","lme.test")
rownames(cov6)<- c("Acc","Sen","Spe")
cov6
#### Data scale (standardization) ####
ds.covTrain.V12 <- DataScale(data = cov.V12Train,NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"))
ds.covTest.V3 <- DataScale(data = cov.V3Test,NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"))
#### after data scale modeling again ####
dscov.V12Train<- ds.covTrain.V12$scale.df
dscov.V3Test <- ds.covTest.V3$scale.df
#### random = "+(1|MRN)" +Visit(3 levels) ####
dscovV12V3  <- BiMMforest1(traindata = dscov.V12Train, testdata = dscov.V3Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "maxfun")

dscovV12V3$`model summary`
dscovV12V3$`CM of Train data`
dscovV12V3$`Train acc sen spe`#0.9718310 0.9821429 0.9333333
dscovV12V3$`CM of Test data`
dscovV12V3$`Test acc sen spe`#0.7746479 0.9245283 0.3333333
dscovV12V3$`lme.CM of Test data`
dscovV12V3$`lme.Test acc sen spe`#0.64084507 0.83018868 0.08333333
#輸出成一個table
dscov1 <- cbind(dscovV12V3$`Train acc sen spe`,dscovV12V3$`Test acc sen spe`,dscovV12V3$`lme.Test acc sen spe`)
colnames(dscov1)<- c("Train","Test","lme.test")
rownames(dscov1)<- c("Acc","Sen","Spe")
dscov1

tryy  <- BiMMforest1(traindata = dscov.V12Train, testdata = dscov.V3Test,
                     formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                     random = "+(1|MRN)",
                     seed = 123, glmControl = "tolPwrss")
tryy$RF
tryy$`model summary`
tryy$`CM of Train data`
tryy$`Train acc sen spe`# 0.9718310 0.9821429 0.9333333
tryy$`CM of Test data`
tryy$`Test acc sen spe`#0.7746479 0.9245283 0.3333333
tryy$`lme.CM of Test data`
tryy$`lme.Test acc sen spe`#0.63380282 0.82075472 0.08333333

aa <- as.data.frame(tryy$test.preds)
table(cov.V12Train$dip)
table(cov.V3Test$dip)
table(aa$`tryy$test.preds`)

#cov.V12Train$HOS <- as.numeric(cov.V12Train$HOS)
#cov.V3Test$HOS <- as.numeric(cov.V3Test$HOS)
#cov.V12Train$CCB <- as.numeric(cov.V12Train$CCB)
#cov.V3Test$CCB <- as.numeric(cov.V3Test$CCB)
#把time 變成factor後 有機會變成all of the binary outcomes are the same
dscovV12V3.H1 <- BiMMforestH1(traindata = dscov.V12Train, testdata = dscov.V3Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")
dscovV12V3.H1$iter #6"all of the binary outcomes are the same"
dscovV12V3.H1$`model summary`
dscovV12V3.H1$`CM of Train data`
dscovV12V3.H1$`Train acc sen spe`#
dscovV12V3.H1$`CM of Test data` 
dscovV12V3.H1$`Test acc sen spe`#
dscovV12V3.H1$`lme.CM of Test data`
dscovV12V3.H1$`lme.Test acc sen spe`#
dscov3 <- cbind(dscovV12V3.H1$`Train acc sen spe`,dscovV12V3.H1$`Test acc sen spe`,dscovV12V3.H1$`lme.Test acc sen spe`)
colnames(dscov3)<- c("Train","Test","lme.test")
rownames(dscov3)<- c("Acc","Sen","Spe")
dscov3

dscovV12V3.H1.max <- BiMMforestH1(traindata = dscov.V12Train, testdata = dscov.V3Test,
                                formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                                random = "+(1|MRN)",
                                seed = 123, glmControl = "maxfun")
dscovV12V3.H1.max$iter#6"all of the binary outcomes are the same"
dscovV12V3.H1.max$`run time`
dscovV12V3.H1.max$`model summary`
dscovV12V3.H1.max$`CM of Train data`
dscovV12V3.H1.max$`Train acc sen spe`#
dscovV12V3.H1.max$`CM of Test data`
dscovV12V3.H1.max$`Test acc sen spe`#
dscovV12V3.H1.max$`lme.CM of Test data`
dscovV12V3.H1.max$`lme.Test acc sen spe`#
dscov4 <- cbind(dscovV12V3.H1.max$`Train acc sen spe`,dscovV12V3.H1.max$`Test acc sen spe`,dscovV12V3.H1.max$`lme.Test acc sen spe`)
colnames(dscov4)<- c("Train","Test","lme.test")
rownames(dscov4)<- c("Acc","Sen","Spe")
dscov4

dscovV12V3.H3 <- BiMMforestH3(traindata = dscov.V12Train, testdata = dscov.V3Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")

dscovV12V3.H3$iter#10
dscovV12V3.H3$`model summary`
dscovV12V3.H3$`Train acc sen spe`#0.9788732 0.9866071 0.9500000
dscovV12V3.H3$`Test acc sen spe`#0.6760563 0.8018868 0.3055556
dscovV12V3.H3$`CM of Train data`
dscovV12V3.H3$`CM of Test data`
dscovV12V3.H3$`lme.CM of Test data`
dscovV12V3.H3$`lme.Test acc sen spe`#0.6971831 0.8773585 0.1666667
dscov5 <- cbind(dscovV12V3.H3$`Train acc sen spe`,dscovV12V3.H3$`Test acc sen spe`,dscovV12V3.H3$`lme.Test acc sen spe`)
colnames(dscov5)<- c("Train","Test","lme.test")
rownames(dscov5)<- c("Acc","Sen","Spe")
dscov5

dstry <- BiMMforestH3(traindata = dscov.V12Train, testdata = dscov.V3Test,
                    formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                    random = "+(1|MRN)",
                    seed = 123, glmControl = "maxfun")
dstry$iter#10
dstry$`model summary`
dstry$`CM of Train data`
dstry$`Train acc sen spe`#0.9788732 0.9866071 0.9500000
dstry$`CM of Test data`
dstry$`Test acc sen spe`# 0.6760563 0.8018868 0.3055556
dstry$`lme.CM of Test data`
dstry$`lme.Test acc sen spe`#0.6971831 0.8773585 0.1666667
dscov2 <- cbind(dstry$`Train acc sen spe`,dstry$`Test acc sen spe`,dstry$`lme.Test acc sen spe`)
colnames(dscov2)<- c("Train","Test","lme.test")
rownames(dscov2)<- c("Acc","Sen","Spe")
dscov2

dscovV12V3.H2 <- BiMMforestH2(traindata = dscov.V12Train, testdata = dscov.V3Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+visit+HOS,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "maxfun")
dscovV12V3.H2$iter#10
dscovV12V3.H2$`run time`
dscovV12V3.H2$`model summary`
dscovV12V3.H2$`Train acc sen spe`#0.9788732 0.9866071 0.9500000
dscovV12V3.H2$`Test acc sen spe`#0.6760563 0.8018868 0.3055556
dscovV12V3.H2$`CM of Train data`
dscovV12V3.H2$`CM of Test data`
dscovV12V3.H2$`lme.CM of Test data`
dscovV12V3.H2$`lme.Test acc sen spe`#0.6971831 0.8773585 0.1666667
dscov6 <- cbind(dscovV12V3.H2$`Train acc sen spe`,dscovV12V3.H2$`Test acc sen spe`,dscovV12V3.H2$`lme.Test acc sen spe`)
colnames(dscov6)<- c("Train","Test","lme.test")
rownames(dscov6)<- c("Acc","Sen","Spe")
dscov6
#k2=1.8 Train 0.92 0.96 0.78 Test 0.61 0.61 0.61
#k2=1.65Train 0.97 0.98 0.93 Test 0.65 0.77 0.28 0.69 0.85 0.2





#

#

#### V1 V2 樣本與V12V3相同####
covTrain.V1V2 <- covTrain.V12V3[which(covTrain.V12V3$visit==1),]#162
covTest.V1V2 <- covTrain.V12V3[which(covTrain.V12V3$visit==2),]#162
ds.covTrain.V1V2 <- DataScale(data = covTrain.V1V2,NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"))
ds.covTest.V1V2 <- DataScale(data = covTest.V1V2,NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"))

#### Model Building ####
covV1Train <- ds.covTrain.V1V2$scale.df
covV2Test <- ds.covTest.V1V2$scale.df
View(covV2Test)
table(covV2Test$dip)
covV1Train$HOS <- factor(covV1Train$HOS)
covV2Test$HOS <- factor(covV2Test$HOS)
covV1Train$CCB <- factor(covV1Train$CCB)
covV2Test$CCB <- factor(covV2Test$CCB)
str(covV1Train)
#### random = "(1+MRN)"####
covV1V2  <- BiMMforest1(traindata = covV1Train, testdata = covV2Test,
                         formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+HOS,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "maxfun")
covV1V2$`model summary`
covV1V2$`CM of Train data`
covV1V2$`Train acc sen spe`#1 1 1
covV1V2$`CM of Test data`
covV1V2$`Test acc sen spe`#0.7676056 0.9818182 0.0312500
covV1V2$`lme.CM of Test data`
covV1V2$`lme.Test acc sen spe`#0.7605634 0.8636364 0.4062500
#Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

tryy12  <- BiMMforest1(traindata = covV1Train, testdata = covV2Test,
                     formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+HOS,
                     random = "+(1|MRN)",
                     seed = 123, glmControl = "tolPwrss")
#Model is nearly unidentifiable: very large eigenvalue
tryy12$RF
tryy12$`model summary`
tryy12$`CM of Train data`
tryy12$`Train acc sen spe`#1 1 1
tryy12$`CM of Test data`
tryy12$`Test acc sen spe`#0.7676056 0.9818182 0.0312500
tryy12$`lme.CM of Test data`
tryy12$`lme.Test acc sen spe`#0.7535211 0.8454545 0.4375000

aa <- as.data.frame(tryy$test.preds)
table(cov.V12Train$dip)
table(cov.V3Test$dip)
table(aa$`tryy$test.preds`)

covV1V2.H1 <- BiMMforestH1(traindata = covV1Train, testdata = covV2Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+HOS,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")
covV1V2.H1$iter #1000
covV1V2.H1$`model summary`
covV1V2.H1$`CM of Train data`
covV1V2.H1$`Train acc sen spe`#1 1 1
covV1V2.H1$`CM of Test data` 
covV1V2.H1$`Test acc sen spe`#0.7746479 0.9818182 0.0625000
covV1V2.H1$`lme.CM of Test data`
covV1V2.H1$`lme.Test acc sen spe`#0.8028169 0.8909091 0.5000000
cov32 <- cbind(covV1V2.H1$`Train acc sen spe`,covV1V2.H1$`Test acc sen spe`,covV1V2.H1$`lme.Test acc sen spe`)
colnames(cov32)<- c("Train","Test","lme.test")
rownames(cov32)<- c("Acc","Sen","Spe")
cov32


covV12.H3 <- BiMMforestH3(traindata = covV1Train, testdata = covV2Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+HOS,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")

covV12.H3$iter#100
covV12.H3$`model summary`
covV12.H3$`Train acc sen spe`#1 1 1
covV12.H3$`Test acc sen spe`#0.7676056 0.9363636 0.1875000
covV12.H3$`CM of Train data`
covV12.H3$`CM of Test data`
covV12.H3$`lme.CM of Test data`
covV12.H3$`lme.Test acc sen spe`#0.8028169 0.8909091 0.5000000
cov52 <- cbind(covV12.H3$`Train acc sen spe`,covV12.H3$`Test acc sen spe`,covV12.H3$`lme.Test acc sen spe`)
colnames(cov52)<- c("Train","Test","lme.test")
rownames(cov52)<- c("Acc","Sen","Spe")
cov52

try <- BiMMforestH3(traindata = covV1Train, testdata = covV2Test,
                    formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+HOS,
                    random = "+(1|MRN)",
                    seed = 123, glmControl = "maxfun")
try$iter#7
try$`model summary`
try$`CM of Train data`
try$`Train acc sen spe`#0.9683099 0.9821429 0.9166667
try$`CM of Test data`
try$`Test acc sen spe`#0.9683099 0.9821429 0.9166667
try$`lme.CM of Test data`
try$`lme.Test acc sen spe`#0.7042254 0.8867925 0.1666667
cov2 <- cbind(try$`Train acc sen spe`,try$`Test acc sen spe`,try$`lme.Test acc sen spe`)
colnames(cov2)<- c("Train","Test","lme.test")
rownames(cov2)<- c("Acc","Sen","Spe")
cov2

covV12V3.H2 <- BiMMforestH2(traindata = covV1Train, testdata = covV2Test,
                            formula = dip ~ sys+dia+time+sex+age+HbA1C+HR+CCB+HOS,
                            random = "+(1|MRN)",
                            seed = 123, glmControl = "tolPwrss")
covV12V3.H2$iter#1000
covV12V3.H2$`run time`
covV12V3.H2$`model summary`
covV12V3.H2$`Train acc sen spe`#1 1 1
covV12V3.H2$`Test acc sen spe`# 0.7676056 0.9363636 0.1875000
covV12V3.H2$`CM of Train data`
covV12V3.H2$`CM of Test data`
covV12V3.H2$`lme.CM of Test data`
covV12V3.H2$`lme.Test acc sen spe`#0.8028169 0.8909091 0.5000000
cov6 <- cbind(covV12V3.H2$`Train acc sen spe`,covV12V3.H2$`Test acc sen spe`,covV12V3.H2$`lme.Test acc sen spe`)
colnames(cov6)<- c("Train","Test","lme.test")
rownames(cov6)<- c("Acc","Sen","Spe")
cov6


######### NO COV ########
##### timeSplit裡有把sys dia轉成數值 ####

timeSplit.22 <- timeSplit(data = result.22$myData, 
                      removeCol.AM = reCol.AM,
                      removeCol.PM = reCol.PM)
write.csv(timeSplit.22,"TCHCData/noCOVdata.csv")
table(timeSplit.22$visit)
#### V1 V2####
timeSplit.hos <- read.csv("TCHCData/noCOVplusHOS.csv")
timeSplit.hos <- timeSplit.hos[,-1]
TrainTest.22 <- TrainTest(data = timeSplit.hos, VisitOrCase = "Visit", nfixed = T, Train = 1,
                      Test = 2, seed = 123, removeCategory = NULL, Trainper = 0.8)
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
#加醫院變數 去跑
#### model Building ####
V12Train <- Train.V12V3 #324
V3Test <- Test.V12V3 #162
V12Train$visit <- factor(V12Train$visit, levels = c(1,2,3))
V3Test$visit <- factor(V3Test$visit, levels = c(1,2,3))
#### random = "+(1+time|sys)+(1+time|dia)" ####
V12V3 <- BiMMforest1(traindata = V12Train, testdata = V3Test,
                    formula = dip ~ sys+dia+time+visit,
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


#### random = "+(1|MRN)" +Visit(3levels) ####

V12V3.1 <- BiMMforest1(traindata = V12Train, testdata = V3Test,
                   formula = dip ~ sys+dia+time+visit,
                   random = "+(1|MRN)",
                   seed = 123)
V12V3.1$`model summary`
V12V3.1$`CM of Train data`
V12V3.1$`Train acc sen spe`#0.9012346 0.9488189 0.7285714
V12V3.1$`CM of Test data`
V12V3.1$`Test acc sen spe`#0.7037037 0.9237288 0.1136364


V12V3.H1<-BiMMforestH1(traindata = V12Train, testdata = V3Test,
                     formula = dip ~ sys+dia+time+visit,
                     random = "+(1|MRN)",
                     seed = 123)
#"all of the binary outcomes are the same"

V12V3.H3<-BiMMforestH3(traindata = V12Train, testdata = V3Test,
                     formula = dip ~ sys+dia+time+visit,
                     random = "+(1|MRN)",
                     seed = 123)
V12V3.H3$iter#2
V12V3.H3$`Train acc sen spe`#0.9012346 0.9488189 0.7285714
V12V3.H3$`Test acc sen spe`#0.7037037 0.9237288 0.1136364
V12V3.H3$`CM of Train data`
V12V3.H3$`CM of Test data`

V12V3.H2 <- BiMMforestH2(traindata = V12Train, testdata = V3Test,
                       formula = dip ~ sys+dia+time+visit,
                       random = "+(1|MRN)",
                       seed = 123)
V12V3.H2$iter#2
V12V3.H2$`model summary`
V12V3.H2$`Train acc sen spe`#0.9012346 0.9488189 0.7285714
V12V3.H2$`Test acc sen spe`#0.7037037 0.9237288 0.1136364
V12V3.H2$`CM of Train data`
V12V3.H2$`CM of Test data`



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
ds.Train.V1V2 <- DataScale(data = Train.V1V2,NumVar = c("sys", "dia" ))
ds.Test.V1V2 <- DataScale(data = Test.V1V2,NumVar = c("sys", "dia" ))

#### model Building ####
V1Train.V3 <- ds.Train.V1V2$scale.df
V2Test.V3 <- ds.Test.V1V2$scale.df
#### random = "+(1|MRN)" ####

V1V2.V3.1 <- BiMMforest1(traindata = V1Train.V3, testdata = V2Test.V3,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123, glmControl = "tolPwrss")
V1V2.V3.1$`model summary`
V1V2.V3.1$`CM of Train data`
V1V2.V3.1$`Train acc sen spe`#1 1 1
V1V2.V3.1$`CM of Test data`
V1V2.V3.1$`Test acc sen spe`#0.753 1 0


V1V2.V3.H1<-BiMMforestH1(traindata = V1Train.V3, testdata = V2Test.V3,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123,glmControl = "tolPwrss")
#"all of the binary outcomes are the same"

V1V2.V3.H3<-BiMMforestH3(traindata = V1Train.V3, testdata = V2Test.V3,
                       formula = dip ~ sys+dia+time,
                       random = "+(1|MRN)",
                       seed = 123, glmControl = "tolPwrss")
V1V2.V3.H3$iter#2
V1V2.V3.H3$`model summary`
V1V2.V3.H3$`Train acc sen spe`#1 1 1
V1V2.V3.H3$`Test acc sen spe`#0.753 1 0
V1V2.V3.H3$`CM of Train data`
V1V2.V3.H3$`CM of Test data`

V1V2.V3.H2 <- BiMMforestH2(traindata = V1Train.V3, testdata = V2Test.V3,
                         formula = dip ~ sys+dia+time,
                         random = "+(1|MRN)",
                         seed = 123, glmControl = "tolPwrss")
V1V2.V3.H2$iter#2
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















#
#####小結 ####
#### 預測new cases #### 

