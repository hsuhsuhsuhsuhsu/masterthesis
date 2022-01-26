source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#
#### setting parameter ####
file <- "TCHCData/hbp_dip_byx.csv"

#for time split
reCol.AM <- c("HBP_d_PM_systolic","HBP_d_PM_diastolic")
reCol.PM <- c("HBP_d_AM_systolic","HBP_d_AM_diastolic")


#### data processing ####
result.22 <- myRead(file, removeNa = T, category = c("Non dipper", "Reverse dipper"),
                 newVar = "dip")
#timeSplit裡有把sys dia轉成數值
timeSplit.22 <- timeSplit(data = result.22$myData, 
                      removeCol.AM = reCol.AM,
                      removeCol.PM = reCol.PM)
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
                   seed = 123)
H3$iter
H3$`model summary`
H3$`Train acc sen spe`#0.8405797 1 0.3333333 same as V1V2
H3$`Test acc sen spe`#0.7536232 1 0 same as V1V2
H3$`CM of Train data`
H3$`CM of Test data`

H2 <- BiMMforestH2(traindata = V1Train, testdata = V2Test,
                   formula = dip ~ sys+dia+time,
                   random = "+(1+time|sys)+(1+time|dia)",
                   seed = 123)
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

#####小結####
#V1V2 略優於 V12V3
#(1|MRN) 比(1+time|sys)+(1+time|dia)好
#H3H2結果和1iter相同 H1因資料已經偏向1 不需要用

#把樣本限制在一樣的 看誰表現得比較好 就用那個訓練跟測試資料集
#測試sys dia time 22交互作用
#如果有交互作用 在考慮要不要放random effect
#如果交互作用都不顯著 就放3個變數的random effect
#放鄭醫師提到的covariate
#+(1|MRN)是對的
#最後重要的是mixed model 的系數
#把係數寫出來 確保自己了解
#題目machine learning model for nocturnal dipping