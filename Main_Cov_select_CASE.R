setwd("C:/Users/hsu/Desktop/master")
source("Train_Test_file_cov_select.r")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#CCTr,CCTe,CNTr,CNTe
F1 <- dip~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia
F2 <- dip~sbp+dbp+time+HOS
F3 <- factor(dip)~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia
F4 <- factor(dip)~sbp+dbp+time+HOS
seed = 123
dim(CCTr)#838 *17
dim(CCTe)#210 * 17
dim(CNTr)#880
dim(CNTe)#222

table(CCTe$dip)
table(CCTr$dip)


install.packages("devtools")
devtools::install_github("ncordon/imbalance")
library("imbalance")
A <- CCTr[which(CCTr$dip %in% 0),]
n <- nrow(CCTr)-nrow(A)*2
str(A)
B <- CCTr
B$Gender <- as.numeric(B$Gender)
B$Drug_conut <- as.numeric(B$Drug_conut)
B$DM <- as.numeric(B$DM)
B$HOS <- as.numeric(B$HOS)
B$Walk_TM_week <- as.numeric(B$Walk_TM_week)
B$anti_HP <- as.numeric(B$anti_HP)
B["ID"] <- rep(1:419, each = 2) 
B <- B[,c(18,2:17)]
t <- mwmote(B, numInstances = n,classAttr = "dip")
imbalanceRatio(B,classAttr = "dip")
re <- rbind(B,t)

re$sbp <- as.numeric(re$sbp)
re$dbp <- as.numeric(re$dbp)
re$HR <- as.numeric(re$HR)
re$BMI <- as.numeric(re$BMI)
re$Waist <- as.numeric(re$Waist)
re$office_peri_L_sys <- as.numeric(re$office_peri_L_sys)
re$office_peri_L_dia <- as.numeric(re$office_peri_L_dia)
re$HR <- as.numeric(re$HR)

re$HOS <- factor(re$HOS,levels=c(1:3))
re$Gender <- as.factor(re$Gender)
re$Drug_conut <- factor(re$Drug_conut,levels=c(0:5))
re$DM <- factor(re$DM,levels=c(1:3))
re$Walk_TM_week <- factor(re$Walk_TM_week,levels=c(0:7))
re$anti_HP <- factor(re$anti_HP,levels=c(1:13))


re.rf <- RF(traindata = re, testdata = CCTe,
            formula = F3,seed = 23)
any(is.na(re))
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
CC.1$test.preds
CC.1$test.lme.preds
length(factor(CCTe$dip))
length(factor(CC.1$test.preds,ordered = TRUE))
auc(factor(CCTe$dip),factor(CC.1$test.preds,ordered = TRUE))#Area under the curve: 0.5238
#Area under the curve: 0.5238
roc_qda=roc(response=as.factor(CCTe$dip), predictor= factor(CC.1$test.lme.preds,ordered = TRUE), plot=TRUE)


CCC.1 <- BiMMforest1(traindata = D, testdata = E,
                    formula = F1, random = "+(1|time)",
                    seed = seed, glmControl = "maxfun")
CCC.1$`model summary`
CCC.1$`CM of Train data`
CCC.1$`Train acc sen spe`#0.9260143 0.9752705 0.7591623
CCC.1$`CM of Test data`
CCC.1$`Test acc sen spe`#0.9190476 0.9937107 0.6862745
CCC.1$`lme.CM of Test data`
CCC.1$`lme.Test acc sen spe`#0.6714286 0.7798742 0.3333333

auc(factor(E$dip),factor(CCC.1$test.preds,ordered = TRUE))#Area under the curve: 0.5238
#Area under the curve: 0.8488



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

CCC.H1 <- BiMMforestH1(traindata = D, testdata = E,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")

CCC.H1$iter#1000
CCC.H1$RF#OB estimate of  error rate: 4.42
CCC.H1$`model summary`
CCC.H1$`CM of Train data`
CCC.H1$`Train acc sen spe`#0.8973747 0.9891808 0.5863874
CCC.H1$`CM of Test data`
CCC.H1$`Test acc sen spe`#0.76190476 1.00000000 0.01960784
CCC.H1$`lme.CM of Test data`
CCC.H1$`lme.Test acc sen spe`#0.7142857 0.8616352 0.2549020
CCC.H1$`RF OOBerror`



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

CCC.H2 <- BiMMforestH2(traindata = D, testdata =E,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
CCC.H2$RF
CCC.H2$`model summary`
CCC.H2$`CM of Train data`
CCC.H2$`Train acc sen spe`#0.9176611 0.9675425 0.7486911
CCC.H2$`CM of Test data`
CCC.H2$`Test acc sen spe`#0.8857143 0.9433962 0.7058824
CCC.H2$`lme.CM of Test data`
CCC.H2$`lme.Test acc sen spe`#0.6857143 0.7987421 0.3333333


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
CC.H3$test.preds
CC.H3$test.lme.preds

CCC.H3 <- BiMMforestH3(traindata = D, testdata = E,
                      formula = F1, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")

CCC.H3$RF
CCC.H3$`model summary`
CCC.H3$`CM of Train data`
CCC.H3$`Train acc sen spe`#0.9176611 0.9675425 0.7486911
CCC.H3$`CM of Test data`
CCC.H3$`Test acc sen spe`#0.8857143 0.9433962 0.7058824
CCC.H3$`lme.CM of Test data`
CCC.H3$`lme.Test acc sen spe`#0.6857143 0.7987421 0.3333333


CC.rf <- RF(traindata = CCTr, testdata = CCTe,
                  formula = F3,seed = 23)
CC.rf$`CM of Train data`
CC.rf$`Train acc sen spe`#0.9677804 1.0000000 0.8650000
CC.rf$`CM of Test data`
CC.rf$`Test acc sen spe`#0.81428571 1.00000000 0.07142857
CC.rf$`Var importance`
CC.rf$RF
del <- which(colnames(CCTr)%in% c("office_peri_L_sys","office_peri_L_dia"))
del2 <- which(colnames(CCTe)%in% c("office_peri_L_sys","office_peri_L_dia"))

t1 <- CCTr[,-c(del)]
t2 <- CCTe[,-c(del2)]
FF <- factor(dip)~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP

CW <- RF(traindata = t1, testdata = t2,
            formula = FF,seed = 23,
         classwt = c("0" = 2.095,"1" = 0.65))
CW
w0 <- nrow(CCTr)/(2*length(which(CCTr$dip %in% 0)))
w1 <- nrow(CCTr)/(2*length(which(CCTr$dip %in% 1)))


table(t1$dip)
sam <- CCTr[which(CCTr$dip %in% 0),]
reidx <- sample(1:200,438,replace = T)
rere <- sam[reidx,]
re1 <- rbind(CCTr,rere)
re1 <- re1[,-c(15,16)]
re1RF <- RF(traindata = re1, testdata = t2,
            formula = FF,seed = 23,
            classwt = c("0" = 2.095,"1" = 0.65))
table(t2$dip)

sam2 <- t2[which(t2$dip %in% 0),]
reidx2 <- sample(1:42,126,replace = T)
rere2 <- sam2[reidx2,]
re2 <- rbind(t2,rere2)

re2RF <- RF(traindata = re1, testdata = re2,
            formula = FF,seed = 23,
            classwt = c("0" = 2.095,"1" = 0.65))

R1 <- randomForest(FF,data = re1, method = "class")
R1T <- predict(R1,re2)
table(real = re2$dip ,pred = R1T)
table(re1$dip)
table(re2$dip)

View(CCTr)
library(ggplot2)
ggplot(CCTr, aes(x = 1:nrow(CCTr), y=BMI*BMI*BMI, col = as.factor(dip)))+
  geom_point()

AA <- CCTr
AA["BMII"]<- AA$BMI*AA$BMI*AA$BMI
FA <- factor(dip)~sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMII+Waist+Walk_TM_week+anti_HP
BB <- CCTe
BB["BMII"]<- BB$BMI*BB$BMI*BB$BMI
BB <- BB[,c(1:16,18,17)]
BMIRF <- RF(traindata = AA, testdata = BB,
            formula = FA,seed = 23,)

a <- read.csv("TCHCData/CASE_COV_select_RFimp.csv")
a <- a[,-c(1,2)]
set.seed(123)
idx <- sample(1:nrow(a),0.8*nrow(a))
D <- a[idx,]
E <- a[-idx,]

CC1.rf <- RF(traindata = D, testdata = E,
            formula = F3,seed = 123)
CC1.rf$`CM of Train data`
CC1.rf$`Train acc sen spe`#0.9045346 0.9968701 0.6080402
CC1.rf$`CM of Test data`
CC1.rf$`Test acc sen spe`#0.9285714 1.0000000 0.6511628
CC1.rf$`Var importance`
CC1.rf$RF
CC1.rf$`test pred`

library(pROC)
#auc(CC.H3$test.preds,CCTe$dip)
auc(as.factor(E$dip),factor(CC1.rf$`test pred`,ordered = TRUE))#Area under the curve: 0.8256
roc_qda=roc(response=as.factor(E$dip), predictor= factor(CC1.rf$`test pred`, 
                                                        ordered = TRUE), plot=TRUE)

### 找mtry
library(caret)
#for RF only mtry can be tuned by caret
#reasons :its effect on the final accuracy and that it must be found empirically for a dataset.
#因為變數不多直接用grid search
#Grid Search
metric="accuracy"
dataset = D
seed = 123
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3,
                        search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(F3, data=dataset,
                       method="rf", metric=metric,
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)#最好的mtry =15
p1<- plot(rf_gridsearch)
gridExtra::marrangeGrob(list(p1), top = "",
                        nrow = 1, ncol = 1)

CC.rf.15 <- RF(traindata = D, testdata = E,
               formula = F3, classwt = NULL,seed = seed,mtry = 15)
CC.rf.15$`CM of Train data`
CC.rf.15$`Train acc sen spe`#0.9212411 0.9937402 0.6884422
CC.rf.15$`CM of Test data`
CC.rf.15$`Test acc sen spe`#0.947619 1.000000 0.744186
CC.rf.15$`Var importance`

table(D$dip)# 0=191 1=647
table(E$dip)# 0=51 1=159

#Area under the curve: 0.8721
auc(factor(E$dip),factor(CC.rf.15$`test pred`,ordered = TRUE))
###
###
###
###
###

W <- as.data.frame(rbind(CNTr,CNTe))
View(W)
W <- W[,-c(1,2,3,6,8)]
set.seed(123)
idx <- sample(1:nrow(W),0.8*nrow(W))
G <- W[idx,]#881
H <- W[-idx,]#221


CN.1 <- BiMMforest1(traindata = CNTr, testdata = CNTe,
                    formula = F2, random = "+(1|time)",
                    seed = seed, glmControl = "maxfun")
CN.1$`model summary`
CN.1$`CM of Train data`
CN.1$`Train acc sen spe`#0.762 1 0
CN.1$`CM of Test data`
CN.1$`Test acc sen spe`#0.76666667 0.94610778 0.06976744
CN.1$`lme.CM of Test data`
CN.1$`lme.Test acc sen spe`# 0.795 1 0


CN.H1 <- BiMMforestH1(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
#"all of the binary outcomes are the same"

CN.H2 <- BiMMforestH2(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
CN.H2$iter#100
CN.H2$RF#OOB estimate of  error rate: 22.84%
CN.H2$`model summary`
CN.H2$`CM of Train data`
CN.H2$`Train acc sen spe`#0.775 1 0
CN.H2$`CM of Test data`
CN.H2$`Test acc sen spe`#0.69819820 0.90000000 0.03846154
CN.H2$`lme.CM of Test data`
CN.H2$`lme.Test acc sen spe`#0.765 1 0


CN.H3 <- BiMMforestH3(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = seed, glmControl = "maxfun")
CN.H3$iter#100
CN.H3$RF
CN.H3$`model summary`
CN.H3$`CM of Train data`
CN.H3$`Train acc sen spe`#0.775 1 0
CN.H3$`CM of Test data`
CN.H3$`Test acc sen spe`#0.69819820 0.90000000 0.03846154
CN.H3$`lme.CM of Test data`
CN.H3$`lme.Test acc sen spe`#0.765 1 0

CN.rf <- RF(traindata = CNTr, testdata = CNTe,
            formula = F4, classwt = NULL,seed = seed)
CN.rf$`CM of Train data`
CN.rf$`Train acc sen spe`#0.6885442 0.8622848 0.1306533
CN.rf$`CM of Test data`
CN.rf$`Test acc sen spe`#0.7333333 0.8802395 0.1627907
CN.rf$`Var importance`
CN.rf$RF

CCN.rf <- RF(traindata = G, testdata = H,
            formula = F4, classwt = NULL,seed = seed)
CCN.rf$RF
CCN.rf$`CM of Train data`
CCN.rf$`Train acc sen spe`#0.75028377 0.95857988 0.06341463
CCN.rf$`CM of Test data`
CCN.rf$`Test acc sen spe`#0.76470588 0.94318182 0.06666667

auc(as.factor(E$dip),factor(CN.rf$`test pred`,ordered = TRUE))#Area under the curve: 0.8256
#AUC = 0.5215




# a <- read.csv("TCHCData/CASE_COV_select_RFimp.csv")
# a <- a[,-1]
# e <- a[which(a$time %in% 1),]
# f <- a[which(a$time %in% 2),]
# Q <- RF(traindata = e, testdata = f,
#              formula = F4,seed = 23)
# Q$`CM of Train data`
# Q$`Train acc sen spe`#0.76335878 0.98511166 0.02479339
# Q$`CM of Test data`#
# Q$`Test acc sen spe`#0.9503817 1.0000000 0.7851240
# Q$`Var importance`
# Q$RF


D$sbp <- as.numeric(D$sbp)
D$dbp <- as.numeric(D$dbp)
D$HR <- as.numeric(D$HR)
D$BMI <- as.numeric(D$BMI)
D$Waist <- as.numeric(D$Waist)
D$office_peri_L_sys <- as.numeric(D$office_peri_L_sys)
D$office_peri_L_dia <- as.numeric(D$office_peri_L_dia)
D$HR <- as.numeric(D$HR)
D$HOS <- factor(D$HOS,levels=c(1:3))
D$Gender <- as.factor(D$Gender)
D$Drug_conut <- factor(D$Drug_conut,levels=c(0:5))
D$DM <- factor(D$DM,levels=c(1:3))
D$Walk_TM_week <- factor(D$Walk_TM_week,levels=c(0:7))
D$anti_HP <- factor(D$anti_HP,levels=c(1:13))



E$sbp <- as.numeric(E$sbp)
E$dbp <- as.numeric(E$dbp)
E$HR <- as.numeric(E$HR)
E$BMI <- as.numeric(E$BMI)
E$Waist <- as.numeric(E$Waist)
E$office_peri_L_sys <- as.numeric(E$office_peri_L_sys)
E$office_peri_L_dia <- as.numeric(E$office_peri_L_dia)
E$HR <- as.numeric(E$HR)

E$HOS <- factor(E$HOS,levels=c(1:3))
E$Gender <- as.factor(E$Gender)
E$Drug_conut <- factor(E$Drug_conut,levels=c(0:5))
E$DM <- factor(E$DM,levels=c(1:3))
E$Walk_TM_week <- factor(E$Walk_TM_week,levels=c(0:7))
E$anti_HP <- factor(E$anti_HP,levels=c(1:13))


C <- read.csv("TCHCData/CASE_COV_select_wNA.csv")
C <- arrange(C,age)
C <- C[,-1]
library(visdat)
library(ggplot2)
p <- vis_miss(C, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)

V <- read.csv("TCHCData/Visit_COV_select_wNA.csv")
V <- V[,-1]
V <- V[,-which(colnames(V)%in%"T_pain")]
p <- vis_miss(V, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)
S <- read.csv("TCHCData/COV_change_noimp.csv")
S <- S[,-1]





#DC1=>train EC1=>Test
colnames(DC1)
DC1 <- DC1[,c(1:12,14:21,13)]
EC1 <- EC1[,c(1:12,14:21,13)]

FF1 <- dip~sbp+dbp+Age+HR+Drug_conut+time+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia+Gender_1+Gender_2+DM_1+DM_2+DM_3+HOS_1+HOS_2+HOS_3
seed = 123
co <- BiMMforest1(traindata = DC1, testdata = EC1,
                    formula = FF1, random = "+(1|time)",
                    seed = seed, glmControl = "maxfun",sampsize = c(200,110))
co$`model summary`
co$`CM of Train data`
co$`Train acc sen spe`#0.9522673 0.9733542 0.8850000
co$`CM of Test data`
co$`Test acc sen spe`#0.6380952 0.7083333 0.3571429


co.1 <- BiMMforestH1(traindata = DC1, testdata = EC1,
                  formula = FF1, random = "+(1|time)",
                  seed = seed, glmControl = "maxfun",sampsize = c(100,110),mtry=18)

co.1$`model summary`
co.1$test.preds
co.1$`CM of Train data`
co.1$`Train acc sen spe`#0.8603819 0.9764890 0.4900000
co.1$`CM of Test data`
co.1$`Test acc sen spe`#0.7476190 0.8630952 0.2857143

bimc <- as.data.frame(co.1$test.preds)
b <- as.data.frame(bimc[seq(2,210,2),])
a <- as.data.frame(bimc[seq(1,210,2),])
bim <- cbind(a,b)
colnames(bim)<-c("bimcrow1","bimcrow2")


co.2 <- BiMMforestH2(traindata = DC1, testdata = EC1,
                     formula = FF1, random = "+(1|time)",
                     seed = seed, glmControl = "maxfun",sampsize = c(200,110),mtry=18)

co.2$`model summary`
co.2$`CM of Train data`
co.2$`Train acc sen spe`#0.7398568 0.8573668 0.3650000
co.2$`CM of Test data`
co.2$`Test acc sen spe`#0.6190476 0.6726190 0.4047619

co.3 <- BiMMforestH3(traindata = DC1, testdata = EC1,
                     formula = FF1, random = "+(1|time)",
                     seed = seed, glmControl = "maxfun",sampsize = c(200,110),mtry=18)
co.3$`model summary`
co.3$`CM of Train data`
co.3$`Train acc sen spe`#0.7386635 0.8401254 0.4150000
co.3$`CM of Test data`
co.3$`Test acc sen spe`#0.6476190 0.7023810 0.4285714

#訓練=>CNTr 測試=>CNTe
#模型 bimm1 h1 h2 h3 RF
#要社mtry 跟sampsize
#formula2 4
#訓練=>CNTr 測試=>CNTe
str(CNTr)
#模型 bimm1 h1 h2 h3 RF


#要社mtry(一棵樹丟幾個變數) 跟sampsize
#formula 2 4
dim(CNTr)
table(CNTr$dip)
newCNTr <- rbind(CNTr, dplyr::filter(CNTr, dip == 0))

table(newCNTr$dip)

newCNTr2<- rbind(newCNTr, dplyr::filter(CNTr, dip == 0))
table(newCNTr2$dip)

bimm1_mtry2_samp198198 <- BiMMforestH3(traindata = CNTr, testdata = CNTe,
                                       formula = F2, random = "+(1|time)",
                                       seed = 123, glmControl = "maxfun",
                                       sampsize = c(198,198),
                                       mtry=4)#0.5360360 0.6176471 0.2692308


bimm1_mtry2_samp198198$`Train acc sen spe`
bimm1_mtry2_samp198198$`Test acc sen spe`

(tb <- bimm1_mtry2_samp198198$`CM of Train data`)
acc = 682 / (198+682)
sen = 682 / 682+0
spe = 0

bimm1_mtry2_samp198198$`CM of Test data`



bimm3 <- BiMMforestH1(traindata = CNTr, testdata = CNTe,
                      formula = F2, random = "+(1|time)",
                      seed = 123, glmControl = "maxfun",
                      sampsize = c(198,198),
                      mtry=4)
bimm3$`Train acc sen spe`
bimm3$`Test acc sen spe`



bimm4 <- BiMMforest1(traindata = CNTr, testdata = CNTe,
                     formula = F2, random = "+(1|time)",
                     seed = 123, glmControl = "maxfun",
                     sampsize = c(198,198),
                     mtry=4)

bimm4$`Train acc sen spe`
bimm4$`Test acc sen spe`

colnames(CNTr)
rr <- RF(traindata = CNTr, testdata = CNTe,
         formula = F2, classwt = NULL, seed = 123)

rr <- RF(traindata = CNTr, testdata = CNTe,
         formula = F4, seed = 123, sampsize = c(198, 198))

rr$`Train acc sen spe`
rr$`Test acc sen spe`
rr$`CM of Train data`

rr$`CM of Test data`
rr$RF
