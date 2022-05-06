getwd()
setwd("C:/Users/hsu/Desktop/master")
source("1p1r_file_check.r")
source("dummyTest.r")
setwd("C:/Users/hsu/Desktop/master")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")

#1p1r 
#CC => train= dm1tr test=dm1te
#set order of column(dip at last)
colnames(dm1tr)
seed=123
str(dm1tr)
dm1tr <- dm1tr[,c(1:13,15:22,14)]

#logistic
library(dplyr)
 lo <- glm(factor(dip)~sbp_d+dbp_d+sbp_n+dbp_n+Age+HR+Drug_conut+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia+Gender_1+Gender_2+DM_1+DM_2+DM_3+HOS_1+HOS_2+HOS_3,
             data=dm1tr,family = "binomial")
summary(lo)
table(dm1tr$dip)
1-(98/(321+98))
321/(321+98)
probabilities <- lo %>% predict(dm1te, type = "response")
predicted.classes <- ifelse(probabilities > 0.7661098,1 , 0)
cm <- table(real = dm1te$dip, pred = predicted.classes)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
16*15
lopr <- as.data.frame(probabilities)
colnames(lopr)<-"lopr1"
library(car)
vif(lo)

colnames(dm1tr)
numdf <- dm1tr[,c(1:6,8,9,12,13)]
a <- cor(numdf)
View(a)

#SVM

#XGB=>other file
library(randomForest)
FF1 <- factor(dip)~sbp_d+dbp_d+sbp_n+dbp_n+Age+HR+Drug_conut+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia+Gender_1+Gender_2+DM_1+DM_2+DM_3+HOS_1+HOS_2+HOS_3
p <- RF(traindata = dm1tr, testdata = dm1te,
        formula = FF1,sampsize = c(98,80))

p$RF
p$`CM of Train data`
p$`Train acc sen spe`
p$`CM of Test data`
p$`Test acc sen spe`

set.seed(123)
pr1 <- randomForest(factor(dip)~.,data = dm1tr, method = "class",
                    sampsize = c(98,78))
pr1$confusion

se <- predict(pr1,dm1te,type = "prob")
rfc <- ifelse(se > 0.5,1,0)
rfpr <-data.frame(se)
colnames(rfpr) <- c("rfpr0","rfpr1")

cm <- table(re = dm1te$dip,se)
table(test$dip)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))



ESB <- cbind(rfpr,lopr,xgpr)
ESB["lopr0"] <- 1-ESB[,3]
ESB["xgpr0"] <- 1-ESB[,4]
#lo => 0.7661098 xg=>0.4 rf=>0.5
loc <- as.data.frame(predicted.classes)
colnames(loc) <- "loca"
rf <- as.data.frame(rfc)
colnames(rf)<-c("rfc0","rfc1")
xgc <- as.data.frame(xgclass)
colnames(xgc)<-c("xgc1")

ESC.c <- cbind(rf,loc,xgc)

getwd()
setwd("C:/Users/hsu/Desktop/master")
write.csv(ESC.c,"ensclass.csv",row.names = F)
write.csv(ESB,"ENSprob.csv",row.names = F)
View(Un)
Un <- ESC.c
for(i in 1:(nrow(Un))){
  if (any(Un[i,c(2:4)]==1)){
    Un[i,"union"] <- "1"
  }else{
    Un[i,"union"] <- "0"
  }
}#RF XGB LoG聯集(任一個是1結果就是1)
Un <- cbind(ra,Un)
colnames(Un)[1]<-"real"
ra <- as.data.frame(dm1te$dip)
table(re = Un$real,pr= Un$union)
cm <- table(re = Un$real,pr= Un$union)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
#"0.733333333333333 0.890243902439024 0.173913043478261"
for(i in 1:(nrow(Un))){
  if (any(Un[i,c(3:5)]==0)){
    Un[i,"union0"] <- "0"
  }else{
    Un[i,"union0"] <- "1"
  }
}#RF XGB LoG聯集(任一個是0結果就是0)
table(re = Un$real,pr= Un$union0)
cm <- table(re = Un$real,pr= Un$union0)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
#"0.561904761904762 0.548780487804878 0.608695652173913"
for(i in 1:(nrow(Un))){
  if (sum(Un[i,c(3:5)])>=2){
    Un[i,"major"] <- "1"
  }else{
    Un[i,"major"] <- "0"
  }
}#RF XGB LoG多數決
table(re = Un$real,pr= Un$major)
Un <- cbind(Un,bim)
cm <- table(re = Un$real,pr= Un$major)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
#"0.685714285714286 0.804878048780488 0.260869565217391"

Un$bimcrow1 <- as.numeric(Un$bimcrow1)-1
Un$bimcrow2 <- as.numeric(Un$bimcrow2)-1
for(i in 1:(nrow(Un))){
  if (sum(Un[i,c(5,9,10)])>=2){
    Un[i,"majorXB"] <- "1"
  }else{
    Un[i,"majorXB"] <- "0"
  }
}#XGB BIMMH1多數決
table(re = Un$real,pr= Un$majorXB)
cm <- table(re = Un$real,pr= Un$majorXB)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
#"0.695238095238095 0.865853658536585 0.0869565217391304"

for(i in 1:(nrow(Un))){
  if (any(Un[i,c(5,9,10)]==0)){
    Un[i,"unionXB"] <- "0"
  }else{
    Un[i,"unionXB"] <- "1"
  }
}#XGB BIMMH1聯集(任一個是0結果就是0)
table(re = Un$real,pr= Un$unionXB)
cm <- table(re = Un$real,pr= Un$unionXB)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
#"0.647619047619048 0.75609756097561 0.260869565217391"

for(i in 1:(nrow(Un))){
  if (any(Un[i,c(5,9,10)]==1)){
    Un[i,"unionXB1"] <- "1"
  }else{
    Un[i,"unionXB1"] <- "0"
  }
}#XGB BIMMH1聯集(任一個是1結果就是1)
table(re = Un$real,pr= Un$unionXB1)
cm <- table(re = Un$real,pr= Un$unionXB1)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))
#"0.761904761904762 0.975609756097561 0"




write.csv(Un,"allesb.csv",row.names = F)

#VC1=>2 => train=dm1 test= dm2
str(dm1)
colnames(dm1)
dm1 <- dm1[,c(1:14,16:22,15)]
table(dm1$dip)
table(dm2$dip)
FF2 <- factor(dip)~sbp_d+dbp_d+sbp_n+dbp_n+Age+HR+Drug_conut+BMI+Waist+Eco_child+T_pain+Gender_1+Gender_2+DM_1+DM_2+DM_3+HOS_1+HOS_0

VC1p <- RF(traindata = dm1, testdata = dm2,
        formula = FF2,sampsize=c(14,14))

VC1p$RF
VC1p$`CM of Train data`
VC1p$`Train acc sen spe`#0.73239437 0.89473684 0.07142857
VC1p$`CM of Test data`
VC1p$`Test acc sen spe`#0.7746479 0.8909091 0.3750000


#VC12=>3 => train= dm12 test=dm3
colnames(dm3)
dm12 <- dm12[,c(1:14,16:22,15)]
dm3<- dm3[,c(1:14,16:22,15)]
FF3 <- dip~sbp_d+dbp_d+sbp_n+dbp_n+visit+Age+HR+Drug_conut+BMI+Waist+Eco_child+Gender_1+Gender_2+DM_1+DM_2+DM_3+HOS_1+HOS_0

VC1p12 <- BiMMforest1(traindata = dm12, testdata = dm3,
                     formula = FF3, random = "+(1|visit)",
                     seed = seed, glmControl = "maxfun")
VC1p12$`model summary`
VC1p12$`CM of Train data`
VC1p12$`Train acc sen spe`#0.7957746 0.9732143 0.1333333
VC1p12$`CM of Test data`
VC1p12$`Test acc sen spe`#0.7605634 0.9433962 0.2222222

VC1p12.1 <- BiMMforestH1(traindata = dm12, testdata = dm3,
                      formula = FF3, random = "+(1|visit)",
                      seed = seed, glmControl = "maxfun")

VC1p12.1$`model summary`
VC1p12.1$`CM of Train data`
VC1p12.1$`Train acc sen spe`#0.8098592 0.9732143 0.2000000
VC1p12.1$`CM of Test data`
VC1p12.1$`Test acc sen spe`#0.76056338 1.00000000 0.05555556


VC1p12.2 <- BiMMforestH2(traindata = dm12, testdata = dm3,
                         formula = FF3, random = "+(1|visit)",
                         seed = seed, glmControl = "maxfun")
VC1p12.2$`model summary`
VC1p12.2$`CM of Train data`
VC1p12.2$`Train acc sen spe`#0.8028169 0.9553571 0.2333333
VC1p12.2$`CM of Test data`
VC1p12.2$`Test acc sen spe`#0.7042254 0.8679245 0.2222222

VC1p12.3 <- BiMMforestH3(traindata = dm12, testdata = dm3,
                         formula = FF3, random = "+(1|visit)",
                         seed = seed, glmControl = "maxfun")
VC1p12.3$`model summary`
VC1p12.3$`CM of Train data`
VC1p12.3$`Train acc sen spe`#0.8028169 0.9553571 0.2333333
VC1p12.3$`CM of Test data`
VC1p12.3$`Test acc sen spe`#0.7042254 0.8679245 0.2222222
