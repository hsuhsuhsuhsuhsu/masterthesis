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

se <- predict(pr1,dm1te)
cm <- table(re = dm1te$dip,se)
table(test$dip)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))






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
