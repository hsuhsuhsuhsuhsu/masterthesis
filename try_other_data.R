#### CCtr CCte####
CCTr <- read.csv("TCHCData/CASE_COV_hos_Train.csv")#838
CCTr <- CCTr[,-1]
CCTe <- read.csv("TCHCData/CASE_COV_hos_Test.csv")#210
CCTe <- CCTe[,-1]
mydata <- rbind(CCTr,CCTe)
my <- mydata

#####DEMOGRAPHICS####
de <- read.csv("TCHCData/uDE.csv")
de <- de[,-c(1,3,4,5,6)]
de <- de[,-which(colnames(de) %in% "DM")]
de <- de[,-which(colnames(de) %in% "CV_risk_Non")]
de <- de[,-which(colnames(de) %in% "CV_risk_HF")]
de <- de[,-which(colnames(de) %in% "CV_risk_ACS_3mon")]
de <- de[,-which(colnames(de) %in% "CV_risk_Surg_6_mon")]
de <- de[,-which(colnames(de) %in% "CV_risk_RHF_LD")]
de <- de[,-which(colnames(de) %in% "CV_risk_DM")]
de <- de[,-which(colnames(de) %in% "CV_risk_CKD")]
de <- de[,-which(colnames(de) %in% "CV_risk_CLD")]
de <- de[,-which(colnames(de) %in% "CV_risk_Dys_L")]
de <- de[,-which(colnames(de) %in% "CV_risk_Other")]
de <- de[,-c(2,3,4)]
de <- de[,-which(colnames(de) %in% "CV_risk_Other_D")]



#####Family History####
fh <- read.csv("TCHCData/cFH.csv")
fh <- fh[,-c(1,3,4)]
table(fh$Kinship)
fh <- fh[which(fh$Kinship %in% 1:2),]
fh <- fh[,-c(7,11)]
b <- fh[,c(1,2)]
c <- fh[-which(duplicated(b)),]
fh <- c
#start
fh$Kinship <- as.factor(fh$Kinship)
levels(fh$Kinship) <- c("Dad", "Mom")
new_fh <- data.frame(matrix(0, ncol = 2 * ncol(fh), nrow = 0))
paste("Dad", colnames(fh), sep = "_")
paste("Mom", colnames(fh), sep = "_")
colnames(new_fh)[seq(1, ncol(new_fh), 2)] <- paste("Dad", colnames(fh), sep = "_")
colnames(new_fh)[seq(2, ncol(new_fh), 2)] <- paste("Mom", colnames(fh), sep = "_")
new_fh <- new_fh[,-1]
colnames(new_fh)[1] <- "MRN"
MomIndex <- seq(3, ncol(new_fh), 2)
DadIndex <- seq(2, ncol(new_fh), 2)
for(i in unique(fh$MRN)){
  temp <- data.frame(matrix(NA, ncol = ncol(new_fh), nrow = 1))
  colnames(temp) <- colnames(new_fh)
  test <- fh[fh$MRN == i, ]
  temp[1] <- i
  if(nrow(test) == 1){
    if(test$Kinship == "Dad"){
      temp[DadIndex[1]] <- 1
      temp[MomIndex[1]] <- 0
      temp[DadIndex[-1]] <- test[-(1:2)]
    }else{
      temp[MomIndex[1]] <- 1  
      temp[DadIndex[1]] <- 0
      temp[MomIndex[-1]] <- test[-(1:2)]
    }
  }else if(nrow(test) == 2){#mom and dad both exist
    temp[c(DadIndex[1], MomIndex[1])] <- 1
    temp[DadIndex[-1]] <- test[test$Kinship == "Dad", -(1:2)]
    temp[MomIndex[-1]] <- test[test$Kinship == "Mom", -(1:2)]
  }else{
    print("Something error")
    print(test)
    break
  }
  new_fh <- rbind(new_fh, temp)
}

View(new_fh)
nfh <- read.csv("TCHCData/ass.csv")
nfh <- nfh[,-1]

#### Visit Treatment####
vt <- read.csv("TCHCData/cVT.csv")
vt <- vt[,-c(1,2)]
Nvt <- vt[,c(1,2,4,50:63)]

#### Visit VitalSign####
vs <- read.csv("TCHCData/VVs.csv")
vs <- vs[,-c(1,4)]
vs <- vs[,-58]
##### merge & write####
try.de <- de
try.vt <- Nvt
try.vs <- vs
try.fh <- nfh
dim(mydata)
dim(try.de)
m1 <- merge(mydata,try.de,by = "MRN",all.x = T)
dim(m1)
m2 <- merge(m1,try.vt,by = "Mrn_Vis",all.x = T)
dim(m2)
m3 <- merge(m2,try.vs,by = "Mrn_Vis",all.x = T)
dim(m3)
mydata <- m3
colnames(mydata)
mydata <- mydata[,-c(75,76,91,92)]
mydata <- mydata[,-20]
colnames(mydata)[2]<-"MRN"
dim(mydata)#1048*142
for(r in 1:nrow(mydata)){
  for(c in 1:ncol(mydata)){
    if(!is.na(mydata[r,c])){
      if(mydata[r,c]=="<NA>"){
        mydata[r,c] <- NA
      }else if(mydata[r,c]==""){
        mydata[r,c] <- NA
      }else if(mydata[r,c]==" "){
        mydata[r,c] <- NA
      }else if(mydata[r,c]=="{}"){
        mydata[r,c] <- NA
      }else if(mydata[r,c]=="999"){
        mydata[r,c] <- NA
      }
    }
  }
}
del <- which(colnames(mydata)%in%c("Free_walk_Day","H_activity_week","H_activity_D"))
mydata <-mydata[,-del] 
dim(mydata)
m4 <- merge(mydata,try.fh,by = "MRN",all.x = T)
dim(m4)
mydata <- m4



cou = 0
cc = NULL
for (c in 1:ncol(mydata)) {
  new <- mydata
  per <- nrow(mydata)*30/100#314
  if(sum(is.na(mydata[,c])) > per){
    #print(colnames(mydata)[c])
    cn <- colnames(mydata)[c]
    cou =cou+1
    cc <- c(cc,cn)
  }
}
print(cou)
na30 <- which(colnames(mydata)%in%cc)
data.30 <- mydata[,-na30]
y.col <- which(colnames(data.30)%in% "dip")
data.301 <- data.30[,c(1:(y.col-1),(y.col+1):ncol(data.30),y.col)]
dim(data.301)#1048*72
write.csv(data.301,"TCHCData/COV_NA_less30.csv")
write.csv(mydata,"TCHCData/COV_Case_ALL.csv")
##### naplot ####
library(visdat)
library(ggplot2)

dim(data.30)
df <- data.30[,1:36]
p <- vis_miss(df, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)
df1 <- data.30[,37:72]
p1 <- vis_miss(df1, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p1), top = "",
                        nrow = 1, ncol = 1)
#### try rf var importance####
library(randomForest)
DF <- read.csv("TCHCData/COV_NA_less30.csv")
DF <- DF[,-1]
DF <- DF[,-which(colnames(DF)%in% c("BH","BW"))]
#DF <- DF[,-which(colnames(DF)%in% c("office_peri_L_sys","office_peri_R_sys",
#  "office_peri_L_dia","office_peri_R_dia",
#  "office_central_sys","office_central_dia",
#  "office_peri_L_remark","office_peri_R_remark",
#  "office_central_remark"))]

#去掉不用的變數 全放進隨機森林 給他挑重要性
del <- which(colnames(DF)%in% c("MRN","Mrn_Vis","dipping.status","visit","hos"))
DF <- DF[,-del]#1048*65
#aa <- DF[,c(1:9,67)]
comp <- DF[which(complete.cases(DF)),]#368*65

rr <- randomForest(factor(dip)~.,data = comp, method = "class")
rr$confusion
rr
rr$importance

tr <- sample(1:nrow(comp),size = nrow(comp)*0.8)
Tr <- comp[tr,]
te <- comp[-tr,]
rr1 <- randomForest(factor(dip)~.,data = Tr
                    , method = "class",mtry = 10)
rr1
rr1$importance
pr <- predict(rr1,te)
table(te$dip,pr)

#### random /grid search mtry + 測試 ####
library(caret)#for RF only mtry can be tuned by caret
#reasons :its effect on the final accuracy and that it must be found empirically for a dataset.
# Random Search
seed = 23
x <- Tr
dataset = Tr
metric <- "Accuracy"
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3,
                        search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(factor(dip)~., data=dataset,
                   method="rf", metric=metric,
                   tuneLength=15, trControl=control)
print(rf_random)#mtry = 8
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
print(rf_gridsearch)#mtry = 5
plot(rf_gridsearch)

#mtry = 5
rr2 <- randomForest(factor(dip)~.,data = Tr,
                    method = "class",mtry = 5)
rr2#OOB estimate of  error rate:  4.42%
rr2$importance
pr2 <- predict(rr2,te)
table(te$dip,pr2)

#mtry = 8
rr3 <- randomForest(factor(dip)~.,data = Tr,
                    method = "class",mtry = 8)
rr3#OOB estimate of  error rate: 5.1%
pr3 <- predict(rr3,te)
table(te$dip,pr3)


#mtry = 5 變數重要性前10名+原本的變數
var.10 <- c("BMI","office_peri_L_sys","HR",
            "office_peri_L_dia","Age","Waist",
            "anti_HP","sbp","dbp","Walk_TM_week","Drug_conut","HOS",
            "Gender","DM","time","dip")

CC1 <- DF[,which(colnames(DF) %in% var.10)]#1048*16

p <- vis_miss(CC1, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)
CC1.cmp <- CC1[which(complete.cases(CC1)),]#764*16
tr1 <- sample(1:nrow(CC1.cmp),size = nrow(CC1.cmp)*0.8)
Tr1 <- CC1.cmp[tr1,]#611
te1 <- CC1.cmp[-tr1,]#153
table(Tr1$dip)
table(CC1.cmp$dip)
table(te1$dip)
set.seed(23)
rr4 <- randomForest(factor(dip)~.,data = Tr1 ,
                    method = "class",mtry = 8)
rr4#OOB estimate of  error rate: 7.69%
rr4$importance
pr4 <- predict(rr4,te1)
table(te1$dip,pr4)
#pr4
#    0   1
#0  25   7
#1   2 119
write.csv(Tr1,"TCHCData/select_Tr.csv")
write.csv(te1,"TCHCData/select_Te.csv")

str(CC1.cmp)
CC1.cmp$Walk_TM_week<-as.factor(CC1.cmp$Walk_TM_week)
CC1.cmp$HOS<-as.factor(CC1.cmp$HOS)
CC1.cmp$Drug_conut<-as.factor(CC1.cmp$Drug_conut)
CC1.cmp$Gender<-as.factor(CC1.cmp$Gender)
tr2 <- sample(1:nrow(CC1.cmp),size = nrow(CC1.cmp)*0.8)
Tr2 <- CC1.cmp[tr2,]#611
te2 <- CC1.cmp[-tr2,]#153
rr4 <- randomForest(factor(dip)~.,data = Tr2 ,
                    method = "class",mtry = 8)
rr4#OOB estimate of  error rate: 7.69%
rr4$importance
pr4 <- predict(rr4,te2)
table(te2$dip,pr4)



#Grid Search
seed = 23
dataset = Tr1
metric <- "Accuracy"
control <- trainControl(method="repeatedcv",
                        number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:15))
rf_gridsearch <- train(factor(dip)~., data=dataset,
                       method="rf", metric=metric,
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)#mtry = 13
plot(rf_gridsearch)
#### 把所有資料集 加上這些變數####
df <- read.csv("TCHCData/COV_NA_less30.csv")
var <- c("MRN","BMI","office_peri_L_sys","HR",
            "office_peri_L_dia","Age","Waist",
            "anti_HP","sbp","dbp","Walk_TM_week","Drug_conut","HOS",
            "Gender","DM","time","dip")
ndf <- df[,which(colnames(df) %in% var)] 
write.csv(ndf,"TCHCData/CASE_COV_select_wNA.csv")
dim(ndf)
colnames(ndf)
#### Visit ####
VCTr.12 <- read.csv("TCHCData/VISIT_COV_hos_V12Train.csv")#284
VCTr.12 <- VCTr.12[,-1]
VCTe.3 <- read.csv("TCHCData/VISIT_COV_hos_V3Test.csv")#142
VCTe.3 <- VCTe.3[,-1]

mydata <- rbind(VCTr.12,VCTe.3)


dim(mydata)
dim(try.de)
m1 <- merge(mydata,try.de,by = "MRN",all.x = T)
dim(m1)
m2 <- merge(m1,try.vt,by = "Mrn_Vis",all.x = T)
dim(m2)
m3 <- merge(m2,try.vs,by = "Mrn_Vis",all.x = T)
dim(m3)
m4 <- merge(m3,try.fh,by = "MRN",all.x = T)
dim(m4)
mydata <- m4
mydata <- mydata[,-c(1,76,92)]
colnames(mydata)[2] <- "MRN"
dim(mydata)

cou = 0
cc = NULL
for (c in 1:ncol(mydata)) {
  new <- mydata
  per <- nrow(mydata)*30/100#128
  if(sum(is.na(mydata[,c])) > per){
    #print(colnames(mydata)[c])
    cn <- colnames(mydata)[c]
    cou =cou+1
    cc <- c(cc,cn)
  }
}
print(cou)

Visit_na30 <- which(colnames(mydata)%in%cc)
V.30 <- mydata[,-Visit_na30]

V.30 <- V.30[,-which(colnames(V.30)%in% c("BH","BW"))]

del <- which(colnames(V.30)%in% c("MRN","Mrn_Vis","dipping.status","hos"))
V.30 <- V.30[,-del]
dim(V.30)#426*51
colnames(V.30)
which(colnames(V.30)%in% "dip")
V.30 <- V.30[,c(1:10,12:51,11)]
Vcomp <- V.30[which(complete.cases(V.30)),]#330*51

VV <- randomForest(factor(dip)~.,data = aa
                   , method = "class")
VV$confusion
VV
VV$importance
aa <- Vcomp
str(aa)
x <- c(HR,sbp,dbp,Waist,BMI,Age)
x1 <- c(visit,Eco_child,T_pain,Drug_conut)
aa$visit <- as.factor(aa$visit)
aa$Eco_child <- as.factor(aa$Eco_child)
aa$T_pain <- as.factor(aa$T_pain)
aa$Drug_conut <- as.factor(aa$Drug_conut)
aa$BMI <- as.numeric(aa$BMI)
aa$Waist <- as.numeric(aa$Waist)
aa$Walk_TM_week <- as.factor(aa$Walk_TM_week)
var <- c("BMI","office_peri_L_sys","HR",
         "office_peri_L_dia","Age","Waist",
         "anti_HP","sbp","dbp","Walk_TM_week","Drug_conut","HOS",
         "Gender","DM","time","dip")

var1 <- c("MRN","Mrn_Vis","time","HR","sbp","dbp","visit",
          "Waist","Eco_child","Drug_conut",
          "Age","BMI","T_pain","HOS","dip","Gender","DM")

nmd <- mydata[,which(colnames(mydata) %in% var1)]
nmd <- nmd[,c(1:12,14:17,13)]

c <- nmd[which(complete.cases(nmd)),]
c1 <-c[which(c$visit %in% 1:2),]
c2 <-c[which(c$visit %in% 3),] 
VV <- randomForest(factor(dip)~.,data = c1
                   , method = "class")
VV
pr <- predict(VV,c2)
table(c2$dip,pr)
plot(VV)
p <- vis_miss(nmd, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)
write.csv(nmd,"TCHCData/Visit_COV_select_wNA.csv")
colnames(nmd)
dim(nmd)

#### imputation ####
CC <- read.csv("TCHCData/CASE_COV_select_wNA.csv")
CC<- CC[,-1]
p <- vis_miss(CC, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)

tmp <- data.frame(CC[,1])
df <- CC[,-1]
colnames(df)
df$sbp <- as.numeric(df$sbp)
df$dbp <- as.numeric(df$dbp)
df$HR <- as.numeric(df$HR)
df$BMI <- as.numeric(df$BMI)
df$Waist <- as.numeric(df$Waist)
df$office_peri_L_sys <- as.numeric(df$office_peri_L_sys)
df$office_peri_L_dia <- as.numeric(df$office_peri_L_dia)

df$Gender <- as.factor(df$Gender)
df$Drug_conut <- as.factor(df$Drug_conut)
df$DM <- as.factor(df$DM)
df$HOS <- as.factor(df$HOS)
df$dip <- as.factor(df$dip)
df$time <- as.factor(df$time)
df$Walk_TM_week <- as.factor(df$Walk_TM_week)
df$anti_HP <- as.factor(df$anti_HP)
str(df)
RF.impute.HH <- missForest(df,verbose=T)#iter =8 
rfimp <- RF.impute.HH$ximp
vis_miss(rfimp, show_perc = F) + coord_flip()
rfimp.proc <- cbind(tmp,rfimp)#727*14
colnames(rfimp.proc)[1]<-"MRN"
write.csv(rfimp.proc,file = "TCHCData/CASE_COV_select_RFimp.csv ")

source("DataProcFunctions.r")
d <- TrainTest(data = rfimp.proc, VisitOrCase = "Case",
                      nfixed = T, Train = NULL,
                      Test = NULL, seed = NULL,
                      removeCategory = NULL, Trainper = 0.8)
d1 <- d$`Training set`
d2 <- d$`Test set`
write.csv(d1,"TCHCData/CASE_COV_select_Train.csv")#838

write.csv(d2,"TCHCData/CASE_COV_select_Test.csv")#210
