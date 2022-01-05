#從 845筆有Y的去做 =>收縮 舒張 早上 晚上 平均 =>取4個
#先抓平均完的complete case 去做模型(完整case會變多) 看效果
# case-wise 隨機切割ID 用ID所有資料去訓練 測試
# visit-wise V1 預測 V2 ； V1 V2 預測 V3
#再對X先做隨機森林插補 再抓4個平均 再跑模型 看效果 
#做完再去做 對ABPM插補 再算Y 再跑模型
library(dplyr)
dip <- read.csv("TCHCData/hbp_dip_byx.csv")
na <- which(is.na(dip$dipping.status))
dip <- dip[-na,]#845
dip[,"dip"] <- ifelse(dip$dipping.status=="Non dipper"|dip$dipping.status=="Reverse dipper",0,1)
colnames(dip)
table(dip$dip)
table(dip$dipping.status)
FourAvg <- dip[,c(2,4,6,8,9,11,12,125,126)]
sum(complete.cases(FourAvg))#830筆
FourAvg <- FourAvg[complete.cases(FourAvg),]

# case-wise 隨機切割ID 用ID所有資料去訓練 測試
#Split Train test
length(unique(FourAvg$MRN))#564人
people <-as.data.frame(unique(FourAvg$MRN))
set.seed(1226)
trainID <- as.data.frame(sample(people$`unique(FourAvg$MRN)`,nrow(people)*0.8))
colnames(trainID)[1] <- "MRN"
Train <- merge(trainID,FourAvg,by = "MRN",all.x = T)
Train <- arrange(Train,Train$Mrn_Vis)
Test <- rbind(Train, FourAvg)
Test <- Test[!(duplicated(Test) | duplicated(Test, fromLast = TRUE)), ]
write.csv(Train,file="TCHCData/4avg_case_Train.csv")#154
write.csv(Test,file="TCHCData/4avg_case_Test.csv")#676

# visit-wise 所有人的V1 預測所有人的 V2 ； V1 V2 預測 V3
#或是 有V2的人=> V1 預測 V2 有V3的人=>V1 V2 預測V3
V1 <- FourAvg[which(FourAvg$visit_HBP_Dmode==1),]#562
V2 <- FourAvg[which(FourAvg$visit_HBP_Dmode==2),]#142
V3 <- FourAvg[which(FourAvg$visit_HBP_Dmode==3),]#85
V4 <- FourAvg[which(FourAvg$visit_HBP_Dmode==4),]#39
V12 <- rbind(V1,V2)
V12 <- arrange(V12,V12$Mrn_Vis)#704
V34 <- rbind(V3,V4)
V34 <- arrange(V34,V12$Mrn_Vis)#124
write.csv(V12,file = "TCHCData/4avg_Visit_Train_V12.csv")
write.csv(V34,file = "TCHCData/4avg_Visit_Test_V34.csv")
write.csv(V3,file = "TCHCData/4avg_Visit_Test_V3.csv")

######模型訓練 BIMMRF 1iter / H1 / H3 / RF
library(rpart)
library(blme)
library(randomForest)
formula <- dip ~ HBP_d_AM_systolic+HBP_d_PM_systolic+HBP_d_AM_diastolic+HBP_d_PM_diastolic
traindata <-  V12
table(V12$dip)#0:542 1:162
traindata$dip <- as.factor(traindata$dip)
testdata <- V3 
testdata$dip <- as.factor(testdata$dip)
table(traindata$dip)
random <- "(1|HBP_d_AM_systolic)+(1|HBP_d_PM_systolic)+(1|HBP_d_AM_diastolic)+(1|HBP_d_PM_diastolic)"
BiMMforest1<-function(traindata,testdata,formula,random,seed){
  data=traindata
  initialRandomEffects=rep(0,length(data[,1]))#起始都是0
  ErrorTolerance = 0.006
  MaxIterations = 1000
  #parse formula
  Predictors <- paste(attr(terms(formula), "term.labels"), 
                      collapse = "+")
  TargetName <- formula[[2]]
  Target <- data[,toString(TargetName)]
  class(Target)#Target is factor
  #set up variables for loop
  ContinueCondition <- TRUE
  iterations <- 0
  #initial values
  #把factor0 1 as.numeric就變成1 2
  AdjustedTarget <- as.numeric(levels(Target)) - initialRandomEffects
  table(AdjustedTarget)
  oldlik<- -Inf#負無窮大
  # Make a new data frame to include all the new variables
  newdata <- data
  #compile one iteration of the BiMM forest algorithm
  newdata[, "AdjustedTarget"] <- AdjustedTarget# 1跟2
  iterations <- iterations + 1
  #build tree
  #set.seed(seed)
  set.seed(123)
  table(AdjustedTarget)
  forest <- randomForest(formula(paste(c("factor(AdjustedTarget)",Predictors),collapse = "~")),
                         data = data, method = "class")
  forestprob<-predict(forest, type = "prob")[, 2]
  RFpredictprob <- as.data.frame(forestprob)
  length(RFpredictprob[forestprob>=0.5,])
  ## Estimate New Random Effects and Errors using GLMER
  options(warn = -1)
  
  #隨機效應怎麼放是一個問題
  #(1|random)=(random intercept | random slope) 要放隨機效應變數進去
  lmefit <- tryCatch(bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                                 collapse="~"), "+(1|HBP_d_AM_systolic)",sep=""))),data=data,family=binomial,
                           control = glmerControl(optCtrl=list(maxfun=20000)
                           )),
                    error = function(cond)"skip")
  
  #if GLMM did not converge, produce NAs for accuracy statistics
  if(class(lmefit)[1]=="character"){
    #return train and test confusion matrices
    return(list(c(NA,NA,NA,NA),c(NA,NA,NA,NA),NA))
  }
  else if(!(class(lmefit)[1]=="character")){
    test.preds <- predict(forest,testdata)
    random <- c("HBP_d_AM_systolic")
    traindata1 <- cbind(traindata,random)
    train.preds <- ifelse(predict(lmefit,traindata,type="response")<.5,0,1)
    
    #format table to make sure it always has 4 entries, even if it is only 2 by 1 (0's in other spots)
    table(traindata$dip)
    t1<-table(traindata$dip,train.preds)
    trainacc <- (t1[1]+t1[4]) / sum(t1)
    train0acc <- t1[1]/(t1[1]+t1[3])
    train1acc <- t1[4]/(t1[2]+t1[4])
    t1
    t4<-table(testdata$dip,test.preds)
    testacc <- (t4[1]+t4[4]) / sum(t4)
    test0acc <- t4[1]/(t4[1]+t4[3])
    test1acc <- t4[4]/(t4[2]+t4[4])
    t4
    if(ncol(t1)==1 & train.preds[1]==1){
      t1<-c(0,0,t1[1,1],t1[2,1])
    }else if(ncol(t1)==1 & train.preds[1]==0){
      t1<-c(t1[1,1],t1[2,1],0,0)
    }
    if(ncol(t4)==1 & test.preds[1]==1){
      t4<-c(0,0,t4[1,1],t4[2,1])
    }else if(ncol(t4)==1 & test.preds[1]==0){
      t4<-c(t4[1,1],t4[2,1],0,0)
    }
    #return train and test confusion matrices, # iterations
    return(list(c(t1),c(t4),iterations))
  }
} 









