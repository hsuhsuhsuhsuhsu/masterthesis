library(joineR)
hv<-joineR::heart.valve#988*25

#刪log.lvmi na用平均插補
hv <- hv[,-which(colnames(hv)=="log.lvmi")]
sum(complete.cases(hv))#629
for(i in 1:ncol(hv)) {
  hv[ , i][is.na(hv[ , i])] <- mean(hv[ , i], na.rm = TRUE)
}
sum(complete.cases(hv))#988
#help(heart.valve)
#install.packages("lme4")

#用 lvmi分類 男的>134=>positive(1) 女的>110=>positive
hv$lvmica<-""

hv[hv$sex==0 & hv$lvmi>=134,"lvmica"]<-"1"
hv[hv$sex==0 & hv$lvmi<134,"lvmica"]<-"0"
hv[hv$sex==1 & hv$lvmi>=110,"lvmica"]<-"1"
hv[hv$sex==1 & hv$lvmi<110,"lvmica"]<-"0"
sum(is.na(hv$lvmica))

#刪掉lvmi lvmica是Y
hv <- hv[,-which(colnames(hv)=="lvmi")]
#把類別變數挑出來 看有沒有排列組合人數是0
str(hv)
#類別:sex status prenyha lv emergenc hc sten.reg.mix hs
#ftable(hv$sex,hv$status,hv$prenyha,hv$lv,hv$hs)

hv$sex<-as.factor(hv$sex)
hv$lvmica <- as.factor(hv$lvmica)
ftable(hv$sex,hv$lvmica)
#分訓練測試
smp_size <- round(nrow(hv) * 0.7, 0)
set.seed(123)
train_ind <- sample(seq_len(nrow(hv)), size = smp_size)
train <- hv[train_ind,]
test <- hv[-train_ind, ]

#load libraries
library(rpart)
library(blme)
library(randomForest)
###############################################################################
#variable names
#traindata: name of the training dataset
#testdata: name of the test dataset
#formula: formula for fixed variables with binary outcome
#example:
comagradelow1~Sex+Ethnicity+Age+ALT+AST+Bilirubin+Creat+Phosphate+Lactate+plate
lets+ammonia+inr+pressors+rrt
#random: name of the random clustering variable
###############################################################################
#BiMM forest with one iteration
#note: requires training and test data with no missing values
#random<-c("age","time")
#f <- lvmica~sex+age+time+grad+ef+bsa
#BiMMforest1(train,test,f,random,123)
BiMMforest1<-function(traindata,testdata,formula,random,seed){
  #set up variables for Bimm method
  #先把原本的井字號 
  #data=traindata
  data=train
  initialRandomEffects=rep(0,length(data[,1]))#起始都是0
  ErrorTolerance=0.006
  MaxIterations=1000
  #parse formula
  #Predictors<-paste(attr(terms(formula),"term.labels"),collapse="+")
  formula <- lvmica~sex+age+time+grad+ef+bsa
  str(formula)
  Predictors<-paste(attr(terms(formula),"term.labels"),collapse="+")
  TargetName<-formula[[2]]
  Target<-data[,toString(TargetName)]
  class(Target)#Target is factor
  #set up variables for loop
  ContinueCondition<-TRUE
  iterations<-0
  #initial values
  #把factor0 1 as.numeric就變成1 2
  AdjustedTarget<-as.numeric(Target)-initialRandomEffects
  oldlik<- -Inf#負無窮大
  # Make a new data frame to include all the new variables
  newdata <- data
  #compile one iteration of the BiMM forest algorithm
  newdata[,"AdjustedTarget"] <- AdjustedTarget# 1跟2
  iterations <- iterations+1
  #build tree
  #set.seed(seed)
  set.seed(123)
  factor(AdjustedTarget)
  forest <- randomForest(formula(paste(c("factor(AdjustedTarget)",
                                         Predictors),collapse = "~")),
                         data = data, method = "class")
  forestprob<-predict(forest,type="prob")[,2]
  RFpredictprob <- as.data.frame(forestprob)
  ## Estimate New Random Effects and Errors using GLMER
  options(warn=-1)
  
  random<-c("time","age")
  #隨機效應怎麼放是一個問題
  #(1|random)=(random intercept | random slope) 要放隨機效應變數進去
  lmefit <-tryCatch(bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
          collapse="~"), "+(1|time) +(1|age)",sep=""))),data=data,family=binomial,
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
    traindata <- cbind(traindata,random)
    train.preds <- ifelse(predict(lmefit,traindata,type="response")<.5,0,1)
    
    #format table to make sure it always has 4 entries, even if it is only 2 by 1 (0's in other spots)
t1<-table(traindata$lvmica,train.preds)
trainacc <- (t1[1]+t1[4]) / sum(t1)
train0acc <- t1[1]/(t1[1]+t1[3])
train1acc <- t1[4]/(t1[2]+t1[4])
t1
t4<-table(testdata$lvmica,test.preds)
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


