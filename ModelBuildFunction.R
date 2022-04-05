#### library setup ####
#library(rpart)
library(blme)
library(randomForest)
library(caret)
#### BiMMforest1 ####
#作用:一次BIMMRF
#參數: traindata=>訓練資料 testdata=>測試資料 seed=>隨機種子
#formula=>fixed effect 公式
#random=> random effect 公式 
#return: 訓練/測試 的混淆矩陣和準確度敏感度特異度
#隨機森林判成1數量 / 混合模型的結果
BiMMforest1<-function(traindata = NULL, testdata = NULL,
                      formula = NULL, random = "+(1|MRN)",
                      seed = NULL, glmControl = "maxfun"){
  results <- NULL
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
  AdjustedTarget <- as.numeric(Target) - initialRandomEffects
  table(AdjustedTarget)
  oldlik<- -Inf #負無窮大
  # Make a new data frame to include all the new variables
  newdata <- data
  #compile one iteration of the BiMM forest algorithm
  newdata[, "AdjustedTarget"] <- AdjustedTarget
  iterations <- iterations + 1
  #build tree
  set.seed(seed)
  table(AdjustedTarget)
  forest <- randomForest(formula(paste(c("factor(AdjustedTarget)",Predictors),collapse = "~")),
                         data = data, method = "class")
  
  forestprob <- predict(forest, type = "prob")[, 2]
  RFpredictprob <- as.data.frame(forestprob)
  RFpredict1 <- length(RFpredictprob[forestprob>=0.5,])
  results[["forestprob >= 0.5"]] <- RFpredict1
  results[["RF"]] <- forest
  ## Estimate New Random Effects and Errors using GLMER
  options(warn = -1)
  
  #隨機效應怎麼放是一個問題
  if (glmControl == "maxfun"){
    #(1|random)=(random intercept | random slope) 要放隨機效應變數進去
    lmefit <- tryCatch(bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                                    collapse="~"),random,sep=""))),data=data,family=binomial,
                              control = glmerControl(optCtrl=list(maxfun=200000)
                              )), error = function(cond)"skip")
    
  }else if (glmControl == "tolPwrss"){
    lmefit <- bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                           collapse="~"),random,sep=""))),data=data,family=binomial,
                     control = glmerControl(optimizer ="Nelder_Mead",tolPwrss=1e-3))
  }else if (glmControl == "optimx"){
    lmefit <- bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                           collapse="~"),random,sep=""))),data=data,family=binomial,
                     control = glmerControl(optimizer ='optimx', optCtrl=list(method='L-BFGS-B'),tolPwrss=1e-3))
  }
  
  
  results[["model summary"]] <- summary(lmefit)
  results[["model"]] <- lmefit
  
  #if GLMM did not converge, produce NAs for accuracy statistics
  if(class(lmefit)[1]=="character"){
    return("GLMM did not converge")
  }
  else if(!(class(lmefit)[1]=="character")){
    test.preds <- predict(forest,testdata)
    results[["test.preds"]] <- test.preds
    
    testdata1 <- cbind(testdata,random)
    test.lme.preds <- ifelse(predict(lmefit,testdata1,re.form=NA,type="response")<.5,0,1)
    results[["test.lme.preds"]] <- test.lme.preds[1:nrow(testdata)]
    #results[["aaaaa"]] <- lmefit
    RFtrain.preds <- predict(forest,traindata)
    
    traindata1 <- cbind(traindata,random)
    train.preds <- ifelse(predict(lmefit,traindata1,type="response")<.5,0,1)
    results[["train.preds"]] <- train.preds
    
    
    #format table to make sure it always has 4 entries, even if it is only 2 by 1 (0's in other spots)
    t1 <- table(traindata[,ncol(traindata)],train.preds)
    trainacc <- (t1[1]+t1[4]) / sum(t1)
    train0acc <- t1[1]/(t1[1]+t1[3])
    train1acc <- t1[4]/(t1[2]+t1[4])
    results[["CM of Train data"]] <- t1
    results[["Train acc sen spe"]] <- c(trainacc,train1acc,train0acc)
    
    t4 <- table(testdata[,ncol(testdata)],test.preds)
    testacc <- (t4[1]+t4[4]) / sum(t4)
    test0acc <- t4[1]/(t4[1]+t4[3])
    test1acc <- t4[4]/(t4[2]+t4[4])
    results[["CM of Test data"]] <- t4
    results[["Test acc sen spe"]] <- c(testacc,test1acc,test0acc)
    
    t2 <-table(testdata[,ncol(testdata)],test.lme.preds[1:nrow(testdata)])
    lme.testacc <- (t2[1]+t2[4]) / sum(t2)
    lme.test0acc <- t2[1]/(t2[1]+t2[3])
    lme.test1acc <- t2[4]/(t2[2]+t2[4])
    results[["lme.CM of Test data"]] <- t2
    results[["lme.Test acc sen spe"]] <- c(lme.testacc,lme.test1acc,lme.test0acc)
    return(results)
  }
} 

#### BiMMforest H1 ####
#作用:迭代BIMMRF with H1 function
#參數: traindata=> 訓練資料 testdata=>測試資料 seed=>隨機種子
#formula=> fixed effect 公式
#random=> random effect 公式 
#return:訓練/測試 的混淆矩陣和準確度敏感度特異度
#隨機森林判成1數量 / 混合模型的結果 / 迭代次數 / RF OOBError
#程式運行時間
BiMMforestH1<-function(traindata = NULL, testdata = NULL,
                       formula = NULL, random = "+(1|MRN)",
                       seed = NULL, glmControl = "maxfun"){
  results <- NULL
  data = traindata
  initialRandomEffects = rep(0,length(data[,1]))
  ErrorTolerance = 0.006
  MaxIterations = 100
  
  Predictors <- paste(attr(terms(formula),"term.labels"),collapse="+")
  TargetName <- formula[[2]]
  Target <- data[,toString(TargetName)]
  #set up variables for loop
  ContinueCondition <- TRUE
  iterations <- 0
  #initial values
  AdjustedTarget <- as.numeric(Target)-initialRandomEffects
  oldlik <- -Inf
  
  # Make a new data frame to include all the new variables
  newdata <- data
  shouldpredict = TRUE
  starttime <- Sys.time()
  while(ContinueCondition){
    # Current values of variables
    newdata[,"AdjustedTarget"] <- AdjustedTarget
    iterations <- iterations + 1
    print(iterations)
    #build tree
    set.seed(seed)
    forest <- randomForest(formula(paste(c("factor(AdjustedTarget)",
                                           Predictors),collapse = "~")),
                           data = data, method = "class")
    forestprob <- predict(forest,type="prob")[,2]
    RFpredictprob <- as.data.frame(forestprob)
    RFpredict1 <- length(RFpredictprob[forestprob>=0.5,])
    results[["forestprob >= 0.5"]] <- RFpredict1
    results[["RF"]] <- forest
    if(glmControl == "maxfun"){
      lmefit <- tryCatch(bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                                      collapse="~"), random, sep=""))),
                                data=data,family=binomial,
                                control=glmerControl(optCtrl=list(maxfun=20000))),
                         error=function(cond)"skip")
    }else if(glmControl == "tolPwrss"){
    # Estimate New Random Effects and Errors using BLMER
    lmefit <- bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                           collapse="~"),random,sep=""))),data=data,family=binomial,
                     control = glmerControl(tolPwrss=1e-3))
    }
    # Get the likelihood to check on convergence
    if(!(class(lmefit)[1]=="character")){
      newlik <- logLik(lmefit)#loglikelihood
      
      ContinueCondition <- (abs(newlik-oldlik)>ErrorTolerance & iterations < MaxIterations)
      oldlik <- newlik 
      
      # Extract random effects to make the new adjusted target
      logit <- forestprob #rf機率
      logit2 <- exp(predict(lmefit,re.form=NA))/(1+exp(predict(lmefit,re.form=NA))) 
      results[["logit"]] <- logit
      results[["logit2"]] <- logit2
      # logit ()
      #population level effects
      AllEffects <- (logit+logit2)/2 #average them =>paper上的qit
      
      #h1 update
      
      AdjustedTarget <- ifelse(as.numeric(Target) + AllEffects >0.5,1,0)
      if (any(is.na(AdjustedTarget))){
        AdjustedTarget[which(is.na(AdjustedTarget))] <- 1
      }
     
    }else{ 
      ContinueCondition <- FALSE 
    
    }
    
    #if all of the binary outcomes are the same then get out of loop
    if(min(AdjustedTarget) == max(AdjustedTarget)){
      ContinueCondition <- FALSE
      shouldpredict = FALSE
      return(print("all of the binary outcomes are the same"))
    }
  }
  endtime <- Sys.time()
  times <- endtime-starttime
  results[["iter"]] <- iterations
  results[["run time"]] <- times
  
  if(class(lmefit)[1]=="character" | shouldpredict==FALSE){
    return(print("GLMM did not converge or all of the outcomes are the same"))
  }else if(!(class(lmefit)[1]=="character")){
    results[["model summary"]] <- summary(lmefit)
    
    test.preds <- predict(forest,testdata)#為甚麼測試只有丟森林
    traindata1 <- cbind(traindata,random)
    train.preds <- ifelse(predict(lmefit,traindata1,type="response")<.5,0,1)
    results[["train.preds"]] <- train.preds
    results[["test.preds"]] <- test.preds
    
    testdata1 <- cbind(testdata,random)
    test.lme.preds <- ifelse(predict(lmefit,testdata1,re.form=NULL,type="response")<.5,0,1)
    results[["test.lme.preds"]] <- test.lme.preds[1:nrow(testdata)]
    
    t1 <- table(traindata[,ncol(traindata)],train.preds)
    trainacc <- (t1[1]+t1[4]) / sum(t1)
    train0acc <- t1[1]/(t1[1]+t1[3])
    train1acc <- t1[4]/(t1[2]+t1[4])
    results[["CM of Train data"]] <- t1
    results[["Train acc sen spe"]] <- c(trainacc,train1acc,train0acc)
    
    t4 <- table(testdata[,ncol(testdata)],test.preds)
    testacc <- (t4[1]+t4[4]) / sum(t4)
    test0acc <- t4[1]/(t4[1]+t4[3])
    test1acc <- t4[4]/(t4[2]+t4[4])
    results[["CM of Test data"]] <- t4
    results[["Test acc sen spe"]] <- c(testacc,test1acc,test0acc)
    
    t2 <-table(testdata[,ncol(testdata)],test.lme.preds[1:nrow(testdata)])
    lme.testacc <- (t2[1]+t2[4]) / sum(t2)
    lme.test0acc <- t2[1]/(t2[1]+t2[3])
    lme.test1acc <- t2[4]/(t2[2]+t2[4])
    results[["lme.CM of Test data"]] <- t2
    results[["lme.Test acc sen spe"]] <- c(lme.testacc,lme.test1acc,lme.test0acc)
    #return train and test confusion matrices, 
    # iterations, and RF OOBER
    RfOober <- mean(forest$err.rate[,1])
    results[["RF OOBerror"]] <- RfOober
    return(results)
  }
  
}


#### BiMMforest H3 ####
#作用:迭代BIMMRF with H3 function
#參數: traindata=> 訓練資料 testdata=>測試資料 seed=>隨機種子
#formula=> fixed effect 公式
#random=> random effect 公式 
#return:訓練/測試 的混淆矩陣和準確度敏感度特異度
#隨機森林判成1數量 / 混合模型的結果 / 迭代次數 / RF OOBError
#程式運行時間
BiMMforestH3 <- function(traindata = NULL, testdata = NULL,
                         formula = NULL, random = "+(1|MRN)",
                         seed = NULL, glmControl = "maxfun" ) {
  results <- NULL
  #set up variables for Bimm method
  data = traindata
  initialRandomEffects = rep(0, length(data[, 1]))
  ErrorTolerance = 0.006
  MaxIterations = 100
  #parse formula
  Predictors <- paste(attr(terms(formula), "term.labels"), collapse = "+")
  TargetName <- formula[[2]]
  Target <- data[, toString(TargetName)]
  #set up variables for loop
  ContinueCondition <- TRUE
  iterations <- 0
  #initial values
  AdjustedTarget <- as.numeric(Target) - initialRandomEffects
  oldlik <- -Inf
  # Make a new data frame to include all the new variables
  newdata <- data
  shouldpredict = TRUE
  starttime <- Sys.time()
  while (ContinueCondition) {
    # Current values of variables
    newdata[, "AdjustedTarget"] <- AdjustedTarget
    iterations <- iterations + 1
    print(iterations)
    #build tree
    set.seed(seed)
    forest <- randomForest(formula(paste(c("factor(AdjustedTarget)",
        Predictors), collapse = "~")),data = data, method = "class")
    forestprob <- predict(forest, type = "prob")[, 2]
    results[["RF"]] <- forest
    ## Estimate New Random Effects and Errors using BLMER
    if (glmControl == "maxfun"){
      lmefit <-tryCatch(bglmer(formula(c(paste(paste(c(toString(TargetName), 
                                               "forestprob"), collapse = "~"),
                                               random, sep =""))),data = data,
                               family = binomial,
                               control = glmerControl(optCtrl = list(maxfun = 20000))),
                        error = function(cond)"skip")
    }else if(glmControl == "tolPwrss"){
      lmefit <- bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                             collapse="~"),random,sep=""))),data=data,family=binomial,
                       control = glmerControl(tolPwrss=1e-3))
    }
    # Get the likelihood to check on convergence
    if (!(class(lmefit)[1] == "character")) {
      newlik <- logLik(lmefit)
      ContinueCondition <- (abs(newlik - oldlik) > ErrorTolerance & iterations < MaxIterations)
      oldlik <- newlik
      # Extract random effects to make the new adjusted target
      logit <- forestprob
      logit2 <- exp(predict(lmefit, re.form = NA)) / (1 + exp(predict(lmefit, re.form = NA)))
      results[["logit"]] <- logit
      results[["logit2"]] <- logit2
      #population level effects
      AllEffects <- (logit + logit2) / 2 #average them
      #split function h3
      if (any(is.nan(AllEffects))){
        AdjustedTarget[which(is.nan(AllEffects))] <- 1
      }
      for (k in 1:length(AllEffects)) {
        if (as.numeric(Target[k]) + AllEffects[k] - 1 < .5) {
          AdjustedTarget[k] = 0
        }else if (as.numeric(Target[k]) + AllEffects[k] - 1 > 1.5) {
          AdjustedTarget[k] = 1
        }else{
          #generate random probability coin flip based on AllEffects (q notation in paper)
          set.seed(seed)
          AdjustedTarget[k] <- rbinom(1, 1, AllEffects[k])
        }
      }
    }else{
      ContinueCondition <- FALSE
    }
    #if all of the binary outcomes are the same then get out of loop
    if (min(AdjustedTarget) == max(AdjustedTarget)) {
      ContinueCondition <- FALSE
      shouldpredict = FALSE
      return(print("all of the binary outcomes are the same"))
    }
  }
  endtime <- Sys.time()
  times <- endtime - starttime
  results[["iter"]] <- iterations
  results[["run time"]] <- times
  if (class(lmefit)[1] == "character" | shouldpredict == FALSE) {
    return(print("GLMM did not converge or all of the outcomes are the same"))
  } else if (!(class(lmefit)[1] == "character")) {
    results[["model summary"]] <- summary(lmefit)
    #predictions
    test.preds <- predict(forest, testdata)
    traindata1 <- cbind(traindata, random)
    train.preds <- ifelse(predict(lmefit, traindata1, type = "response") < .5, 0, 1)
    results[["train.preds"]] <- train.preds
    results[["test.preds"]] <- test.preds
    
    t1 <- table(traindata[,ncol(traindata)],train.preds)
    trainacc <- (t1[1]+t1[4]) / sum(t1)
    train0acc <- t1[1]/(t1[1]+t1[3])
    train1acc <- t1[4]/(t1[2]+t1[4])
    results[["CM of Train data"]] <- t1
    results[["Train acc sen spe"]] <- c(trainacc,train1acc,train0acc)
    
    t4 <- table(testdata[,ncol(testdata)],test.preds)
    testacc <- (t4[1]+t4[4]) / sum(t4)
    test0acc <- t4[1]/(t4[1]+t4[3])
    test1acc <- t4[4]/(t4[2]+t4[4])
    results[["CM of Test data"]] <- t4
    results[["Test acc sen spe"]] <- c(testacc,test1acc,test0acc)
    
    testdata1 <- cbind(testdata,random)
    test.lme.preds <- ifelse(predict(lmefit,testdata1,re.form=NULL,type="response")<.5,0,1)
    results[["test.lme.preds"]] <- test.lme.preds[1:nrow(testdata)]
    t2 <-table(testdata[,ncol(testdata)],test.lme.preds[1:nrow(testdata)])
    lme.testacc <- (t2[1]+t2[4]) / sum(t2)
    lme.test0acc <- t2[1]/(t2[1]+t2[3])
    lme.test1acc <- t2[4]/(t2[2]+t2[4])
    results[["lme.CM of Test data"]] <- t2
    results[["lme.Test acc sen spe"]] <- c(lme.testacc,lme.test1acc,lme.test0acc)
    #return train and test confusion matrices, 
    # iterations, and RF OOBER
    RfOober <- mean(forest$err.rate[,1])
    results[["RF OOBerror"]] <- RfOober
    return(results)
  
  }
} 


#### BiMMforest H2 ####
#作用:迭代BIMMRF with H1 function
#參數: traindata=> 訓練資料 testdata=>測試資料 seed=>隨機種子
#formula=> fixed effect 公式
#random=> random effect 公式 
#return:訓練/測試 的混淆矩陣和準確度敏感度特異度
#隨機森林判成1數量 / 混合模型的結果 / 迭代次數 / RF OOBError
#程式運行時間
BiMMforestH2 <- function(traindata = NULL, testdata = NULL,
                       formula = NULL, random = "+(1|MRN)",
                       seed = NULL,glmControl = "maxfun"){
  results <- NULL
  data = traindata
  initialRandomEffects = rep(0,length(data[,1]))
  ErrorTolerance = 0.006
  MaxIterations = 100
  
  Predictors <- paste(attr(terms(formula),"term.labels"),collapse="+")
  TargetName <- formula[[2]]
  Target <- data[,toString(TargetName)]
  #set up variables for loop
  ContinueCondition <- TRUE
  iterations <- 0
  #initial values
  AdjustedTarget <- as.numeric(Target)-initialRandomEffects
  oldlik <- -Inf
  
  # Make a new data frame to include all the new variables
  newdata <- data
  shouldpredict = TRUE
  starttime <- Sys.time()
  while(ContinueCondition){
    # Current values of variables
    newdata[,"AdjustedTarget"] <- AdjustedTarget
    iterations <- iterations + 1
    print(iterations)
    #build tree
    set.seed(seed)
    forest <- randomForest(formula(paste(c("factor(AdjustedTarget)",
                                           Predictors),collapse = "~")),
                           data = data, method = "class")
    forestprob <- predict(forest,type="prob")[,2]
    RFpredictprob <- as.data.frame(forestprob)
    RFpredict1 <- length(RFpredictprob[forestprob>=0.5,])
    results[["forestprob >= 0.5"]] <- RFpredict1
    results[["RF"]] <- forest
    if (glmControl == "maxfun"){
      lmefit <- tryCatch(bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                                      collapse="~"), random, sep=""))),
                                data=data,family=binomial,
                                control=glmerControl(optCtrl=list(maxfun=20000))),
                         error=function(cond)"skip")
    }else if(glmControl == "tolPwrss"){
    # Estimate New Random Effects and Errors using BLMER
    
    lmefit <- bglmer(formula(c(paste(paste(c(toString(TargetName),"forestprob"),
                                           collapse="~"),random,sep=""))),data=data,family=binomial,
                     control = glmerControl(tolPwrss=1e-3))
    }
    # Get the likelihood to check on convergence
    if(!(class(lmefit)[1]=="character")){
      newlik <- logLik(lmefit)#loglikelihood
      
      ContinueCondition <- (abs(newlik-oldlik) > ErrorTolerance & iterations < MaxIterations)
      oldlik <- newlik 
      
      # Extract random effects to make the new adjusted target
      logit <- forestprob #rf機率
      logit2 <- exp(predict(lmefit,re.form=NA))/(1+exp(predict(lmefit,re.form=NA))) 
      results[["logit"]] <- logit
      results[["logit2"]] <- logit2
      # logit ()
      #population level effects
      AllEffects <- (logit+logit2)/2 #average them =>paper上的qit
      
      #h2 update
      AdjustedTarget <- ifelse(as.numeric(Target) + AllEffects < 1.5,0,1)
      
    }else{ 
      ContinueCondition <- FALSE 
      
    }
    
    #if all of the binary outcomes are the same then get out of loop
    if(min(AdjustedTarget) == max(AdjustedTarget)){
      ContinueCondition <- FALSE
      shouldpredict = FALSE
      return(print("all of the binary outcomes are the same"))
    }
  }
  endtime <- Sys.time()
  times <- endtime-starttime
  results[["iter"]] <- iterations
  results[["run time"]] <- times
  
  if(class(lmefit)[1]=="character" | shouldpredict==FALSE){
    return(print("GLMM did not converge or all of the outcomes are the same"))
  }else if(!(class(lmefit)[1]=="character")){
    results[["model summary"]] <- summary(lmefit)
    
    test.preds <- predict(forest,testdata)#為甚麼測試只有丟森林
    traindata1 <- cbind(traindata,random)
    train.preds <- ifelse(predict(lmefit,traindata1,type="response")<.5,0,1)
    results[["train.preds"]] <- train.preds
    results[["test.preds"]] <- test.preds
    
    t1 <- table(traindata[,ncol(traindata)],train.preds)
    trainacc <- (t1[1]+t1[4]) / sum(t1)
    train0acc <- t1[1]/(t1[1]+t1[3])
    train1acc <- t1[4]/(t1[2]+t1[4])
    results[["CM of Train data"]] <- t1
    results[["Train acc sen spe"]] <- c(trainacc,train1acc,train0acc)
    
    t4 <- table(testdata[,ncol(testdata)],test.preds)
    testacc <- (t4[1]+t4[4]) / sum(t4)
    test0acc <- t4[1]/(t4[1]+t4[3])
    test1acc <- t4[4]/(t4[2]+t4[4])
    results[["CM of Test data"]] <- t4
    results[["Test acc sen spe"]] <- c(testacc,test1acc,test0acc)
    
    testdata1 <- cbind(testdata,random)
    test.lme.preds <- ifelse(predict(lmefit,testdata1,re.form=NULL,type="response")<.5,0,1)
    results[["test.lme.preds"]] <- test.lme.preds[1:nrow(testdata)]
    t2 <-table(testdata[,ncol(testdata)],test.lme.preds[1:nrow(testdata)])
    lme.testacc <- (t2[1]+t2[4]) / sum(t2)
    lme.test0acc <- t2[1]/(t2[1]+t2[3])
    lme.test1acc <- t2[4]/(t2[2]+t2[4])
    results[["lme.CM of Test data"]] <- t2
    results[["lme.Test acc sen spe"]] <- c(lme.testacc,lme.test1acc,lme.test0acc)
    #return train and test confusion matrices, 
    # iterations, and RF OOBER
    RfOober <- mean(forest$err.rate[,1])
    results[["RF OOBerror"]] <- RfOober
    return(results)
  }
  
}


#### Random Forest ####
RF <- function (traindata = NULL, testdata = NULL,
             formula = "factor(y)~x",classwt = NULL,
             seed = NULL,mtry = sqrt(ncol(traindata))){
  results <- NULL
  set.seed(seed)
  rf.train <- randomForest(formula,
               data = traindata, method = "class",classwt = classwt,mtry = mtry)
  results[["RF"]] <- rf.train
  CM.train <- table(real = rf.train$y,pred = rf.train$predicted)
  trainacc <- (CM.train[1]+CM.train[4]) / sum(CM.train)
  train0acc <- CM.train[1]/(CM.train[1]+CM.train[3])
  train1acc <- CM.train[4]/(CM.train[2]+CM.train[4])
  results[["CM of Train data"]] <- CM.train
  results[["Train acc sen spe"]]<- c(trainacc,train1acc,train0acc)
  results[["Var importance"]] <- importance(rf.train)
  rf.test <- predict(rf.train,testdata)
  results[["test pred"]] <- rf.test
  CM.te <- table(real = testdata[,ncol(testdata)], pred = rf.test)
  testacc <- (CM.te[1]+CM.te[4]) / sum(CM.te)
  test0acc <- CM.te[1]/(CM.te[1]+CM.te[3])
  test1acc <- CM.te[4]/(CM.te[2]+CM.te[4])
  results[["CM of Test data"]] <- CM.te
  results[["Test acc sen spe"]]<- c(testacc,test1acc,test0acc)
  
  return(results)
}
  


