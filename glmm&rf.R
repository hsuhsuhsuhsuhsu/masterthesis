#跑glmm rf比較用
library(randomForest)
library(blme)
formula <- lvmica~sex+age+time+grad+ef+bsa
random <- c("age","time")
forest <- randomForest(lvmica~sex+age+time+grad+ef+bsa,
                       data = train, method = "class")
forestprob<-predict(forest,type="prob")[,2]
RFpredprob <- as.data.frame(forestprob)
train.preds <-  ifelse(RFpredprob <0.5,0,1)
t1 <- table(train$lvmica,train.preds)
trainacc <- (t1[1]+t1[4]) / sum(t1)
train0acc <- t1[1]/(t1[1]+t1[3])
train1acc <- t1[4]/(t1[2]+t1[4])
t1

test.preds <- predict(forest,test,type="prob")[,2]
testprob <- ifelse(test.preds <0.5,0,1)
t4 <- table(test$lvmica,testprob)
testacc <- (t4[1]+t4[4]) / sum(t4)
test0acc <- t4[1]/(t4[1]+t4[3])
test1acc <- t4[4]/(t4[2]+t4[4])
t4

bml<-bglmer(formula(lvmica~sex+grad+ef+bsa +(1|time) +(1|age)),
       data=train,family=binomial,control=glmerControl(optCtrl=list(maxfun=20000)))

bmltrain<- ifelse(predict(bml,train,type="response")<.5,0,1)
b1 <- table(train$lvmica,bmltrain)
#bmltest <- ifelse(predict(bml,test,type="response")<.5,0,1)





