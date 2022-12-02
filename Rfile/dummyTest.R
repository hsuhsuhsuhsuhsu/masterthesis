library(fastDummies)

DD <- CCTr
EE <- CCTe

DC <- dummy_cols(DD,select_columns = c("Gender","DM","HOS"))
DC1 <- DC[,-which(colnames(DC) %in% c("MRN","Gender","DM","HOS"))]

EC <- dummy_cols(EE,select_columns = c("Gender","DM","HOS"))
EC1 <- EC[,-which(colnames(EC) %in% c("MRN","Gender","DM","HOS"))]

dm1tr <- CC1RTr
dm1te <- CC1RTe
DC <- dummy_cols(dm1tr,select_columns = c("Gender","DM","HOS"))
dm1tr <- DC[,-which(colnames(DC) %in% c("MRN","Gender","DM","HOS"))]

EC <- dummy_cols(dm1te,select_columns = c("Gender","DM","HOS"))
dm1te <- EC[,-which(colnames(EC) %in% c("MRN","Gender","DM","HOS"))]

dm12 <- VC1RTr12 
DC <- dummy_cols(dm12,select_columns = c("Gender","DM","HOS"))
dm12 <- DC[,-which(colnames(DC) %in% c("Gender","DM","HOS"))]

dm3 <- VC1RTe3
DC <- dummy_cols(dm3,select_columns = c("Gender","DM","HOS"))
dm3 <- DC[,-which(colnames(DC) %in% c("Gender","DM","HOS"))]


dm1 <- VC1RTr1  
DC <- dummy_cols(dm1,select_columns = c("Gender","DM","HOS"))
dm1 <- DC[,-which(colnames(DC) %in% c("Gender","DM","HOS"))]

dm2 <- VC1RTe2
DC <- dummy_cols(dm2,select_columns = c("Gender","DM","HOS"))
dm2 <- DC[,-which(colnames(DC) %in% c("Gender","DM","HOS"))]

#####set type####
DC1$Gender_1 <- as.factor(DC1$Gender_1)
DC1$Gender_2 <- as.factor(DC1$Gender_2)
DC1$DM_1 <- as.factor(DC1$DM_1)
DC1$DM_2 <- as.factor(DC1$DM_2)
DC1$DM_3 <- as.factor(DC1$DM_3)
DC1$HOS_1 <- as.factor(DC1$HOS_1)
DC1$HOS_2 <- as.factor(DC1$HOS_2)
DC1$HOS_3 <- as.factor(DC1$HOS_3)

EC1$Gender_1 <- as.factor(EC1$Gender_1)
EC1$Gender_2 <- as.factor(EC1$Gender_2)
EC1$DM_1 <- as.factor(EC1$DM_1)
EC1$DM_2 <- as.factor(EC1$DM_2)
EC1$DM_3 <- as.factor(EC1$DM_3)
EC1$HOS_1 <- as.factor(EC1$HOS_1)
EC1$HOS_2 <- as.factor(EC1$HOS_2)
EC1$HOS_3 <- as.factor(EC1$HOS_3)
#####
#DC1=>train EC1=>Test
library(randomForest)


#choose this
dm6 <- randomForest(factor(dip)~.,data = DC1, method = "class",
                    sampsize = c(200,110))
dm6$confusion
(198+510)/(200+638)#0.84
D7 <- predict(dm6,EC1)
table(re = EC1$dip,D7)
spe = 17/(17+25)#0.4
sen = 113/(113+55)#0.67
acc = (113+17)/(17+25+55+113)#0.619

write.csv(DC1,"TCHCData/CCTr_dum.csv")
write.csv(EC1,"TCHCData/CCTe_dum.csv")

#no dummy =>結果差不多
FF <- randomForest(factor(dip)~.,data = CCTr, method = "class",
                    sampsize = c(200,110))
FF$confusion
RE <- predict(FF,CCTe)
table(re = EC1$dip,RE)


#1p1r train= dm1tr test=dm1te
table(dm1tr$dip)
str(dm1tr)
pr1 <- randomForest(factor(dip)~.,data = dm1tr, method = "class",
                    sampsize = c(98,80))
pr1$confusion

se <- predict(pr1,dm1te)
table(re = dm1te$dip,se)
spe = 7/23#0.3
sen = 65/82#0.79
acc = 72/(23+82)#0.68
getwd()
#setwd("C:/Users/hsu/Desktop/master")
write.csv(dm1tr,"TCHCData/CC1p1rTr_dum.csv")
write.csv(dm1te,"TCHCData/CC1p1rTe_dum.csv")



##temp##
normalize <- function(x, na.rm = TRUE) {
  return((x- min(x)) /(max(x)-min(x)))
}
nor <- DC1
nn <- nor[,c(1,2,3,4,7,8,11,12)]
ww <- as.data.frame(apply(nn,2,normalize))
donr<- cbind(nor,ww)
donr <-donr[,-c(1,2,3,4,7,8,11,12)]
donr <- donr[,c(14:21,1:4,6:13,5)]

te <- EC1
gg <- te[,c(1,2,3,4,7,8,11,12)]
gg1 <- as.data.frame(apply(gg,2,normalize))
tenr <- cbind(te,gg1)
tenr <-tenr[,-c(1,2,3,4,7,8,11,12)]
tenr <- tenr[,c(14:21,1:4,6:13,5)]
#train = donr test =tenr
R1 <- randomForest(factor(dip)~.,data = donr, method = "class",
                    sampsize = c(200,110))
R1$confusion
P1 <- predict(R1,tenr)
table(re = tenr$dip,P1)

R2 <- randomForest(factor(dip)~.,data = donr, method = "class",
                   sampsize = c(200,50))

R2$confusion
P2 <- predict(R2,tenr)
table(re = tenr$dip,P2)

dm <- randomForest(factor(dip)~.,data = DC1, method = "class")
dm$confusion
Dpred <- predict(dm,EC1)
table(real = EC1$dip, pred = Dpred)
table(DC1$dip)
dm1 <- randomForest(factor(dip)~.,data = DC1, method = "class",
                    sampsize = c(200,200))
dm1$confusion
D1 <- predict(dm1,EC1)
table(real = EC1$dip, pred = D1)
EC1$dip <- as.factor(EC1$dip)
D2 <- predict(dm1,EC1)
table(real = EC1$dip,D2)

dm2 <- randomForest(factor(dip)~.,data = DC1, method = "class",
                    sampsize = c(200,250))#c(class0,class1)
dm2$confusion
D3 <-predict(dm2,EC1)
table(re = EC1$dip,D3)
table(D3)
table(EC1$dip)


dm3 <- randomForest(factor(dip)~.,data = DC1, method = "class",
                    sampsize = c(200,100))
dm3$confusion
D4 <-predict(dm3,EC1)
table(re = EC1$dip,D4)
spe = 19/(19+23)
sen = 107/(61+107) 
Acc = (19+107)/(19+23+61+107)#0.6



dm4 <- randomForest(factor(dip)~.,data = DC1, method = "class",
                    sampsize = c(200,150))
dm4$confusion
D5 <-predict(dm4,EC1)
table(re = EC1$dip,D5)


dm5 <- randomForest(factor(dip)~.,data = DC1, method = "class",
                    sampsize = c(200,125))
dm5$confusion
D6 <- predict(dm5,EC1)
table(re = EC1$dip,D6)
spe = 11/(11+31)#0.26
sen = 123/(45+123)#0.73
Acc = (11+123)/(11+31+45+123)#0.63
