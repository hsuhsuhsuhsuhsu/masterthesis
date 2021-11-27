#use data from joineR to demo BIMM & MEml
#install.packages("joineR")
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
ftable(hv$sex,hv$status,hv$prenyha,hv$lv,hv$hs)
?ftable
hv$sex<-as.factor(hv$sex)
hv$lvmica <- as.factor(hv$lvmica)
ftable(hv$sex,hv$lvmica)
#分訓練測試
smp_size <- round(nrow(hv) * 0.7, 0)
set.seed(123)
train_ind <- sample(seq_len(nrow(hv)), size = smp_size)
train <- hv[train_ind,]
test <- hv[-train_ind, ]