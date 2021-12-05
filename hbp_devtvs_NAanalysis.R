library(tidyverse)
library(dplyr)
library(VIM)
getwd()
setwd("C:/Users/hsu/Desktop/master/TCHCData")
hbp<-read_csv("hbp_devtvs.csv")

#欄位"MRN.x" - "HBP_d_time_D7_PM2"是HBP
#欄位"Visit Date.y" - "ECOG" 是Demographics
#欄位"MRN.y" - "anti_HP" 是Visit treatment
#欄位"MRN"開始是 Vital sign
#1.用hbp_devtvs.csv裡的Vital sign欄位NA的比例進行挑選
vsStart<-which(colnames(hbp) %in% "MRN")
vsEnd<-which(colnames(hbp) %in% "HR")
VS<- hbp[,vsStart:vsEnd]

vsna = NULL
for(i in 4:58){
  naratio <- paste(round(sum(is.na(VS[,i]))/dim(VS)[1]*100,2),"%")
  name <- colnames(VS[,i])
  con <- cbind(name,naratio)
  vsna <- rbind(vsna , con )
}
vsna <- as.data.frame(vsna)
vsna[,3] <- round(as.numeric(substr(vsna[,2],1,nchar(vsna[,2])-1))*dim(VS)[1]/100,0)
colnames(vsna)[3] <- "nacount"
vsna[,4] <- dim(VS)[1]-vsna[,3]
colnames(vsna)[4] <- "valuecount"
vsna[,5] <- vsna[,3] + vsna[,4]
colnames(vsna)[5] <- "Total"
vsna<-vsna[order(vsna$naratio),]

#HR	Total_C	gluatinine	glu	glu	glu_AC
#這6個不是血壓的算V1 V2 V3的NA狀況
hr <- cbind(VS[,3],VS[,"HR"])
TC <- cbind(VS[,3],VS[,"Total_C"])
Cre <- cbind(VS[,3],VS[,"Createnine"])
tg <- cbind(VS[,3],VS["TG"])
ldl <- cbind(VS[,3],VS["LDL"])
glu <- cbind(VS[,3],VS["glu_AC"])


hr1 <-hr[!is.na(hr[,1]),]
#####
V1hrna <- round(sum(is.na(hr1[hr1[,1]=="1","HR"]))/dim(hr1[hr1[,1]==1,])[1]*100,2)
V2hrna <- round(sum(is.na(hr1[hr1[,1]=="2","HR"]))/dim(hr1[hr1[,1]==2,])[1]*100,2)
V3hrna <- round(sum(is.na(hr1[hr1[,1]=="3","HR"]))/dim(hr1[hr1[,1]==3,])[1]*100,2)
V1hrnacou <- sum(is.na(hr1[hr1[,1]=="1","HR"]))
V2hrnacou <- sum(is.na(hr1[hr1[,1]=="2","HR"]))
V3hrnacou <- sum(is.na(hr1[hr1[,1]=="3","HR"]))
V1hrcou <- dim(hr1[hr1[,1]==1,])[1] - V1hrnacou
V2hrcou <- dim(hr1[hr1[,1]==2,])[1] - V2hrnacou
V3hrcou <- dim(hr1[hr1[,1]==3,])[1] - V3hrnacou
total <- c(dim(hr1[hr1[,1]==1,])[1],dim(hr1[hr1[,1]==2,])[1],dim(hr1[hr1[,1]==3,])[1])
c(V1hrna,V2hrna,V3hrna)
c(V1hrnacou,V2hrnacou,V3hrnacou)
c(V1hrcou,V2hrcou,V3hrcou)
#####
TC1 <-TC[!is.na(TC[,1]),]
V1TCna <- round(sum(is.na(TC1[TC1[,1]=="1","Total_C"]))/dim(TC1[TC1[,1]==1,])[1]*100,2)
V2TCna <- round(sum(is.na(TC1[TC1[,1]=="2","Total_C"]))/dim(TC1[TC1[,1]==2,])[1]*100,2)
V3TCna <- round(sum(is.na(TC1[TC1[,1]=="3","Total_C"]))/dim(TC1[TC1[,1]==3,])[1]*100,2)
V1TCnacou <- sum(is.na(TC1[TC1[,1]=="1","Total_C"]))
V2TCnacou <- sum(is.na(TC1[TC1[,1]=="2","Total_C"]))
V3TCnacou <- sum(is.na(TC1[TC1[,1]=="3","Total_C"]))
V1TCcou <- dim(TC1[TC1[,1]==1,])[1] - V1TCnacou
V2TCcou <- dim(TC1[TC1[,1]==2,])[1] - V2TCnacou
V3TCcou <- dim(TC1[TC1[,1]==3,])[1] - V3TCnacou
total <- c(dim(TC1[TC1[,1]==1,])[1],dim(TC1[TC1[,1]==2,])[1],dim(TC1[TC1[,1]==3,])[1])
c(V1TCna,V2TCna,V3TCna)
c(V1TCnacou,V2TCnacou,V3TCnacou)
c(V1TCcou,V2TCcou,V3TCcou)
#####
glu1 <- glu[!is.na(glu[,1]),]
V1gluna <- round(sum(is.na(glu1[glu1[,1]=="1","glu_AC"]))/dim(glu1[glu1[,1]==1,])[1]*100,2)
V2gluna <- round(sum(is.na(glu1[glu1[,1]=="2","glu_AC"]))/dim(glu1[glu1[,1]==2,])[1]*100,2)
V3gluna <- round(sum(is.na(glu1[glu1[,1]=="3","glu_AC"]))/dim(glu1[glu1[,1]==3,])[1]*100,2)
V1glunacou <- sum(is.na(glu1[glu1[,1]=="1","glu_AC"]))
V2glunacou <- sum(is.na(glu1[glu1[,1]=="2","glu_AC"]))
V3glunacou <- sum(is.na(glu1[glu1[,1]=="3","glu_AC"]))
V1glucou <- dim(glu1[glu1[,1]==1,])[1] - V1glunacou
V2glucou <- dim(glu1[glu1[,1]==2,])[1] - V2glunacou
V3glucou <- dim(glu1[glu1[,1]==3,])[1] - V3glunacou
c(dim(glu1[glu1[,1]==1,])[1],dim(glu1[glu1[,1]==2,])[1],dim(glu1[glu1[,1]==3,])[1])
c(V1gluna,V2gluna,V3gluna)
c(V1glunacou,V2glunacou,V3glunacou)
c(V1glucou,V2glucou,V3glucou)

#####

#2.挑選適合重複測量的插補資料方法 先看HBP欄位NA比例
which(colnames(hbp)%in% "MRN.x")
colnames(hbp[,67])#"HBP_d_diastolic_D7_PM2"
nhbp <- hbp[,3:67]
#先分Visit 算complete case
dim(filter(nhbp,visit_HBP_Dmode=="1"))[1]
sum(complete.cases(filter(nhbp,visit_HBP_Dmode=="1")))
dim(filter(nhbp,visit_HBP_Dmode=="2"))[1]
sum(complete.cases(filter(nhbp,visit_HBP_Dmode=="2")))
dim(filter(nhbp,visit_HBP_Dmode=="3"))[1]
sum(complete.cases(filter(nhbp,visit_HBP_Dmode=="3")))
dim(filter(nhbp,visit_HBP_Dmode=="4"))[1]
sum(complete.cases(filter(nhbp,visit_HBP_Dmode=="4")))

#7天*4 =28 收縮 舒張共56筆 +平均6筆
#56筆中超過3天沒有數值的(NA >12*2)
V11 <-filter(nhbp,visit_HBP_Dmode=="1")
V1<-filter(nhbp,visit_HBP_Dmode=="1")[,10:65]
sum(apply(V1,1,function(x) sum(is.na(x))>24))
V1na <-V11[which(apply(V1,1,function(x) sum(is.na(x))>24)),]

V2<-filter(nhbp,visit_HBP_Dmode=="2")[,10:65]
sum(apply(V2,1,function(x) sum(is.na(x))>24))

V3<-filter(nhbp,visit_HBP_Dmode=="3")[,10:65]
sum(apply(V3,1,function(x) sum(is.na(x))>24))


