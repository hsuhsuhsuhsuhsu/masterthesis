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
a <- unique(fh[,c("MRN","Kinship")])
require(reshape2)
m <- fh[,c(1,2)]
m["kin"] <- ifelse(m[,"Kinship"]==1,"Dad","Mom")
m <- m[,-2]
#b <- melt(BOD, variable.name = "Stat", value.name ="Data") 
DF_result <- melt(m,)
head(DF_result)

library(tidyverse)
dim(fh)
fh2 <- cbind(fh, temp = 1)
a <- fh[,c("MRN","Kinship")]
dim(a)
a <- cbind(id = 1:3030, a)
View(a)
View(fh)
a2 <- reshape(fh, idvar = c("MRN", "Kinship"), direction = "wide")
d <- fh2 %>% spread(Kinship, temp, fill = 0)
spread(fh2, key = Kinship, value = temp, fill = 0,
       drop = TRUE)

reshape(m,
        direction="long",
        idvar=1:2, varying=c("Dad","Mom"), # the constant and varying columns
        times=c("Dad","Mom"),     # sets the values for new 'source' column
        v.names="Name_of_former_variable" ) # the header for the 'source' column
#### Visit Treatment####
vt <- read.csv("TCHCData/cVT.csv")
vt <- vt[,-c(1,2)]
Nvt <- vt[,c(1,2,4,50:63)]

#### Visit VitalSign####
vs <- read.csv("TCHCData/VVs.csv")
vs <- vs[,-c(1,4)]
vs <- vs[,-58]
##### 整理好的 可以merge的
try.de <- de
try.vt <- Nvt
try.vs <- vs
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
nrow(mydata)
#####
library(visdat)
library(ggplot2)
p <- vis_miss(vs, show_perc = F) + coord_flip()
gridExtra::marrangeGrob(list(p), top = "",
                        nrow = 1, ncol = 1)

