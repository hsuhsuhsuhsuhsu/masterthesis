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
DF <- read.csv("TCHCData/COV_NA_less30.csv")
DF <- DF[,-1]
#去掉不用的變數 全放進隨機森林 給他挑重要性
del <- which(colnames(DF)%in% c("MRN","Mrn_Vis","dipping.status","visit","hos"))
DF <- DF[,-del]#1048*67
aa <- DF[,c(1:9,67)]
comp <- DF[which(complete.cases(DF)),]
rr <- randomForest(factor(dip)~.,data = comp, method = "class")
rr$confusion
rr$call
rr$importance
