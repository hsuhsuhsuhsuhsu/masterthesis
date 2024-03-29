#以血壓為主去merge
#原始資輛=>清完的檔案名稱
#Cloud_ABPM=>abpm
#Demographics=>uDE
#Family History=>FH
#Visit_Treatment=>cVT
#Visit_Vital signs=>cVVs
#OUTCOME=>uOC
getwd()
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
setwd("C://Users//hsu//Desktop//master//TCHCData")
#####
#清ABPM
raw <- fread("Cloud_ABPM.csv")
raw <- as.data.frame(raw)
#把不會用到的變數刪掉，叫Cleaning
drop_col <-
  colnames(raw) %in% c("Test/Battery", "Order Filled Out", "Version", "ABPM_cloud")
Cleaning <- raw[, !drop_col]
#把MRN變factor讓他可以有levels
Cleaning$MRN <- as.factor(Cleaning$MRN)
level <- levels(Cleaning$MRN)
#資料是{}的取代成NA
Cleaning[Cleaning == "{}"] <- NA
str(Cleaning)
#因為有"VisitDate"不同但後面資料完全相同的case，所以保留但不參考這個變數
temp <- Cleaning[, -which(colnames(Cleaning) == "Visit Date")]
repeatdata <- Cleaning[which(duplicated(temp) == TRUE), ]#dim=47,188
temp <- Cleaning[-which(duplicated(temp) == TRUE), ]#dim=1671,188
dim(unique(temp))#dim=1671,188 確定清掉重複row
length(levels(temp[, 1]))#people = 1333

#先按照ID Visit date sort過temp
sort(temp, by = c("MRN", "Visit Date"))
#重新給Visit編號 後續csv的Visit編號用visit date當key去對
#如果有ID的visit date都一樣=>先撈出來再看要不要刪
renumberd = NULL
for (i in 1:length(levels(temp[, "MRN"]))) {
  vnumber <- temp[temp[, "MRN"] == levels(temp[, "MRN"])[i], ]
  vnumber[, "Visit_24Hrs_cloud"] <- rep(1:dim(vnumber)[1])
  vnumber[, "Visit_24Hrs_cloud"] <-
    as.character(vnumber[, "Visit_24Hrs_cloud"])
  renumberd <- rbind(renumberd, vnumber)
}
which(renumberd[, "Visit_24Hrs_cloud"] == " ")
#integer(0) 檢查完Visit_24Hrs_cloud沒有空白

setwd("C://Users//hsu//Desktop//master//TCHCData")
write.csv(renumberd, file = "abpm.csv")

######
#Demographics基本資料
#清除重複 確認每個ID只有一筆 =>和ABPM MERGE
Demographics <- read_csv("Demographics.csv")
uDe <- Demographics[row.names(unique(Demographics[, "MRN"])), ]
uDe <- uDe[!duplicated(uDe[, "MRN"]), ]
dim(uDe)
write.csv(uDe,"uDE.csv")#2806*79


ab_de <- merge(x = renumberd,
               y = uDe,
               by = "MRN",
               all.x = TRUE)
dim(ab_de)
#清和資料無關的變數
drop_col <- colnames(ab_de) %in% c("Test/Battery", "Order Filled Out", "Version")
ab_de <- ab_de[, !drop_col]
write.csv(ab_de, file = "Ab_De.csv")


#把ab_de new 一個key=MRN+Visit=>"Mrn_Vis"
AB_DE <- read_csv("Ab_De.csv")
colnames(AB_DE)[1:5]
AB_DE <- AB_DE[, -1]
Mrn_Vis <- AB_DE[, c("MRN", "Visit_24Hrs_cloud")]
#apply(data,1=by row;2=by col,FUN)
str(Mrn_Vis)
#mapply=multi apply 
Mrn_Vis$"Mrn_Vis" <- mapply(Mrn_Vis$MRN,Mrn_Vis$Visit_24Hrs_cloud,FUN=paste0)
AB_DE <- cbind(Mrn_Vis[,3],AB_DE)
write.csv(AB_DE,file="AB_DE_newkey.csv")#1671*264

AB_DE <- read_csv("AB_DE_newkey.csv")

#######
#Family history 一個人可以填很多家人的病史
#清和資料無關的變數
FH <- as.data.frame(fread("Family history.csv"))
colnames(FH)[1:8]
drop_col <- colnames(FH) %in% c("Test/Battery", "Version")
FH <- FH[, !drop_col]
write.csv(FH,file="cFH.csv")
AB_DE_FH<-merge(x=AB_DE,y=FH,by="MRN",all.x=T)#2801*277
#直接merge會一人多筆 如果用單一時間點分析應該沒有影響
#ex丟所有V0的資料=>一人多筆合理
#清完的資料叫FH

########
#Visit_Treatment 用"Mrn_Vis"merge
VT <- read_csv("Visit_Treatment.csv")#6998*65

#1.移除不必要變數 unique刪除重複
drop_col <- colnames(VT) %in% c("Test/Battery", "Version")
VT <- VT[,!drop_col]
VT <- unique(VT)#6998*63
#2.duplicate 每個ID visit treatment相同的保留最後一筆
VT <- arrange(VT,"MRN","Visit Date","Order Filled Out")
VT <- as.data.frame(VT)
VT$MRN <- as.factor(VT$MRN)
length(levels(VT[,1]))#2744
VTrenum=NULL
for(i in 1:length(levels(VT[,"MRN"]))){
  eachid<-VT[VT[,"MRN"]==levels(VT[,"MRN"])[i],]
  keepvis<-which(duplicated(eachid[,"visit_treatment"],fromLast = T)==FALSE)#保留false的
  eachid<-eachid[keepvis,]
  #3.對visit重新編號
  eachid[, "visit_treatment"] <- rep(1:dim(eachid)[1])
  eachid[, "visit_treatment"] <-as.character(eachid[, "visit_treatment"])
  VTrenum <- rbind(VTrenum, eachid)#VTrenum 6913*63
}
#去掉order filled out
VTrenum<-VTrenum[,-which(colnames(VTrenum)=="Order Filled Out")]
#new一個key=MRN+VIS 確定每個Visit只有一筆資料
VTMrn_Vis <- VTrenum[, c("MRN", "visit_treatment")]#6913*2
#mapply=multi apply 
VTMrn_Vis$"VTMrn_Vis" <-mapply(VTMrn_Vis$MRN,VTMrn_Vis$visit_treatment,FUN=paste0)
#清完的資料叫cVT
cVT<-cbind(VTMrn_Vis[,3],VTrenum)#6913*63
names(cVT)[1]<-c("VTMrn_Vis")
cVT<-as.data.frame(cVT)
write.csv(cVT,file="VT.csv")
VT<-read_csv("VT.csv")
names(VT)[2]<-"Mrn_Vis"
write.csv(VT,file="cVT.csv")
#########
#Visit_Vital signs 用"Mrn_Vis"merge
VVs <- read_csv("Visit_Vital signs.csv")#7452*61
#清完的資料叫cVVs
"visit_vital_signs"
VVs<-as.data.frame(VVs)
colna<-lapply(lapply(lapply(VVs,is.na),sum),FUN = function(x) x/7452*100)#每一col NA的數量
drop_col <- colnames(VVs) %in% c("Test/Battery", "Version")
VVs <- VVs[,!drop_col]
VVs <- unique(VVs)#7451*59
#2.duplicate 每個ID visit treatment相同的保留最後一筆
VVs <- arrange(VVs,"MRN","Visit Date","Order Filled Out")
VVs$MRN <- as.factor(VVs$MRN)
length(levels(VVs[,1]))#2706
VVsrenum=NULL
for(i in 1:length(levels(VVs[,"MRN"]))){
  eachid<-VVs[VVs[,"MRN"]==levels(VVs[,"MRN"])[i],]
  keepvis<-which(duplicated(eachid[,"visit_vital_signs"],fromLast = T)==FALSE)#保留false的
  eachid<-eachid[keepvis,]
  #3.對visit重新編號
  eachid[, "visit_vital_signs"] <- rep(1:dim(eachid)[1])
  eachid[, "visit_vital_signs"] <-as.character(eachid[, "visit_vital_signs"])
  VVsrenum <- rbind(VVsrenum, eachid)#VVsrenum 6645*59
}
#去掉order filled out
VVsrenum<-VVsrenum[,-which(colnames(VVsrenum)=="Order Filled Out")]

#new一個key=MRN+VIS 確定每個Visit只有一筆資料
VVsMrn_Vis <- VVsrenum[, c("MRN", "visit_vital_signs")]#6913*2
#mapply=multi apply 
VVsMrn_Vis$"Mrn_Vis" <-mapply(VVsMrn_Vis$MRN,VVsMrn_Vis$visit_vital_signs,FUN=paste0)
#清完的資料叫cVT
cVVs<-cbind(VVsMrn_Vis[,3],VVsrenum)#6645*59
names(cVVs)[1]<-c("Mrn_Vis")
cVVs<-as.data.frame(cVVs)
write.csv(cVVs,file="VVs.csv")
AB_DE <- read_csv("AB_DE_newkey.csv")
AB_DE<-AB_DE[,-1]
AB_DE_VVS<-merge(AB_DE,cVVs,by="Mrn_Vis",all.x = T)#1671*322
write.csv(AB_DE_VVS,file="aa.csv")
AB_DE_VVS<-read_csv("aa.csv",col_select = -1)
which(colnames(AB_DE_VVS)=="office_peri_L_sys")
#算268:322的NA比例
couna<-AB_DE_VVS[,268:322]
colna<-lapply(lapply(couna,is.na),sum)#每一col NA的數量
narario<-sort(unlist(colna))
print(colna)
abpmna<-as.data.frame(unlist(colna))




##########
#OUTCOME 計算NA比例
OC <- read_csv("OUTCOME.csv")
#刪掉 OT_visit_vital_signs
dim(unique(OC))
colna<-lapply(lapply(lapply(OC,is.na),sum),FUN = function(x) x/972*100)#每一col NA的數量
#清完的資料叫uOC
library(VIM)#畫missing比例圖
OC.aggrplot <- aggr(OC, col=c('lightblue','red'), numbers=TRUE, prop = TRUE, sortVars=TRUE, labels=names(OC), cex.axis=.7, gap=3)
sort(unlist(colna))
drop_col <-
  colnames(OC) %in% c("Test/Battery", "Order Filled Out", "Version", "ABPM_cloud")
Cleaning <- OC[, !drop_col]
#把MRN變factor讓他可以有levels
Cleaning$MRN <- as.factor(Cleaning$MRN)
level <- levels(Cleaning$MRN)
#資料是{}的取代成NA
Cleaning[Cleaning == "{}"] <- NA
str(Cleaning)
#因為有"VisitDate"不同但後面資料完全相同的case，所以保留但不參考這個變數
temp <- Cleaning[, -which(colnames(Cleaning) == "Visit Date")]
repeatdata <- Cleaning[which(duplicated(temp) == TRUE), ]#dim=1 32
temp <- Cleaning[-which(duplicated(temp) == TRUE), ]#dim=971 312
dim(unique(temp))#dim=971 31 確定清掉重複row
length(levels(temp[, "MRN"]))#people = 1333
temp<-as.data.frame(temp)
#先按照ID Visit date sort過temp
sort(temp, by = c("MRN", "Visit Date"))
#重新給Visit編號 後續csv的Visit編號用visit date當key去對
#如果有ID的visit date都一樣=>先撈出來再看要不要刪
renumberd = NULL
for (i in 1:length(levels(temp[, "MRN"]))) {
  vnumber <- temp[temp[, "MRN"] == levels(temp[, "MRN"])[i], ]
  vnumber[, "OT_visit_vital_signs"] <- rep(1:dim(vnumber)[1])
  vnumber[, "OT_visit_vital_signs"] <-
    as.character(vnumber[, "OT_visit_vital_signs"])
  renumberd <- rbind(renumberd, vnumber)
}
write.csv(renumberd, file = "cOC.csv")
#合併資料集們=>把key名字改一樣再merge
#merge cOC with AB_DE 
#new一個key=MRN+VIS 確定每個Visit只有一筆資料
OCMrn_Vis <- renumberd[, c("MRN", "OT_visit_vital_signs")]#6913*2
#mapply=multi apply 
OCMrn_Vis$"Mrn_Vis" <-mapply(OCMrn_Vis$MRN,OCMrn_Vis$OT_visit_vital_signs,FUN=paste0)
COC<-cbind(OCMrn_Vis[,3],renumberd)
names(COC)[1]<-c("Mrn_Vis")
COC<-as.data.frame(COC)
write.csv(COC,file="nkeyOC.csv")
AB_DE <- read_csv("AB_DE_newkey.csv")
AB_DE <- AB_DE[,-1]
AB_DE_OC <- merge(AB_DE,COC,by="Mrn_Vis",all.x = T)#1671*296
write.csv(AB_DE_OC,file="bb.csv")
which(colnames(AB_DE_OC)=="OT_GOT")
#算268:322的NA比例
couna<-AB_DE_OC[,268:296]
colna<-lapply(lapply(couna,is.na),sum)#每一col NA的數量
D<-as.data.frame(unlist(colna))







###########
#HBP
HBP<-read_csv("Cloud_HOME BP_Dmode.csv")
HBP<-as.data.frame(HBP)#3088*124

#把不會用到的變數刪掉，叫CleanHBP 
drop_col <-
  colnames(HBP) %in% c("Test/Battery", "Order Filled Out", "Version")
CleanHBP <- HBP[, !drop_col]#3088*121
CleanHBP <- as.data.frame(CleanHBP)
CleanHBP <- arrange(CleanHBP,"MRN","Visit Date")

#把MRN變factor讓他可以有levels
CleanHBP$MRN <- as.factor(CleanHBP$MRN)
level <- levels(CleanHBP$MRN)#1507人

#因為有"VisitDate"不同但後面資料完全相同的case，所以保留但不參考這個變數
temp <- CleanHBP[, -which(colnames(CleanHBP) == "Visit Date")]
repeatdata <- CleanHBP[which(duplicated(temp) == TRUE), ]#dim=56*121
temp <- CleanHBP[-which(duplicated(temp) == TRUE), ]#dim=3032*121
dim(unique(temp))
length(levels(temp[, 1]))#1507
#a<-temp[temp$MRN=="NCKU0251",]

#重新給Visit編號 後續csv的Visit編號用visit date當key去對
#如果有ID的visit date都一樣=>先撈出來再看要不要刪
hrenumberd = NULL
for (i in 1:length(levels(temp[, "MRN"]))) {
  hnumber <- temp[temp[, "MRN"] == levels(temp[, "MRN"])[i], ]
  hnumber[, "visit_HBP_Dmode"] <- rep(1:dim(hnumber)[1])
  hnumber[, "visit_HBP_Dmode"] <-
    as.character(hnumber[, "visit_HBP_Dmode"])
  hrenumberd <- rbind(hrenumberd, hnumber)
}
which(hrenumberd[, "visit_HBP_Dmode"] == " ")
#integer(0) 檢查完Visit_24Hrs_cloud沒有空白
hrenumberd[hrenumberd == "{}"] <- NA
setwd("C://Users//hsu//Desktop//master//TCHCData")
write.csv(hrenumberd, file = "hbp.csv")#3032*121 1507人

Hbp <- read_csv("hbp.csv")
colnames(Hbp)[1:5]
Hbp <- Hbp[, -1]
Mrn_Vis <- Hbp[, c("MRN", "visit_HBP_Dmode")]
#apply(data,1=by row;2=by col,FUN)
str(Mrn_Vis)
#mapply=multi apply 
Mrn_Vis$"Mrn_Vis" <- mapply(Mrn_Vis$MRN,Mrn_Vis$visit_HBP_Dmode,FUN=paste0)
Hbp <- cbind(Mrn_Vis[,3],Hbp)
write.csv(Hbp,file="hbpnewkey.csv")#


############
#hbpnewkey+uDE+cVT+VVs
hbp <- read_csv("hbpnewkey.csv", col_select = -1)
ude <- read_csv("uDE.csv", col_select = -1)
cvt <- read_csv("cVT.csv", col_select = -c(1, 2))
vvs <- read_csv("VVs.csv", col_select = -1)


hbp_de <- merge(hbp, ude, by = "MRN", all.x = T)
hbp_de_vt <- merge(hbp_de, cvt, by = "Mrn_Vis", all.x = T)
hbp_de_vt_vs <- merge(hbp_de_vt, vvs, by = "Mrn_Vis", all.x = T)
#hbp_de_vt_vs = 3032*320
which(colnames(hbp_de_vt_vs)=="office_peri_L_sys")
#算266:320的NA比例
counh<-hbp_de_vt_vs[,266:320]
colnh<-lapply(lapply(counh,is.na),sum)#每一col NA的數量
narario<-sort(unlist(colnh))
print(colnh)
hbpna<-as.data.frame(unlist(colnh))


dim(complete.cases(AB_DE_VVS))








