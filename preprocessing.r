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
Mrn_Vis$"Mrn_Vis" <-mapply(Mrn_Vis$MRN,Mrn_Vis$Visit_24Hrs_cloud,FUN=paste0)
AB_DE <- cbind(Mrn_Vis[,3],AB_DE)
write.csv(AB_DE,file="AB_DE_newkey.csv")#1671*264

AB_DE <- read_csv("AB_DE_newkey.csv")

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


#Visit_Treatment 用"Mrn_Vis"merge
VT <- read_csv("Visit_Treatment.csv")#6998*65
#清完的資料叫cVT
#1.移除不必要變數 unique刪除重複
drop_col <- colnames(VT) %in% c("Test/Battery", "Version")
VT <- VT[,!drop_col]
VT <- unique(VT)#6998*63
#2.duplicate 每個ID
VT <- arrange(VT,"MRN","Visit Date")
VT <- as.data.frame(VT)
VT$MRN <- as.factor(VT$MRN)
length(levels(VT[,1]))#2744
for(i in 1:length(levels(VT[,"MRN"]))){
  eachid<-VT[VT[,"MRN"]==levels(VT[,"MRN"])[i],]
  
}



for (i in 1:length(levels(temp[, "MRN"]))) {
  vnumber <- temp[temp[, "MRN"] == levels(temp[, "MRN"])[i], ]
  vnumber[, "Visit_24Hrs_cloud"] <- rep(1:dim(vnumber)[1])
  vnumber[, "Visit_24Hrs_cloud"] <-
    as.character(vnumber[, "Visit_24Hrs_cloud"])
  renumberd <- rbind(renumberd, vnumber)
}
#4.對visit重新編號
#new一個key=MRN+VIS 確定每個Visit只有一筆資料


#Visit_Vital signs 用"Mrn_Vis"merge
VVs <- read_csv("Visit_Vital signs.csv")#7452*61
#清完的資料叫cVVs

#OUTCOME 計算NA比例
OC <- read_csv("OUTCOME.scv")
#清完的資料叫uOC



fdergeagreger







