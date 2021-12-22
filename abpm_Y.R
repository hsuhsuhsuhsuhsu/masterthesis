library(dplyr)
#讓日期時間讀進來就是字串 read.csv (colClass = c("colname" ="class type"))
ab <- read.csv("TCHCData/abpm.csv", header = T)
abp <- ab
#計算TCHC 的 nocturnal dipping rate
#白天:06-22 晚上22-06
#白天sbp : 06 - 22 的平均sbp
#晚上 sbp : 22 - 06 的平均sbp
#nocturnal dipping rate：(白天sbp - 晚上sbp) / 白天sbp *100

#row 1041 數值都是NA
abp <- abp[-1041,]
ab[1041,]
#所有元素變時間格式
timeS <- which(colnames(abp) %in% "ABPM_time_1")
timeE <- which(colnames(abp) %in% "ABPM_time_36")
asTime <-lapply(abp[, timeS:timeE] ,
    FUN = function(x)strptime(x, "%H:%M"))
asTime <- as.data.frame(asTime)
ten <- strptime("22:00", "%H:%M")
six <- strptime("06:00", "%H:%M")

#***用時間差異算會是錯的
#要用每個時間判斷是白天還是晚上=>另一個data frame都是白天晚上
#然後再算白天晚上的col範圍 像DNColnTime
DN <- data.frame(matrix(NA,nrow=1670,ncol=36))
for(x in 1:dim(asTime)[2]){
  DN[which(asTime[,x] <ten & asTime[,x]>=six),x] <- "D" #白天
  DN[which(asTime[,x] >=ten | asTime[,x]<six),x] <- "N"
  DN[which(asTime[,x]==as.POSIXct("2022-1-1")),x] <- "na"#晚上
} 
sys<-abp[,10:45]
time<-abp[,118:153]

final =NULL
for(i in 1:dim(DN)[1]){
  Davg <- mean(as.numeric(sys[i,which(DN[i,]=="D")]),na.rm = T)
  Navg <- mean(as.numeric(sys[i,which(DN[i,]=="N")]),na.rm = T)
  avg <- cbind(Davg,Navg)
  avg <- as.data.frame(avg)
  final <- rbind(final,avg)
}

#dipping ratio = (davg-navg)/davg*100
final[,"dipping ratio"] <- (final[,"Davg"] - final[,"Navg"] )/final[,"Davg"] * 100
length(which(is.na(final[,"dipping ratio"])))
for(i in 1:dim(final)[1]){
  if(is.na(final[i,"dipping ratio"])){
    final[i,"dipping status"] <- NA
  }else if(final[i,"dipping ratio"]<0){
    final[i,"dipping status"] <- "Reverse dipper"
  }else if(final[i,"dipping ratio"]<10 & final[i,"dipping ratio"]>=0){
    final[i,"dipping status"] <- "Non dipper"
  }else if(final[i,"dipping ratio"]>= 10 & final[i,"dipping ratio"]<20 ){
    final[i,"dipping status"] <- "Dipper"
  }else{
    final[i,"dipping status"] <- "Extreme dipper"
  }
}
length(which(final[,"dipping status"]=="Extreme dipper"))#24
length(which(final[,"dipping status"]=="Dipper"))#327
length(which(final[,"dipping status"]=="Non dipper"))#759
length(which(final[,"dipping status"]=="Reverse dipper"))#385

ABP <- cbind(abp,final)
write.csv(ABP,file="TCHCData/abpm_Y.csv")


#把hbp和ABPM ID Visit ..."dipping status" merge
hbp <- read.csv("TCHCData/hbpnewkey.csv")
abpm <- read.csv("TCHCData/abpm_Y.csv")
abpm <- abpm[,-1]
newkey <-as.data.frame(paste0(abpm$MRN,abpm$Visit_24Hrs_cloud))
abnkey<-cbind(newkey,abpm)
colnames(abnkey)[1] <- "Mrn_Vis"
abdip<-as.data.frame(cbind(abnkey$Mrn_Vis,abnkey$dipping.status))
length(which(is.na(abdip[,2])))#175
colnames(abdip)[1] <- "Mrn_Vis"
mydata <- merge(x = hbp,y = abdip,by = "Mrn_Vis",all.y = T)
colnames(mydata)[124] <- "dipping.status"
write.csv(mydata,file="TCHCData/hbp_dip.csv")
mydata1 <- merge(x = hbp,y = abdip,by = "Mrn_Vis",all.x = T)
colnames(mydata1)[124] <- "dipping.status"
write.csv(mydata1,file="TCHCData/hbp_dip_byx.csv")
#看hbp_dip_byx 各個訪視的dipping status ＮＡ數量
table(hbpdip_byX$dipping.status)
sum(is.na(hbpdip_byX$dipping.status))
hbpdip_byX<-read.csv("TCHCData/hbp_dip_byx.csv")
aa<-read.csv("TCHCData/hbp_dip.csv")
#dipping status 4類 在各個訪視的數量
v1<-filter(hbpdip_byX,visit_HBP_Dmode=="1")
table(v1$dipping.status)
sum(is.na(v1$dipping.status))
v2<-filter(hbpdip_byX,visit_HBP_Dmode=="2")
table(v2$dipping.status)
sum(is.na(v2$dipping.status))
v3<-filter(hbpdip_byX,visit_HBP_Dmode=="3")
table(v3$dipping.status)
sum(is.na(v3$dipping.status))
v4<-filter(hbpdip_byX,visit_HBP_Dmode=="4")
table(v4$dipping.status)
sum(is.na(v4$dipping.status))

#看已經可以分析的complete case有多少個
sum(complete.cases(hbpdip_byX))#245
okcases<-hbpdip_byX[which(complete.cases(hbpdip_byX)),]
table(okcases$dipping.status)






#以下都是錯的
#####
#算時間差異 可砍
TimeDifften <-lapply(asTime ,FUN = function(x)
      abs(as.numeric(difftime(x, ten, units = "secs"))))
TimeDifften <- as.data.frame(TimeDifften)

TimeDiffsix <-lapply(asTime ,FUN = function(x)
      abs(as.numeric(difftime(x, six, units = "secs"))))
TimeDiffsix <- as.data.frame(TimeDiffsix)

#A<-lapply(asTime,FUN = function(x) x-six)
#A<-as.data.frame(A)

ClosestTenCol <- apply(TimeDifften , 1 ,FUN = function(x)
                           which(x == min(x,na.rm = T))[1])
#有2格一樣靠近22點的=>取前面的=>白天的尾巴
ClosestTenCol <-as.data.frame(ClosestTenCol)

ClosestSixCol <- apply(TimeDiffsix , 1 ,FUN = function(x)
  which(x == min(x,na.rm = T))[1])
#有2格一樣靠近6點的=>取前面的=>晚上的尾巴
ClosestSixCol <-as.data.frame(ClosestSixCol)

#day開始col =>ClosestSixCol+1  day結束col => ClosestTenCol
#night開始col =>ClosestTenCol+1  night結束col=> ClosestSixCol
DNColnTime = NULL
DNColnTime <- as.data.frame(DNColnTime)
DNColnTime[1:dim(ClosestSixCol)[1],1] <- NA
DNColnTime[,1] <- ClosestSixCol+1
DNColnTime[,2] <- ClosestTenCol
DNColnTime[,3] <- ClosestTenCol+1
DNColnTime[,4] <- ClosestSixCol
colnames(DNColnTime) <- c("day Start","day End","night Start","night End")
#如果是36 +1就要變成1
DNColnTime[DNColnTime[,]==37] <- 1
#看要不要處理 沒有晚上 或是不符合白天測量至少20次(夜間 < 17)及夜間測量至少7次
DNColnTime[,"night count"] <- abs(DNColnTime[,3]-DNColnTime[,4])+1
length(which(DNColnTime[,"night count"] < 7))
length(which(DNColnTime[,"night count"] >16))
DNColnTime1<-NULL
DNColnTime1 <- DNColnTime[-which(DNColnTime[,"night count"] < 7),] 
DNColnTime1 <- DNColnTime1[-which(DNColnTime1[,"night count"] >16),] 

#DNColnTime找晚上和白天區間 看哪一個可以是連續區段
colnames(DNColnTime)

sys<-abp[,10:45]
time<-abp[,118:153]

#如果白天頭<白天尾 => 白天是連續 晚上是其他 
final = NULL
for(r in 1: dim(DNColnTime)[1]){
  if(DNColnTime[r, "day Start"] < DNColnTime[r, "day End"]){
    ds <- DNColnTime[r, "day Start"]
    de <- DNColnTime[r, "day End"]
    dTime <- time[r, ds:de]
    dSbp <- sys[r, ds:de]
    avgdSbp <- mean(as.numeric(dSbp),na.rm = T)
    nTime <- time[r, -c(ds:de)]
    nSbp <- sys[r, -c(ds:de)]
    avgnSbp <- mean(as.numeric(nSbp),na.rm = T)
  }else if(DNColnTime[r, "day Start"] > DNColnTime[r, "day End"]){
    ns <- DNColnTime[r,"night Start"] 
    ne <- DNColnTime[r,"night End"]
    nTime <- time[r,ns:ne]
    nSbp <- sys[r,ns:ne]
    avhnSbp <- mean(as.numeric(nSbp),na.rm = T)
    dTime <- time[r,-c(ns:ne)]
    dSbp <- sys[r,-c(ns:ne)]
    avgdSbp <- mean(as.numeric(dSbp),na.rm = T)
  }else{
    print(paste(r,"day start = day end. Please check"))
    avgdSbp <- "na"
    avhnSbp <- "na"
  }
  avg <- cbind(avgdSbp,avhnSbp)
  avg <- as.data.frame(avg)
  final <- rbind(final,avg)
}
DNColnTime[1034,]

ABP <- cbind(abp,final)













