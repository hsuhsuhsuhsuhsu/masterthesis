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

#找每一row最靠近0600 2200的時間點
timeS <- which(colnames(abp) %in% "ABPM_time_1")
timeE <- which(colnames(abp) %in% "ABPM_time_36")
asTime <-lapply(abp[, timeS:timeE] ,
    FUN = function(x)strptime(x, "%H:%M"))
asTime <- as.data.frame(asTime)
ten <- strptime("22:00", "%H:%M")
six <- strptime("06:00", "%H:%M")
TimeDifften <-lapply(asTime ,FUN = function(x)
      abs(as.numeric(difftime(x, ten, units = "secs"))))
TimeDifften <- as.data.frame(TimeDifften)

TimeDiffsix <-lapply(asTime ,FUN = function(x)
      abs(as.numeric(difftime(x, six, units = "secs"))))
TimeDiffsix <- as.data.frame(TimeDiffsix)

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
#要處理 沒有晚上 或是不符合白天測量至少20次(夜間 < 17)及夜間測量至少7次
DNColnTime[,"night count"] <- abs(DNColnTime[,3]-DNColnTime[,4])
length(which(DNColnTime[,"night count"] < 7))
DNColnTime1 <- DNColnTime[-which(DNColnTime[,"night count"] < 7),] 
DNColnTime1 <- DNColnTime1[-which(DNColnTime1[,"night count"] >16),] 

#DNColnTime找晚上和白天區間 看哪一個可以是連續區段


#用欄位差距 用時間去找數值



#在windows上 隨便加一行 看會不會在MAC出現

#在MAC上 隨便加一行 看會不會出現在windows
