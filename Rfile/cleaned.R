#read所有清好的檔案
#999=NA 要取代 (還有其他不是數字的東西也要取代)
#用ABPM HBP分別併檔
#確認總筆數/人數 還有各訪視筆數/人數
#原始資料=>清完的檔案名稱
#Cloud_ABPM=>abpm
#Cloud_HOMEBP=>hbp hbpnewkey
#Demographics=>uDE
#Family History=>cFH
#Visit_Treatment=>VT cVT
#Visit_Vital signs=>VVs
#OUTCOME=>OC nkeyOC


getwd()
library(data.table)
library(dplyr)
library(tidyr)
library(tidyverse)
setwd("C://Users//hsu//Desktop//master//TCHCData")

#hbpnewkey+uDE+cVT+VVs
hbp <- read_csv("hbpnewkey.csv", col_select = -1)
ude <- read_csv("uDE.csv", col_select = -1)
cvt <- read_csv("cVT.csv", col_select = -c(1, 2))
vvs <- read_csv("VVs.csv", col_select = -1)


hbp_de <- merge(hbp, ude, by = "MRN", all.x = T)
hbp_de_vt <- merge(hbp_de, cvt, by = "Mrn_Vis", all.x = T)
hbp_de_vt_vs <- merge(hbp_de_vt, vvs, by = "Mrn_Vis", all.x = T)
#hbp_de_vt_vs = 3032*320
write.csv(hbp_de_vt_vs,file="hbp_devtvs.csv")
#暫定最終預測目標是 office血壓 Createnine TG
#找出這些預測目標有值的 人 筆數
