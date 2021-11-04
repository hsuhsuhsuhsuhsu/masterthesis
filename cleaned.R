#read所有清好的檔案
#999=NA 要取代 (還有其他不是數字的東西也要取代)
#用ABPM HBP分別併檔
#確認總筆數/人數 還有各訪視筆數/人數
#原始資輛=>清完的檔案名稱
#Cloud_ABPM=>abpm
#Cloud_HOMEBP=>hbp hbpnewkey
#Demographics=>uDE
#Family History=>cFH
#Visit_Treatment=>VT cVT
#Visit_Vital signs=>VVs
#OUTCOME=>OC nkeyOC

#hbpnewkey+uDE+cVT+VVs
ab_de <- merge(x = renumberd,y = uDe,by = "MRN",all.x = TRUE)





