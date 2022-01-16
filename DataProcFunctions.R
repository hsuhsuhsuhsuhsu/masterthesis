library(dplyr)
#作用:
#參數: file 
myRead <- function(file = NULL, removeNa = T, category = NULL,
                   newVar = NULL){
  results <- NULL
  dip <- read.csv(file)
  
  if(removeNa){
    na <- which(is.na(dip$dipping.status))
    dip <- dip[-na,]
  }
  if (length(category) == 2){
    dip[, newVar] <- ifelse(dip$dipping.status==category[1]|dip$dipping.status==category[2],1,0)
  }else if(length(category) == 1){
    dip[, newVar] <- ifelse(dip$dipping.status==category[1],1,0)
  }else{
    print("ERROR")
    return("category error")
  }
  FourAvg <- dip[,c(2,4,6,8,9,11,12,125,126)]
  FourAvg[FourAvg[,]=="---"]<-NA
  lapply(1:(dim(FourAvg)[2]-1), function(x) gsub("0",NA,FourAvg[[x]]))
  FourAvg <- FourAvg[complete.cases(FourAvg),]
  results[["complete_cases.n"]] <- sum(complete.cases(FourAvg))#817筆
  results[["people.n"]] <-length(levels(factor(FourAvg$MRN)))#553人
  results[["dip.status.table"]] <-table(FourAvg$dipping.status)
  results[["myData"]] <- FourAvg
  return(results)
}

#作用:
#參數: file
timeSplit <- function(data = NULL, removeCol.AM = NULL,
                      removeCol.PM = NULL){
  AM <- select(data,-removeCol.AM)
  AM[,"time"] <- 1
  PM <- select(data,-removeCol.PM)
  PM[,"time"] <- 2
  colnames(AM) <- 1:(dim(AM)[2])
  colnames(PM) <- 1:(dim(PM)[2])
  AMPM <- rbind(AM, PM)
  colnames(AMPM) <- c("Mrn_Vis", "MRN", "visit", "sys", 
                      "dia", "dip.status", "dip", "time")
  #sort
  AMPM <- arrange(AMPM, Mrn_Vis, visit)
  return(AMPM[, c(1:6, 8, 7)])
}
#作用:
#參數
#VisitOrCase : 填Visit 或是 Case 
#Train / Test : 填Visit數字 ex:  1 / 2 or 1/ 2:3
TrainTest <- function(data = NULL, VisitOrCase = NULL, nfixed = T, Train = NULL,
                      Test = NULL, seed = NULL, removeCategory = NULL){
  if (VisitOrCase =="Visit"){
    if (nfixed){
      #選出有test visit的MRN(人)
      test.true.MRN <- filter(data, data$visit %in% Test) %>%
        select(MRN)
      0
      Training <- filter(data, data$visit %in% Train)
    }else{
     Training <- data[which(data$visit %in% Train), ]
     Testing <- data[which(data$visit %in% Test), ]
    }
  }else if(VisitOrCase =="Case"){
    set.seed(seed)
  }else{
    print("ERROR")
    return("VisitOrCase error")
  }
}

