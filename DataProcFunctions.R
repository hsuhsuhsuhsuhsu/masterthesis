library(dplyr)
library(faraway)
library(randomForest)
library(tibble)
library(mice)
library(missForest)
library(visdat)
#作用: 切Y的資料；設定1/0的類別和新變數(Y)
#參數: file=> 檔案名 category=>類別=1的名稱 ex:c(non dipper,reverse dipper)
#return : 樣本數 / 人數 / Y的分布 / 最後切完的data frame
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
  FourAvg[FourAvg[,]=="---"] <- NA
  FourAvg.proc<- FourAvg[,1:(dim(FourAvg)[2]-1)]
  FourAvg.proc[FourAvg.proc[,]=="0"] <- NA
  FourAvg.proc1 <- as.data.frame(cbind(FourAvg.proc,FourAvg[,newVar]))
  FourAvg <- FourAvg.proc1[complete.cases(FourAvg.proc1),]
  colnames(FourAvg)[dim(FourAvg)[2]] <- newVar
  results[["complete_cases.n"]] <- sum(complete.cases(FourAvg))
  results[["people.n"]] <-length(levels(factor(FourAvg$MRN)))
  results[["dip.status.table"]] <-table(FourAvg$dipping.status)
  results[["myData"]] <- FourAvg
  return(results)
}


#### 隨機森林插補FUN沒寫 直接寫在CALL FUN####
#作用: 對特定變數做隨機森林插補
#參數: data => 資料 
#Var => 要插補的變數 / all => 全部都impute
#Yname => 目標變數名稱
#return: 插補完的資料集
#miss Forest 變數要都是數值 或是binary
RF.impute <- function(data = NULL, chrVar = c("MRN","Mrn_Vis","dipping.status"),
                      factorVar = "CCB" , maxiter = 10, ntree = 100, verbose = F){
  #強制轉換成numeric 除了特定col以外
  results = NULL
  
  }
  


#作用: 加入其他變數 餅且合併成一個資料集
#參數: data => 原始資料
#Covlist =>
#return:合併完的資料集
#性別跟年齡na的預設刪掉
PlusCov <- function(data = NULL, Covlist = NULL,
                    IDname = "MRN", Cov = NULL,
                    Yname = "dip"){
  results = NULL
  OrginData <- data
  f <- read.csv(Covlist)
  ID.col <- which(colnames(f) %in% IDname)
  Cov.col <- which(colnames(f) %in% Cov)
  Cov.df <- f[ ,c(ID.col,Cov.col)]
  results[["Cov.df"]] <- Cov.df
  merge.df <- merge(OrginData, Cov.df, by = IDname, all.x=T)
  results[["merge.df"]] <- merge.df
  #合併完再刪掉年齡性別NA的row
  if (any(Cov %in% "Gender")){
    merge.df <- merge.df[-which(is.na(merge.df[,"Gender"])),]
    results[["dim without sex na"]] <- dim(merge.df)
  }
  if(any(Cov %in% "Age")){
    if(length(which(is.na(merge.df[,"Age"])))!=0){
      merge.df <- merge.df[-which(is.na(merge.df[,"Age"])),]
      results[["dim without age na"]] <- dim(merge.df)
    }
  }
  #把Yname那行放在最後面
  y.col <- which(colnames(merge.df) %in% Yname)
  n <- ncol(merge.df)
  complete.df <- merge.df[,c(1:(y.col-1),(y.col+1):n,y.col)]
  #檢查資料筆數 ID unique
    if(nrow( unique(complete.df)) == nrow(complete.df)){
      results[["AddCov.df"]] <- complete.df
    }else{
      return("nrow is not unique")
    }
  return(results)
}


#作用: 時間分割 1筆切成早上晚上2筆
#參數: removeCol.AM=> 要丟掉的colname 
#return : 分割完且排序的dataframe
timeSplit <- function(data = NULL, removeCol.AM = NULL,
                      removeCol.PM = NULL){
  cnames <- colnames(data)
  AM <- select(data,-removeCol.AM)
  AM[,"time"] <- 1
  PM <- select(data,-removeCol.PM)
  PM[,"time"] <- 2
  colnames(AM) <- 1:(dim(AM)[2])
  colnames(PM) <- 1:(dim(PM)[2])
  AMPM <- rbind(AM, PM)
  if(ncol(AMPM)==13){
    colnames(AMPM) <- c("Mrn_Vis","MRN","dipping.status",
                        "visit","sys","dia","sex","age","HbA1C",
                        "HR","CCB","dip","time")
  }else if(ncol(AMPM)==8){
    colnames(AMPM) <- c("Mrn_Vis","MRN","visit","sys",
                        "dia", "dip.status","dip","time" )  
  }
  
  AMPM$sys <- as.numeric(AMPM$sys)
  AMPM$dia <- as.numeric(AMPM$dia)
  #AMPM$MRN <- as.factor(AMPM$MRN)
  
  #sort
  AMPM <- arrange(AMPM, Mrn_Vis, visit)
  y.col <- which(colnames(AMPM) %in% "dip")
  n <- ncol(AMPM)
  col.names <- c(seq(n)[-which(seq(n)==y.col)],y.col)
  return(AMPM[,col.names])
}

#作用: 按照不同回訪時間或是不同人去切割訓練測試資料集
#參數: VisitOrCase=> 填Visit 或是 Case 
#removeCategory=> 要移除的Y類別 ex:"non dipper"
#for Visit.ver的參數:
#Train / Test=> 填Visit數字 ex:  1 / 2 or 1/ 2:3
#nfixed=> 人數固定(訓練和測試的Visit都存在才納入)
#for Case.ver的參數: seed隨機種子 
#return : test.true.mrn=> MRN / Training set / test set
TrainTest <- function(data = NULL, VisitOrCase = NULL, nfixed = T, Train = NULL,
                      Test = NULL, seed = NULL, removeCategory = NULL, Trainper = 0.8){
  results <- NULL
  if (VisitOrCase =="Visit"){
    if(is.null(removeCategory)){
      if (nfixed){
        #選出有test visit的MRN(人)
        test.true.MRN <- filter(data, data$visit %in% Test) %>%
          select(MRN)
        
        #timeSplit.22$MRN %in% TrainTest.22$test.true.MRN$MRN
        
        Training.proc <- filter(data, MRN %in% test.true.MRN$MRN) %>%
          filter(visit %in% Train)
        Testing.proc <- filter(data, MRN %in% test.true.MRN$MRN) %>%
          filter(visit %in% Test)
        print(nrow(Testing.proc))

        delMRN <- NULL 
        for (i in 1:dim(Testing.proc)[1]){
            if(all(Testing.proc[i,"MRN"]!= Training.proc[,"MRN"])){
              MRNnotinTrain <- Testing.proc[i,"MRN"]
              delMRN <-rbind(delMRN,MRNnotinTrain)
            }
        }
        print(nrow(Testing.proc))
        if (!is.null(delMRN)){
          Testing.proc <- Testing.proc[-which(Testing.proc$MRN %in% delMRN ),]
        }
        print(nrow(Testing.proc))
        results[["test.true.MRN"]] <- test.true.MRN
        results[["Training set"]] <- Training.proc
        results[["Test set"]] <- Testing.proc
        
      }else{
       Training <- data[which(data$visit %in% Train), ]
       Testing <- data[which(data$visit %in% Test), ]
       results[["Training set"]] <- Training
       results[["Test set"]] <- Testing
      }
    }else if (!is.null(removeCategory)){
      data <- data [-which(data[,dim(data)[2]] == removeCategory),]
      if (nfixed){
        #選出有test visit的MRN(人)
        test.true.MRN <- filter(data, visit %in% Test) %>%
          select(MRN)
        
        Training.proc <- filter(data, MRN %in% test.true.MRN) %>%
          filter(visit %in% Train)
        Testing.proc <- filter(data, MRN %in% test.true.MRN) %>%
          filter(visit %in% Test)
        delMRN <- NULL 
        for (i in 1:dim(Testing.proc)[1]){
          if(all(Testing.proc[i,"MRN"]!= Training.proc[,"MRN"])){
            MRNnotinTrain <- Testing.proc[i,"MRN"]
            delMRN <-rbind(delMRN,MRNnotinTrain)
          }
        }
        Testing.proc <- Testing.proc[-which(MRN %in% delMRN ),]
        results[["test.true.MRN"]] <- test.true.MRN
        results[["Training set"]] <- Training.proc
        results[["Test set"]] <- Testing.proc
        
      }else{
        Training <- data[which(data$visit %in% Train), ]
        Testing <- data[which(data$visit %in% Test), ]
        results[["Training set"]] <- Training
        results[["Test set"]] <- Testing
      }
    }
    return(results)
  }else if(VisitOrCase =="Case"){
    # case-wise 隨機切割ID 用ID所有資料去訓練 測試
    set.seed(seed)
    people <- as.data.frame(unique(data$MRN))
    trainID <- as.data.frame(sample(people$`unique(data$MRN)`,nrow(people)*Trainper))
    colnames(trainID)[1] <- "MRN"
    Training <- merge(trainID,data,by = "MRN",all.x = T)
    Testing <- rbind(Training, data)
    Testing <- Testing[!(duplicated(Testing) | duplicated(Testing, fromLast = TRUE)), ]
    results[["Training MRN"]] <- trainID
    results[["Training set"]] <- Training
    results[["Test set"]] <- Testing
    return(results)
  }else{
    print("ERROR")
    return("VisitOrCase error")
  }
  
}


#作用: 更改變數的資料型態
#參數: data=>資料 variable=>要轉換的變數 ex:"MRN" 
#type=>要轉換成的型態 ex:"factor"
#return: 資料集
TypeChange <- function(data = NULL, variable = NULL, type = NULL){
  A <- paste0("as.",type)
  data[,variable] <-  eval(parse(text = A))(data[,variable])
  return(data)
}


#### 測試變數交互作用 ####
#作用:測2個變數間的交互作用是否顯著
#參數: 
#data => 資料
#formula => effect 公式
#scaleCol => 要標準化的變數
#return: model summary 
Interact <- function(data1 = NULL, data2 = NULL, formula = NULL,
                     scaleCol = NULL, seed = NULL){
  results = NULL
  if(!is.null(data2)){
    data <- as.data.frame(rbind(data1, data2))
  }else{
    data <- as.data.frame(data1)
  }
  results[["data"]] <- data
  data[,scaleCol] <- scale(data[,scaleCol])
  results[["scale data"]] <- data
  glmfit <- glmer(formula,data = data , family = binomial)
  results[["model summary"]] <- summary(glmfit)
  return(results)
}

##### 連續型資料標準化 #####
#作用 :資料標準化
#參數 : data =>資料 NumVar =>連續型變數 Yname =>目標變數
#return : 標準化完的資料
DataScale <- function(data = NULL, NumVar = c("sys", "dia" ,"age" ,"HbA1C" ,"HR"),
                      Yname = "dip"){
  results = NULL
  Num <- which(colnames(data) %in% NumVar)
  NonNum.df <- data[,-Num]
  scale.df <- data[,Num]
  scale.df.done <- scale(scale.df)
  scale.ok <- cbind(NonNum.df,scale.df.done)
  y.col <- which(colnames(scale.ok) %in% Yname)
  n <- ncol(scale.ok)
  scale.ok <- scale.ok[,c(1:(y.col-1),(y.col+1):n,y.col)] 
  results[["scale.df"]] <- scale.ok
  return(results)
}


##### 暫放code ####
# x <- data.frame(k1=c(NA,NA,3,4,5), k2=c(1,NA,NA,4,5), data=1:5)
# y <- data.frame(k1=c(NA,2,NA,4,5), k2=c(NA,NA,3,4,5), data=1:5)
# merge(x, y, by=c("k1","k2"))
# timeID.col <- which(colnames(f) %in% timeIDname)
# timeCov.col <- which(colnames(f) %in% timeCov)
# timeCov.df <- f[,c(timeID.col,timeCov.col)]
# results[["timeCov.df"]] <- timeCov.df
# merge.df1 <- merge(a, timeCov.df, by=timeIDname, all.x=T)
# b <- unique(merge.df2)
# aa <- merge.df1[ which(is.na(merge.df1$Gender)),]
# sum(is.na(merge.df1$Age))
# sum(complete.cases(merge.df1))

