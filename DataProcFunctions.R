library(dplyr)
library(faraway)
library(randomForest)
library(tibble)
install.packages("farway")
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

#### 加入其他變數 ####
#作用: 加入其他變數 餅且合併成一個資料集
#參數:
#return:合併完的資料集
PlusCov <- function(data = NULL, Covfile = NULL,
                    timeIDname = "Mrn_Vis", timeCov = NULL,
                    IDname = "MRN", Cov = NULL,
                    impute = FALSE, Yname = "dip"){
  OrginData <- result.22$myData
  f <- read.csv(Covfile)
  Cov = c("CCB","Gender","Age")
  timeCov = c("HbA1C","HR")
  results = NULL
  OrginData <- data
  f <- read.csv(Covfile)
  ID.col <- which(colnames(f) == IDname)
  Cov.col <- which(colnames(f) %in% Cov)
  Cov.df <- f[ ,c(ID.col,Cov.col)]
  results[["Cov.df"]] <- Cov.df
  timeID.col <- which(colnames(f) %in% timeIDname)
  timeCov.col <- which(colnames(f) %in% timeCov)
  timeCov.df <- f[,c(timeID.col,timeCov.col)]
  results[["timeCov.df"]] <- timeCov.df
  
  #有沒有時間性的變數 分開來merge
  merge.df <- merge(OrginData, Cov.df, by = IDname, all.x=T)
  merge.df1 <- merge(merge.df, timeCov.df, by=timeIDname, all.x=T)
  a <- unique(merge.df1)
  aa <- merge.df1[ which(is.na(merge.df1$Gender)),]
  sum(complete.cases(merge.df1))
  #插補也分開 網路上說要先分訓練測試再插補
  if(impute){
    df.rf.impute <- rfImpute(Yname ~ ., ntree = 200, iter = 5, data = merge.df)
    if(nrow(OrginData)==nrow(df.rf.impute)){
      results[["AddCov.df"]] <- df.rf.impute
    }else{
      return("nrow not equal in two df")
    }
  }else{
    if(nrow(OrginData)==nrow(merge.df)){
      results[["AddCov.df"]] <- merge.df
    }else{
      return("nrow not equal in two df")
    }
  }
  
  return(results)
}


#作用: 時間分割 1筆切成早上晚上2筆
#參數: removeCol.AM=> 要丟掉的colname 
#return : 分割完且排序的dataframe
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
  AMPM$sys <- as.numeric(AMPM$sys)
  AMPM$dia <- as.numeric(AMPM$dia)
  #AMPM$MRN <- as.factor(AMPM$MRN)
  
  #sort
  AMPM <- arrange(AMPM, Mrn_Vis, visit)
  return(AMPM[, c(1:6, 8, 7)])
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




