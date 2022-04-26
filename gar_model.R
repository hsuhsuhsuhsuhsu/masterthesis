library(readr)
library(dplyr)
source("ModelBuildFunction.r")
changeType <- function(data, adjustType){
  for(i in 1:dim(data)[2]){
    this.var <- which(colnames(data)[i] == names(adjustType))
    if(is.na(adjustType[[this.var]][[1]])){
      print(paste0("Skip (missing pre-defined): ", colnames(data)[i]))
      next
    }
    if(adjustType[[this.var]][[1]] == "f"){#factor
      if(length(adjustType[[this.var]]) == 1){
        data[[i]] <- as.factor(data[[i]])
      }else{
        data[[i]] <- factor(data[[i]], levels = adjustType[[this.var]][[2]])
      }
    }else{#numeric
      data[[i]] <- as.numeric(data[[i]])
    }
  }
  return(data)
}
initial_adjustType <- function(x){
  x$MRN <- "f"
  x$sbp_d <- "n"
  x$dbp_d <- "n"
  x$sbp_n <- "n"
  x$dbp_n <- "n"
  x$Gender <- "f"
  x$Age <- "n"
  x$HR <- "n"
  x$Drug_conut <- list("f", LEVELS = c(0:5))
  x$DM <- list("f", LEVELS = c(1:3))
  x$HOS <- "f"
  x$BMI <- "n"
  x$Waist <- "n"
  x$Walk_TM_week <- list("f", LEVELS = c(0:7))
  x$anti_HP <- list("f", LEVELS = c(1:13))
  x$office_peri_L_sys <- 'n'
  x$office_peri_L_dia <- 'n'
  x$dip #skip
  x$Mrn_Vis <- "f"
  x$sys_d <- "n"
  x$dia_d <- "n"
  x$sys_n <- "n"
  x$dia_n <- 'n'
  x$visit <- list("f", LEVELS = c(1:3))
  x$dip.status <- "f"
  x$hos <- "f"
  x$Eco_child <- list("f", LEVELS = c(1:4))
  x$T_pain <- list("f", LEVELS = c(1:3))
  return(x)
}

dataList <- list(NULL)

#### read data into dataList####
u <- 1
myName <- NULL
for(file in list.files(paste0(getwd(), "/Div"))){
  dataList[[u]] <- read_csv(paste0("Div/", file))
  tmp <- which(colnames(dataList[[u]]) == "MRN")
  dataList[[u]] <- dataList[[u]][,tmp:dim(dataList[[u]])[2]]
  myName <- c(myName, colnames(dataList[[u]]))
  names(dataList)[[u]] <- file
  u <- u + 1
}


#### data preprocessing ####

#pre-defined all variable type
myName <- unique(myName)
adjustType <- as.list(rep(NA, length(myName)))
names(adjustType) <- myName
adjustType <- initial_adjustType(adjustType)

#adjust all variable type
count <- 1
for(data in dataList){
  dataList[[count]] <- changeType(data = data, adjustType = adjustType)
  print(paste0("Done: ", count))
  count = count + 1
}






#### build model ####

#check na
lapply(1:length(dataList), FUN = function(x) all(apply(dataList[[x]], 2, is.na)))

#build foumula
myFomula <- list(RF = list(CASE = list(COV = factor(dip)~sbp_d+dbp_d+sbp_n+dbp_n+Gender+Age+HR+Drug_conut+DM+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia,
                                       NO_COV = factor(dip)~sys_d+dia_d+sys_n+dia_n+HOS),
                           VISIT = list(COV = factor(dip)~sbp_d+sbp_n+dbp_d+dbp_n+Gender+Age+HR+Drug_conut+DM+HOS+BMI+Waist+Eco_child+T_pain,
                                        NO_COV = factor(dip)~sys_d+sys_n+dia_d+dia_n+HOS)),
                 BIM = list(VISIT = list(COV = dip~visit+sbp_d+sbp_n+dbp_d+dbp_n+Gender+Age+HR+Drug_conut+DM+HOS+BMI+Waist+Eco_child+T_pain,
                                         NO_COV = dip~visit+sys_d+sys_n+dia_d+dia_n+HOS)))

#start build model
#RF
mySeed <- 419
classwt = NULL
#mtry = 15

#BIM
glmControl = "maxfun"
random = "+(1|time)"

#ttm: train_test_match, first=train index in dataList, sec = testing index...
ttm <- list(c(2, 1), c(4, 3), 
            c(6, 7), c(5, 8),
            c(10, 11), c(9, 12))


resultSets <- list(NULL)
for(i in 1:length(ttm)){
  detect_model <- grepl("3", names(dataList)[ttm[[i]][2]], ignore.case = TRUE)
  detect_case <- grepl("case", names(dataList)[ttm[[i]][2]], ignore.case = TRUE)
  detect_nocov <- grepl("nocov", names(dataList)[ttm[[i]][2]], ignore.case = TRUE)
  dataList[[ttm[[i]][1]]] <- as.data.frame(dataList[[ttm[[i]][1]]])
  if(detect_model){ #BIM
    if(detect_nocov){#nocov
      this.fomula <- myFomula$BIM$VISIT$NO_COV
    }else{#cov
      this.fomula <- myFomula$BIM$VISIT$COV
    }
    resultSets[[i]] <- BiMMforest1(traindata = dataList[[ttm[[i]][1]]], 
                                    testdata = dataList[[ttm[[i]][2]]],
                                    formula = this.fomula, 
                                    random = random,
                                    seed = mySeed, 
                                    glmControl = glmControl)
    names(resultSets)[i] <- paste0("BIM: ", i)
    
  }else{ #RF
    if(detect_case){#case
      if(detect_nocov){#nocov
        this.fomula <- myFomula$RF$CASE$NO_COV
      }else{#cov
        this.fomula <- myFomula$RF$CASE$COV
      }
    }else{#visit
      if(detect_nocov){#nocov
        this.fomula <- myFomula$RF$VISIT$NO_COV
      }else{#cov
        this.fomula <- myFomula$RF$VISIT$COV
      }
    }

    resultSets[[i]] <- RF(traindata = dataList[[ttm[[i]][1]]], 
                          testdata = dataList[[ttm[[i]][2]]],
                          formula = this.fomula, 
                          classwt = classwt,
                          seed = mySeed)
    
    names(resultSets)[i] <- paste0("RF: ", i)
    
  }
  
  print(paste0("Done: ", i))
}









#### Evaluate ####

resultSets$`RF: 1`$`Train acc sen spe`
resultSets$`RF: 1`$`Test acc sen spe`
resultSets$`RF: 2`$`Test acc sen spe`
resultSets$`RF: 2`$`Train acc sen spe`
resultSets$`RF: 3`$`Test acc sen spe`
resultSets$`RF: 3`$`Train acc sen spe`
resultSets$`BIM: 4`
resultSets$`BIM: 6`
resultSets$`RF: 5`$`Test acc sen spe`
resultSets$`RF: 5`$`Train acc sen spe`
#