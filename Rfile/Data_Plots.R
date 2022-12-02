getwd()
test <- read.csv("TCHCData/cov_V3Test.csv")
train <- read.csv("TCHCData/cov_V12Train.csv")
n <- dim(train)[1]
p <- dim(train)[2]
library(ggplot2)
library(gridExtra)
library(GGally)
train <- train[,-1]
test <- test[,-1]
this.data <- train
this.data$dip <- as.factor(this.data$dip)
this.data$visit <- as.factor(this.data$visit)
this.data$sex <- as.factor(this.data$sex)
this.data$CCB <- as.factor(this.data$CCB)
this.data$time <- as.factor(this.data$time)

numericList <- unlist(lapply(this.data, is.numeric))
numInd <- which(numericList)
#看index plot
#age 可以跟visit或time決定?
myF <- lapply(numInd, FUN = indPlot, myGroup = "visit")#age,hr,hba1c
myF <- lapply(numInd, FUN = indPlot, myGroup = "time")

myF <- lapply(numInd, FUN = indPlot, myGroup = "sex")#男女比不對稱
myF <- lapply(numInd, FUN = indPlot, myGroup = "CCB")#hr,hba1c
marrangeGrob(myF, nrow = 2, ncol = 3)

#看boxplot 有outlier
myF <- lapply(numInd, FUN = boxPlot)
marrangeGrob(myF, nrow = 2, ncol = 3)

#看scatter matrix 可能有共線性
ggpairs(this.data[, numericList], title="123",
        aes(col = this.data$dip, alpha = 0.3)) 


#取出outlier
#if return "<0 rows> (or 0-length row.names)" 表示沒有outlier
lapply(names(numInd), FUN = getOutlier)









indPlot <- function(i = NULL, data = this.data, myGroup = "dip"){
  ggplot(this.data, aes(x = 1:n, y = this.data[[i]], col = this.data[[myGroup]])) +
    geom_point() +
    theme(legend.position = "left")+
    labs(title = colnames(this.data)[i])
}

boxPlot <- function(i = NULL, data = this.data){
  ggplot(train, aes(x = train[[i]])) +
    geom_boxplot(outlier.color = "red") +
    theme(legend.position = "None")+
    labs(title = colnames(this.data)[i])
}
getOutlier <- function(var = NULL, data = this.data){
  s <- sd(this.data[,var])
  m <- mean(this.data[,var])
  list(lower = this.data[this.data[,var] <= m - 3*s,],
    upper = this.data[this.data[,var] >= m + 3*s,])
}




library(ggplot2)
start <- 1
end <- 9
data <- mapply(lapply(start:end, FUN = function(x) seq(x, end)),
               start:end, FUN = function(y, x) rbind(x, y))
plotdata <- NULL
for(i in 1:length(data)){
  plotdata <- rbind(plotdata, as.data.frame(t(data[[i]])))
}
ggplot(plotdata, aes(x = x, y = y)) +
  geom_point(size = 3)



