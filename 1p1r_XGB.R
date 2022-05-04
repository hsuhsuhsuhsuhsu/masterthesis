#CC => train= dm1tr test=dm1te
library(caret)
library(xgboost)
library(gmodels)

dim(dm1tr)
dim(dm1te)

train <- dm1tr
test <- dm1te

tr_dat <- data.matrix(train[,-which(colnames(train)%in%"dip")])
View(te_dat)
te_dat <- data.matrix(test[,-which(colnames(test) %in% "dip")])

tr_label <- as.numeric(train$dip)

te_label <- as.numeric(test$dip)



folds <- createFolds(y=train$dip,k=10)
folds[1]

xgb_train = xgb.DMatrix(data = tr_dat, label =tr_label )
xgb_test = xgb.DMatrix(data = te_dat, label = te_label)


xg <- xgboost(data = xgb_train,max.depth = 3,
              eta = 0.05, nround =1000,
              min_child_weight = 5,
              colsample_bytree = 0.5,
              subsample = 0.8, 
              objective = "binary:logistic")

xgpred <- predict(xg, newdata = xgb_test)

log = capture.output(res <- CrossTable(y = as.numeric(xgpred > 0.4),
                                       x = test$dip,
                                       prop.chisq = F))

cm <- res$t
table(test$dip)
acc <- (cm[1]+cm[4]) / sum(cm)
sen <- cm[4] / (cm[2]+cm[4])
spe <- cm[1] / (cm[1]+cm[3])
print(paste(acc,sen,spe))


#set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 1000,
  eta = 0.05,
  max_depth = c(3,5),
  gamma = 1,
  colsample_bytree = c(0.5,0.7),
  min_child_weight = c(1,5),
  subsample = c(0.6,0.8)
)
# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)
# train the model for each parameter combination in the grid,
#   using CV to evaluate
tr_label <- as.factor(tr_label)
levels(tr_label) <- c("Non", "dip")


#Fitting nrounds = 1000, max_depth = 3,
#eta = 0.05, gamma = 1, colsample_bytree = 0.5, 
#min_child_weight = 5, subsample = 0.8 on full training set
xgb_train_1 = train(
  x = tr_dat,
  y = tr_label,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  verbosity = 0
)

View(xgb_train_1)





#PCA
install.packages("ade4")
install.packages("factoextra")
install.packages("magrittr")

library(ade4)
library(factoextra)
library(magrittr)












