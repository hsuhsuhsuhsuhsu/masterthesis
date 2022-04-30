library(fastDummies)

DD <- CCTr
EE <- CCTe

DC <- dummy_cols(DD,select_columns = c("Gender","DM","HOS"))
DC1 <- DC[,-which(colnames(DC) %in% c("MRN","Gender","DM","HOS"))]

EC <- dummy_cols(EE,select_columns = c("Gender","DM","HOS"))
EC1 <- EC[,-which(colnames(EC) %in% c("MRN","Gender","DM","HOS"))]

#DC1=>train EC1=>Test
library(randomForest)
dm <- randomForest(factor(dip)~.,data = DC1, method = "class")
dm$confusion
Dpred <- predict(dm,EC1)
table(real = EC1$dip, pred = Dpred)
