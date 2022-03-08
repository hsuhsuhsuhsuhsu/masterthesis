set.seed(22)
sex = ifelse(sample.int(2,size = 200,replace = T)==1,"male","female")
set.seed(556)
status = ifelse(sample.int(2,size = 200,replace = T)==1,
                "shortsighted","longsighted") 
table(sex,status)
chisq.test(table(sex,status))

Yescov <- read.csv("TCHCData/RFimp.csv ")
Yescov <- Yescov[which(Yescov$visit_HBP_Dmode %in% 1:3),]
NOcov <- result.22$myData
NOcov <- NOcov[which(NOcov$visit_HBP_Dmode %in% 1:3),]
dim(Yescov)
table(Yescov$visit_HBP_Dmode,Yescov$dip)
dim(NOcov)
table(NOcov$visit_HBP_Dmode,NOcov$dip)

WCov <- ifelse(Yescov$dip ==1 ,"non-dipping","dipping")
WOCov <- ifelse(NOcov$dip ==1 ,"non-dip","dip" )

a <- data.frame(x=c(rep(0,171),rep(0,183)),y=c(556,593))
colnames(a) <- c("dip","nondip")
rownames(a) <- c("WithCov" , "WithOutCov")


table(WCov)
table(WOCov)
table(WCov,WOCov)
chisq.test()
as.table(c(180,192,581,625),nrow = 2, ncol = 2)
a <- data.frame(x=c(171,556),y=c(183,593))
colnames(a) <- c("dip","nondip")
table(a)