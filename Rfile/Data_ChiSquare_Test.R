set.seed(22)
sex = ifelse(sample.int(2,size = 200,replace = T)==1,"male","female")
set.seed(556)
status = ifelse(sample.int(2,size = 200,replace = T)==1,
                "shortsighted","longsighted") 
table(sex,status)
chisq.test(table(sex,status))


chi <- read.csv("TCHCData/chi_square.csv")
chi$A <- as.factor(chi$A)
chi$B <- as.factor(chi$B)
table(chi$A,chi$B)
chisq.test(table(chi$A,chi$B))
#data:  table(chi$A, chi$B)
#X-squared = 2.8152e-30, 
#df= 1, p-value = 1


chi1 <- read.csv("TCHCData/chi_square1.csv")
chi1$A <- as.factor(chi1$A)
chi1$B <- as.factor(chi1$B)
table(chi1$A,chi1$B)
chisq.test(table(chi1$A,chi1$B))
#data:  table(chi1$A, chi1$B)
#X-squared = 0.0073271, 
#df = 1, p-value= 0.9318

chi2 <- read.csv("TCHCData/chi_square2.csv")
chi2$A <- as.factor(chi2$A)
chi2$B <- as.factor(chi2$B)
table(chi2$A,chi2$B)
chisq.test(table(chi2$A,chi2$B))
#data:  table(chi2$A, chi2$B)
#X-squared = 0.003011, 
#df = 1, p-value =0.9562

chi3 <- read.csv("TCHCData/chi_square3.csv")
chi3$A <- as.factor(chi3$A)
chi3$B <- as.factor(chi3$B)
table(chi3$A,chi3$B)
chisq.test(table(chi3$A,chi3$B))
#data:  table(chi3$A, chi3$B)
#X-squared = 1.153e-30, df = 1,
#p-value = 1

