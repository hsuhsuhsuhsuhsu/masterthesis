source("Train_Test_file_cov_select.r")
source("DataProcFunctions.r")
source("ModelBuildFunction.r")
#VNTr.12 VNTe.3 VNTr.1 VNTe.2
#VCTr.12 VCTe.3 VCTr.1 VCTe.2
F5 <- dip~visit+sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia
F6 <- dip~visit+sbp+dbp+time+HOS
F7 <- factor(dip)~visit+sbp+dbp+Gender+Age+HR+Drug_conut+DM+time+HOS+BMI+Waist+Walk_TM_week+anti_HP+office_peri_L_sys+office_peri_L_dia
F8 <- factor(dip)~visit+sbp+dbp+time+HOS
seed = 123






