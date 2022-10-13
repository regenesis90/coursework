# HELP :: https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
# INSTALL HELP :: https://bluebead38.blogspot.com/2017/07/r-cran.html
# LOAD HELP :: https://stackoverflow.com/questions/37095916/unable-to-load-r-package-mass

library(tidyverse)
library("MASS", lib.loc = 'C:/dev/R-4.1.1/library')

setwd('D:/OneDrive - 연세대학교 (Yonsei University)/Lectures/2021-2_대학원_도시모빌리티/211213_W15_Term Project_Report/data')

file_path = 'FIN_accident+violation_dummied_scaled2.csv'
data <- read_csv(file_path)

# factor로 바꿔주기
data[, 'accgrade'] = factor(data$accgrade, levels = 2:4, ordered = T)

summary(data)

model_HV <- polr(
  formula = accgrade~
    HV_ViolationRate+daynight+
    operation_01+operation_02+
    driver_condition_01+driver_condition_02+driver_condition_03+driver_condition_04+
    acc_reason_01+acc_reason_02+acc_reason_03+acc_reason_04+acc_reason_05+
    environment_01+environment_02+environment_03+environment_04+environment_05+
    workzone+
    alignmnet_01+alignmnet_02+alignmnet_03+alignmnet_04+
    reason_veh_01+reason_veh_02+reason_veh_03+reason_veh_04+
    line_01+line_02+line_03+
    slope_01+slope_02,
  data = data,
  Hess = TRUE
  )

summary(model_HV)

exp(coef(model_HV))

summary_table <- coef(summary(model_HV))
pval <- pnorm(abs(summary_table[, 't value']), lower.tail = FALSE)*2
summary_table <- cbind(summary_table, 'p value' = round(pval, 3))

summary_table
