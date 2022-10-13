# HELP :: https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
# INSTALL HELP :: https://bluebead38.blogspot.com/2017/07/r-cran.html
# LOAD HELP :: https://stackoverflow.com/questions/37095916/unable-to-load-r-package-mass

library(tidyverse)
library('MASS', lib.loc = 'C:/dev/R-4.1.1/library')

setwd('D:/OneDrive - 연세대학교 (Yonsei University)/Lectures/2021-2_대학원_선택이론/210908-_Term Project/data')

file_path = 'FIN_uphill_merged.csv'
data <- read_csv(file_path, locale = locale('ko', encoding = 'euc-kr'))

# factor로 바꿔주기
data[, '혼잡빈도수'] = factor(data$혼잡빈도수, levels = 0:16, ordered = T)

str(data)
summary(data)

# 데이터 중 일부 값들에 대하여 표준화(scaling) 진행하기
## 표준화 : 데이터 평균 기준으로 값을 조정. scale(데이터명, center = TRUE, scale = TRUE)
data[, 'R(m)'] <- as.numeric(data$`R(m)`)
data[, 'I(%)'] <- as.numeric(data$`I(%)`)

data <- transform(data,
                  평균교통량_s = scale(data$평균교통량),
                  평균속도_s = scale(data$평균속도),
                  R_s = scale(data$`R(m)`),
                  I_s = scale(data$`I(%)`),
                  길어깨폭원_s = scale(data$`길어깨폭원(m)`),
                  중방향계수_s = scale(data$중방향계수),
                  중차량구성비_s = scale(data$중차량구성비),
                  가속차로연장_s = scale(data$`가속차로_연장(m)`),
                  감속차로연장_s = scale(data$`감속차로_연장(m)`),
                  감속차로폭_s = scale(data$`감속차로_폭(m)`))

#결측값 제거
data <- na.omit(data)

# 순서형 로짓모형 구축하기
model <- polr(
  formula = 혼잡빈도수~
    평균교통량_s+
    R_s+
    I_s+
    중차량구성비_s+
    #가속차로연장_s+
    감속차로연장_s+
    감속차로폭_s+
    #종점후터널유무+
    time_0306+
    time_0609+
    time_0912+
    time_1215+
    time_1518+
    time_1821+
    time_2124+
    lanetype_01+lanetype_02,
  data = data,
  Hess = TRUE
  )

summary(model)
exp(coef(model))
summary_table <- coef(summary(model))
pval <- pnorm(abs(summary_table[, 't value']), lower.tail = FALSE)*2
summary_table <- cbind(summary_table, 'p value' = round(pval, 3))
summary_table


# 기술통계량 출력
library(psych)
describe(data)
