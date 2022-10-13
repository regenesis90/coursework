#install.packages('lme4')
#install.packages('lmerTest')
#install.packages('ordinal')
#install.packages('future') #<<< 멀티프로세싱을 가능하게 하는 패키지

library(tidyverse)
#library(lme4)
#library(lmerTest)
library(ordinal)
#library("MASS", lib.loc = 'C:/dev/R-4.1.1/library')
library(future)

# 멀티프로세싱 설정
plan(multisession) # 현재 환경에서 쓸수있는 CPU코어를 탐지, 이 개수만큼 R세션 설정



# 워킹 디렉토리 고정
setwd('D:/OneDrive - 연세대학교 (Yonsei University)/Lectures/2021-2_대학원_선택이론/210908-_Term Project')

# 데이터 불러오기, 구조 확인하기
data <- read_csv('data/FIN_uphill_merged_ver3_202111.csv', locale = locale('ko',encoding = 'euc-kr'))

# 필요한 컬럼만 선택
data <- 
  data %>% 
  dplyr::select(VDS_ID, 콘존명, 평균교통량, 평균속도, 혼잡빈도수, `R(m)`, `I(%)`, `길어깨폭원(m)`,
         중방향계수, 중차량구성비, `가속차로_연장(m)`, `감속차로_연장(m)`, `감속차로_폭(m)`, 종점후터널유무,
         time, 차로번호, 현재운영_설치형식)

# 표준화를 위해, 데이터 중 chr 타입을 num 타입으로 변환하기
data[, 'R(m)'] <- as.numeric(data$`R(m)`)
data[, 'I(%)'] <- as.numeric(data$`I(%)`)

# 데이터 중 일부 값들에 대하여 표준화(scaling) 진행하기
## 표준화 : 데이터 평균 기준으로 값을 조정. scale(데이터명, center = TRUE, scale = TRUE)
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

# 표준화된 변수만 선택하여 다시 data 만들기
data <- 
  data %>% 
  dplyr::select(VDS_ID, 콘존명, 평균교통량_s, 평균속도_s, 혼잡빈도수,
                R_s, I_s, 길어깨폭원_s,
                중방향계수_s, 중차량구성비_s, 가속차로연장_s, 감속차로연장_s, 감속차로폭_s, 종점후터널유무,
                time, 차로번호, 현재운영_설치형식)

# 결측값 제거
data <- na.omit(data)

# 종속변수 및 카테고리형 변수들을 factor로 변환
data[, '혼잡빈도수'] <- 
  factor(data$혼잡빈도수,
         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
         order = T)
data[, 'VDS_ID'] <- as.factor(data$VDS_ID)
data[, '콘존명'] <- as.factor(data$콘존명)
data[, '종점후터널유무'] <- as.factor(data$종점후터널유무)
data[, 'time'] <- as.factor(data$time)
data[, '차로번호'] <- as.factor(data$차로번호)
data[, '현재운영_설치형식'] <- as.factor(data$현재운영_설치형식)

str(data)

# 다층모형 구성: 가장 적합한 랜덤효과 구조의 확정
## 기본모형 :: 절편과 오차항으로 구성
model0 <- clmm(혼잡빈도수 ~ 1 + (1|VDS_ID), data = data)

summary(model0)
# 랜덤효과 추출
var_cov <- data.frame(VarCorr(model0))
var_cov
# 급내상관계수 ICC 계산
ICC = var_cov$X.Intercept.[1]/(var_cov$X.Intercept.[1] + pi**2/3)
ICC # 0.6014: 상위수준에서 발견된 분산의 비중이 상대적으로 어떤지 가늠할 수 있는 수치

str(data)

# 모형
model1 <- clmm(혼잡빈도수~평균교통량_s+평균속도_s+현재운영_설치형식+R_s+I_s+길어깨폭원_s+중방향계수_s+중차량구성비_s+가속차로연장_s+감속차로연장_s+감속차로폭_s+종점후터널유무+(현재운영_설치형식|VDS_ID), data = data)

summary(model1)
traceback()

ordinal::soup
