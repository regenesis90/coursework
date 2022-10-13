#install.packages('lme4')
#install.packages('lmerTest')
#install.packages('ordinal')
#install.packages('future') #<<< ��Ƽ���μ����� �����ϰ� �ϴ� ��Ű��

library(tidyverse)
#library(lme4)
#library(lmerTest)
library(ordinal)
#library("MASS", lib.loc = 'C:/dev/R-4.1.1/library')
library(future)

# ��Ƽ���μ��� ����
plan(multisession) # ���� ȯ�濡�� �����ִ� CPU�ھ Ž��, �� ������ŭ R���� ����



# ��ŷ ���丮 ����
setwd('D:/OneDrive - �������б� (Yonsei University)/Lectures/2021-2_���п�_�����̷�/210908-_Term Project')

# ������ �ҷ�����, ���� Ȯ���ϱ�
data <- read_csv('data/FIN_uphill_merged_ver3_202111.csv', locale = locale('ko',encoding = 'euc-kr'))

# �ʿ��� �÷��� ����
data <- 
  data %>% 
  dplyr::select(VDS_ID, ������, ��ձ��뷮, ��ռӵ�, ȥ��󵵼�, `R(m)`, `I(%)`, `��������(m)`,
         �߹�����, ������������, `��������_����(m)`, `��������_����(m)`, `��������_��(m)`, �������ͳ�����,
         time, ���ι�ȣ, ����_��ġ����)

# ǥ��ȭ�� ����, ������ �� chr Ÿ���� num Ÿ������ ��ȯ�ϱ�
data[, 'R(m)'] <- as.numeric(data$`R(m)`)
data[, 'I(%)'] <- as.numeric(data$`I(%)`)

# ������ �� �Ϻ� ���鿡 ���Ͽ� ǥ��ȭ(scaling) �����ϱ�
## ǥ��ȭ : ������ ��� �������� ���� ����. scale(�����͸�, center = TRUE, scale = TRUE)
data <- transform(data,
                  ��ձ��뷮_s = scale(data$��ձ��뷮),
                  ��ռӵ�_s = scale(data$��ռӵ�),
                  R_s = scale(data$`R(m)`),
                  I_s = scale(data$`I(%)`),
                  ��������_s = scale(data$`��������(m)`),
                  �߹�����_s = scale(data$�߹�����),
                  ������������_s = scale(data$������������),
                  �������ο���_s = scale(data$`��������_����(m)`),
                  �������ο���_s = scale(data$`��������_����(m)`),
                  ����������_s = scale(data$`��������_��(m)`))

# ǥ��ȭ�� ������ �����Ͽ� �ٽ� data �����
data <- 
  data %>% 
  dplyr::select(VDS_ID, ������, ��ձ��뷮_s, ��ռӵ�_s, ȥ��󵵼�,
                R_s, I_s, ��������_s,
                �߹�����_s, ������������_s, �������ο���_s, �������ο���_s, ����������_s, �������ͳ�����,
                time, ���ι�ȣ, ����_��ġ����)

# ������ ����
data <- na.omit(data)

# ���Ӻ��� �� ī�װ����� �������� factor�� ��ȯ
data[, 'ȥ��󵵼�'] <- 
  factor(data$ȥ��󵵼�,
         levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16), 
         order = T)
data[, 'VDS_ID'] <- as.factor(data$VDS_ID)
data[, '������'] <- as.factor(data$������)
data[, '�������ͳ�����'] <- as.factor(data$�������ͳ�����)
data[, 'time'] <- as.factor(data$time)
data[, '���ι�ȣ'] <- as.factor(data$���ι�ȣ)
data[, '����_��ġ����'] <- as.factor(data$����_��ġ����)

str(data)

# �������� ����: ���� ������ ����ȿ�� ������ Ȯ��
## �⺻���� :: ������ ���������� ����
model0 <- clmm(ȥ��󵵼� ~ 1 + (1|VDS_ID), data = data)

summary(model0)
# ����ȿ�� ����
var_cov <- data.frame(VarCorr(model0))
var_cov
# �޳������� ICC ���
ICC = var_cov$X.Intercept.[1]/(var_cov$X.Intercept.[1] + pi**2/3)
ICC # 0.6014: �������ؿ��� �߰ߵ� �л��� ������ ��������� ��� ������ �� �ִ� ��ġ

str(data)

# ����
model1 <- clmm(ȥ��󵵼�~��ձ��뷮_s+��ռӵ�_s+����_��ġ����+R_s+I_s+��������_s+�߹�����_s+������������_s+�������ο���_s+�������ο���_s+����������_s+�������ͳ�����+(����_��ġ����|VDS_ID), data = data)

summary(model1)
traceback()

ordinal::soup