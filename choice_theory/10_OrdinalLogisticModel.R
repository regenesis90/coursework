# HELP :: https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
# INSTALL HELP :: https://bluebead38.blogspot.com/2017/07/r-cran.html
# LOAD HELP :: https://stackoverflow.com/questions/37095916/unable-to-load-r-package-mass

library(tidyverse)
library('MASS', lib.loc = 'C:/dev/R-4.1.1/library')

setwd('D:/OneDrive - �������б� (Yonsei University)/Lectures/2021-2_���п�_�����̷�/210908-_Term Project/data')

file_path = 'FIN_uphill_merged.csv'
data <- read_csv(file_path, locale = locale('ko', encoding = 'euc-kr'))

# factor�� �ٲ��ֱ�
data[, 'ȥ��󵵼�'] = factor(data$ȥ��󵵼�, levels = 0:16, ordered = T)

str(data)
summary(data)

# ������ �� �Ϻ� ���鿡 ���Ͽ� ǥ��ȭ(scaling) �����ϱ�
## ǥ��ȭ : ������ ��� �������� ���� ����. scale(�����͸�, center = TRUE, scale = TRUE)
data[, 'R(m)'] <- as.numeric(data$`R(m)`)
data[, 'I(%)'] <- as.numeric(data$`I(%)`)

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

#������ ����
data <- na.omit(data)

# ������ �������� �����ϱ�
model <- polr(
  formula = ȥ��󵵼�~
    ��ձ��뷮_s+
    R_s+
    I_s+
    ������������_s+
    #�������ο���_s+
    �������ο���_s+
    ����������_s+
    #�������ͳ�����+
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


# �����跮 ���
library(psych)
describe(data)