
install.packages("dplyr")
library(dplyr)
library(foreign)
library(pscl)

data <- read.spss("C:/Users/YANG/Desktop/공모전준비/산재보험패널조사/01.SPSS/PSWCI_5TH_02_WIDE_DATA_V4.sav",
                  to.data.frame = TRUE, use.value.labels = FALSE)

# 연도별 조사 성공 패널

data_13 <- data

data_14 <- data %>% filter(p02 == 1)

data_15 <- data %>% filter(p03 == 1)

data_16 <- data %>% filter(p04 == 1)

data_17 <- data %>% filter(p05 == 1)


-------------------------------------------------------------------

### 경제활동 선택 결정요인 ###

  
## 표본1 - 이항로지스틱회귀분석 ##
  
# 종속변수 recoding
  
data_13$emp012 <- factor(data_13$emp012)
data_13$response1 <- ifelse(data_13$emp012 == 1, 1, 0)


#독립변수 recoding

data_13$나이 <- 2013 - data_13$A01001002 + 1
data_13$혼인 <- ifelse(data_13$A01003001 == 1, 0, 1) #이항변수
data_13$가구원수 <- data_13$I01001001
data_13$비근로소득 <- data_13$H01005001
data_13$요양기간 <- ifelse(data_13$con01 == 1 | data_13$con01 == 2, 1,
                       ifelse(data_13$con01 == 3 | data_13$con01 == 4, 2, 3))
data_13$장애여부 <- 2 - data_13$disa012
data_13$근무기간 <- ifelse(data_13$workperiod01 == 1, 1, 
                       ifelse(data_13$workperiod01 <= 6, 2, 
                              ifelse(data_13$workperiod01 == 7, 3,
                                     ifelse(data_13$workperiod01 == 8, 4,
                                            ifelse(data_13$workperiod01 >= 9 & data_13$workperiod01 <= 11, 5, 6)))))

data_13$노동조합 <- ifelse(data_13$C01002019 == 1 & data_13$C01002020 == 1, 1, 0)
data_13$직업재활서비스 <- 2 - data_13$jobservice01
data_13$의료재활서비스 <- 2 - data_13$medservice01
data_13$사회재활서비스 <- 2 - data_13$psyservice01


# 모델 구축

model1 <- glm(response1 ~ 나이 + 혼인 + 가구원수 + 비근로소득 + 요양기간 + 장애여부 + 근무기간 +
                노동조합 + 직업재활서비스 + 의료재활서비스 + 사회재활서비스, data = data_13, family=binomial(link="logit"))
summary(model1)

# 모델 평가

pR2(model1)
1-pchisq(model1$deviance, model1$df.residual) #P-value for deviance goodness-of-fit statistic

-------------------------------------------------------------------------------------

## 표본2 - 이항로지스틱회귀분석 ##

# 종속변수 : 13년 미취업자 중 14년 취업(=1) / 미취업 (=0)

d13_미취업 <- data_13 %>% filter(emp012 == 2) %>% filter(!is.na(emp022))

d13_미취업$emp022 <- factor(d13_미취업$emp022)

d13_미취업$response3 <- ifelse(d13_미취업$emp022 == 1, 1, 0)


# 독립변수 recoding

d13_미취업$나이 <- 2013 - d13_미취업$A01001002 + 1
d13_미취업$혼인 <- ifelse(d13_미취업$A01003001 == 1, 0, 1) #이항변수
d13_미취업$가구원수 <- d13_미취업$I01001001
d13_미취업$비근로소득 <- d13_미취업$H01005001
d13_미취업$요양기간 <- ifelse(d13_미취업$con01 == 1 | d13_미취업$con01 == 2, 1,
                       ifelse(d13_미취업$con01 == 3 | d13_미취업$con01 == 4, 2, 3))
d13_미취업$장애여부 <- 2 - d13_미취업$disa012
d13_미취업$근무기간 <- ifelse(d13_미취업$workperiod01 == 1, 1, 
                       ifelse(d13_미취업$workperiod01 <= 6, 2, 
                              ifelse(d13_미취업$workperiod01 == 7, 3,
                                     ifelse(d13_미취업$workperiod01 == 8, 4,
                                            ifelse(d13_미취업$workperiod01 >= 9 & d13_미취업$workperiod01 <= 11, 5, 6)))))

d13_미취업$노동조합 <- ifelse(d13_미취업$C01002019 == 1 & d13_미취업$C01002020 == 1, 1, 0)
d13_미취업$직업재활서비스 <- 2 - d13_미취업$jobservice01
d13_미취업$의료재활서비스 <- 2 - d13_미취업$medservice01
d13_미취업$사회재활서비스 <- 2 - d13_미취업$psyservice01


# 모델 구축

model2 <- glm(response3 ~ 나이 + 혼인 + 가구원수 + 비근로소득 + 요양기간 + 장애여부 + 근무기간 +
                노동조합 + 직업재활서비스 + 의료재활서비스 + 사회재활서비스, data = d13_미취업, family=binomial(link="logit"))
summary(model2)


# 모델 평가
library(pscl)

pR2(model2)   #LR test 
1-pchisq(model2$deviance, model2$df.residual) #P-value for deviance goodness-of-fit statistic

---------------------------------------------------------------------------------------------
  
## 표본3 - 백분율분석##
  
d13_취업_원직장 <- data_13 %>% filter(emp016 == 1)
d13_취업_타직장 <- data_13 %>% filter(emp016 == 2 | emp016 == 3 | emp016 == 4)
d13_미취업 <- data_13 %>% filter(emp016 == 5 | emp016 == 6)
d13_취업 <- data_13 %>% filter(emp016 == 1 | emp016 == 2 | emp016 == 3 | emp016 == 4)

d14_취업_원직장 <- data_14 %>% filter(emp026 == 1)
d14_취업_타직장 <- data_14 %>% filter(emp026 == 2 | emp026 == 3 | emp026 == 4)
d14_미취업 <- data_14 %>% filter(emp026 == 5 | emp026 == 6)
d14_취업 <- data_14 %>% filter(emp026 == 1 | emp026 == 2 | emp026 == 3 | emp026 == 4)

d15_취업_원직장 <- data_15 %>% filter(emp036 == 1)
d15_취업_타직장 <- data_15 %>% filter(emp036 == 2 | emp036 == 3 | emp036 == 4)
d15_미취업 <- data_15 %>% filter(emp036 == 5 | emp036 == 6)
d15_취업 <- data_15 %>% filter(emp036 == 1 | emp036 == 2 | emp036 == 3 | emp036 == 4)

d16_취업_원직장 <- data_16 %>% filter(emp046 == 1)
d16_취업_타직장 <- data_16 %>% filter(emp046 == 2 | emp046 == 3 | emp046 == 4)
d16_미취업 <- data_16 %>% filter(emp046 == 5 | emp046 == 6)
d16_취업 <- data_16 %>% filter(emp046 == 1 | emp046 == 2 | emp046 == 3 | emp046 == 4)

d17_취업_원직장 <- data_17 %>% filter(emp056 == 1)
d17_취업_타직장 <- data_17 %>% filter(emp056 == 2 | emp056 == 3 | emp056 == 4)
d17_미취업 <- data_17 %>% filter(emp056 == 5 | emp056 == 6)
d17_취업 <- data_17 %>% filter(emp056 == 1 | emp056 == 2 | emp056 == 3 | emp056 == 4)

--------------------------------------------------------------------------------------------
  
d14_미취업_원직장 <- d13_미취업 %>% filter(p02 == 1) %>% filter(emp026 == 1) 
d14_미취업_타직장 <- d13_미취업 %>% filter(p02 == 1) %>% filter(emp026 == 2 | emp026 == 3 | emp026 == 4)
d14_미취업_미취업 <- d13_미취업 %>% filter(p02 == 1) %>% filter(emp026 == 5 | emp026 == 6)
d14_미취업_취업 <- d13_미취업 %>% filter(p02 == 1) %>% filter(emp026 == 1 | emp026 == 2 | emp026 == 3 | emp026 == 4)

d15_미취업_원직장<- d14_미취업 %>% filter(p03 == 1) %>% filter(emp036 == 1)
d15_미취업_타직장 <-d14_미취업 %>% filter(p03 == 1) %>% filter(emp036 == 2 | emp036 == 3 | emp036 == 4)
d15_미취업_미취업 <- d14_미취업 %>% filter(p03 == 1) %>% filter(emp036 == 5 | emp036 == 6)  
d15_미취업_취업 <- d14_미취업 %>% filter(p03 == 1) %>% filter(emp036 == 1 | emp036 == 2 | emp036 == 3 | emp036 == 4)

d16_미취업_원직장 <- d15_미취업 %>% filter(p04 == 1) %>% filter(emp046 == 1)
d16_미취업_타직장 <- d15_미취업 %>% filter(p04 == 1) %>% filter(emp046 == 2 | emp046 == 3 | emp046 == 4)
d16_미취업_미취업 <- d15_미취업 %>% filter(p04 == 1) %>% filter(emp046 == 5 | emp046 == 6)
d16_미취업_취업 <- d15_미취업 %>% filter(p04 == 1) %>% filter(emp046 == 1 | emp046 == 2 | emp046 == 3 | emp046 == 4)

d17_미취업_원직장 <- d16_미취업 %>% filter(p05 == 1) %>% filter(emp056 == 1)
d17_미취업_타직장 <- d16_미취업 %>% filter(p05 == 1) %>% filter(emp056 == 2 | emp056 == 3 | emp056 == 4)
d17_미취업_미취업 <- d16_미취업 %>% filter(p05 == 1) %>% filter(emp056 == 5 | emp056 == 6)
d17_미취업_취업 <- d16_미취업 %>% filter(p05 == 1) %>% filter(emp056 == 1 | emp056 == 2 | emp056 == 3 | emp056 == 4)



# 백분율분석

# 13년 미취업/14년 취업 or 미취업자 

prop.table(table(d14_미취업_취업$jobservice01))*100  
prop.table(table(d14_미취업_미취업$jobservice01))*100

prop.table(table(d14_미취업_취업$medservice01))*100
prop.table(table(d14_미취업_미취업$medservice01))*100

prop.table(table(d14_미취업_취업$psyservice01))*100
prop.table(table(d14_미취업_미취업$psyservice01))*100

# 14년 미취업/15년 취업 or 미취업자 

prop.table(table(d15_미취업_취업$jobservice02))*100  
prop.table(table(d15_미취업_미취업$jobservice02))*100

prop.table(table(d15_미취업_취업$medservice02))*100
prop.table(table(d15_미취업_미취업$medservice02))*100

prop.table(table(d15_미취업_취업$psyservice02))*100
prop.table(table(d15_미취업_미취업$psyservice02))*100

# 15년 미취업/16년 취업 or 미취업자 

prop.table(table(d16_미취업_취업$jobservice03))*100  
prop.table(table(d16_미취업_미취업$jobservice03))*100

prop.table(table(d16_미취업_취업$medservice03))*100
prop.table(table(d16_미취업_미취업$medservice03))*100

prop.table(table(d16_미취업_취업$psyservice03))*100
prop.table(table(d16_미취업_미취업$psyservice03))*100

# 16년 미취업/17년 취업 or 미취업자 

prop.table(table(d17_미취업_취업$jobservice04))*100  
prop.table(table(d17_미취업_미취업$jobservice04))*100

prop.table(table(d17_미취업_취업$medservice04))*100
prop.table(table(d17_미취업_미취업$medservice04))*100

prop.table(table(d17_미취업_취업$psyservice04))*100
prop.table(table(d17_미취업_미취업$psyservice04))*100

# 시각화

# 14년

par(mfrow = c(1,1))

dt1 <- matrix(c(18.24, 60.58, 27.73, 24.13, 43.29, 28.35), 2, 3, byrow = TRUE)

barplot(dt1, beside = T, ylab = "서비스 이용률(%)", xlab = "14년도 취업상태", names.arg = c("직업재활서비스","의료재활서비스","사회재활서비스"), 
        col = c("white","gray"),las = 1, main = "13년도 미취업자의 14년도 취업상태 별 서비스이용률")


#15년
dt2 <- matrix(c(15.00, 60.00, 26.25, 25.28, 45.00, 33.33), 2, 3, byrow = TRUE)

barplot(dt2, beside = T, ylab = "서비스 이용률(%)", xlab = "15년도 취업상태", names.arg = c("직업재활서비스","의료재활서비스","사회재활서비스"), 
        col = c("white","gray"),las = 1, main = "14년도 미취업자의 15년도 취업상태 별 서비스이용률")


#16년
dt3 <- matrix(c(14.41, 56.76, 24.01, 22.35, 38.82, 34.11), 2, 3, byrow = TRUE)

barplot(dt3, beside = T, ylab = "서비스 이용률(%)", xlab = "16년도 취업상태", names.arg = c("직업재활서비스","의료재활서비스","사회재활서비스"), 
        col = c("white","gray"),las = 1, main = "15년도 미취업자의 16년도 취업상태 별 서비스이용률")


#17년

dt4 <- matrix(c(11.76, 54.29, 23.07, 17.64, 42.64, 23.52), 2, 3, byrow = TRUE)

barplot(dt4, beside = T, ylab = "서비스 이용률(%)", xlab = "17년도 취업상태", names.arg = c("직업재활서비스","의료재활서비스","사회재활서비스"), 
        col = c("white","gray"),las = 1, main = "16년도 미취업자의 17년도 취업상태 별 서비스이용률")




legend("topright", fill = c("white", "gray"), legend = c("미취업자","취업자"), cex = 0.2)


