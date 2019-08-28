install.packages("dplyr")
library(dplyr)
library(foreign)

data <- read.spss("C:/Users/YANG/Desktop/공모전준비/산재보험패널조사/01.SPSS/PSWCI_5TH_02_WIDE_DATA_V4.sav",
                  to.data.frame = TRUE, use.value.labels = FALSE)

# 연도별 조사 성공 패널

data_13 <- data

data_14 <- data %>% filter(p02 == 1)

data_15 <- data %>% filter(p03 == 1)

data_16 <- data %>% filter(p04 == 1)

data_17 <- data %>% filter(p05 == 1)


# 표본1 구축  

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



# 표본2 구축

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

--------------------------------------------------------------------------------------------

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


model_13 <- glm(response1 ~ 나이 + 혼인 + 가구원수 + 비근로소득 + 요양기간 + 장애여부 + 근무기간 +
          
                          노동조합 + 직업재활서비스 + 의료재활서비스 + 사회재활서비스, data = data_13, family=binomial(link="logit"))
 # 모델평가
summary(model_13) #Wald test
anova(model_13)   #LR test 

1-pchisq(model_13$deviance, model_13$df.residual) #P-value for deviance goodness-of-fit statistic



## 13년 표본2 분석 ## 다중회귀분석 ##

# 종속변수 recoding

data_13$emp016 <- factor(data_13$emp016)
data_13$response2 <- ifelse(data_13$emp016 == 1, 1, 
                           ifelse(data_13$emp016 == 2 | data_13$emp016 == 3 | data_13$emp016 == 4, 2, 3))

#독립변수 recoding (앞서정의한 것과 동일)


# 다중로지스틱회귀모델 # - y가 범주형(3개이상) / baseline은 emp016 == 1 (원직장취업인 경우)

install.packages("nnet")
library(nnet)

model_13_1 <- multinom(response2 ~ 나이 + 혼인 + 가구원수 + 비근로소득 +
                         요양기간 + 장애여부 + 근무기간 + 노동조합 + 직업재활서비스 + 의료재활서비스 + 사회재활서비스, data = data_13)
summary(model_13_1)


# 각 계수의 p값 산출
z = summary(model_13_1)$coefficients / summary(model_13_1)$standard.errors
p = (1-pnorm(abs(z), 0, 1)) * 2
print(p)





