install.packages("dplyr")
library(dplyr)

data <- read.csv("C:/Users/YANG/Desktop/공모전준비/산재보험패널조사/01.SPSS/wide type.csv")
data_copy <- read.csv("C:/Users/YANG/Desktop/공모전준비/산재보험패널조사/01.SPSS/wide type.csv")

# 분석대상자 추출
data_copy <- data_copy %>% filter(emp016 == 1) 


# 종속변수 정의
attach(data_copy)

data_copy$work_period <- ifelse(D20200000 == 1 & D03200000 == 1 & D04200000 == 1 & D05200000 == 1, 5,
                                ifelse(D20200000 == 1 & D03200000 == 1 & D04200000 == 1, 4,
                                       ifelse(D20200000 == 1 & D03200000 == 1, 3,
                                              ifelse(D20200000 == 1, 2, 1))))
# 결측치제거
data_copy <- data_copy %>% filter(!is.na(data_copy$work_period))


# 독립변수 recoding

data_copy$요양기간 <- ifelse(data_copy$con05 <= 2, 1, ifelse(data_copy$con05 <= 4, 2, 3))

data_copy$회사관계유지 <- 2 - data_copy$Ba01004001

data_copy$사업장만족도 <- 6 - data_copy$Ba01004014


data_copy$장해등급 <- ifelse(data_copy$disa056 <= 2, 1, 
                         ifelse(data_copy$disa056 <= 5, 2, 3))

data_copy$업무수행능력 <- ifelse(data_copy$Bb05007002 <= 3 | data_copy$Bb05007002 == 11 , 1,
                           ifelse(data_copy$Bb05007002 <= 6, 2, 3))

data_copy$건강상태 <- data_copy$Gb05001001

data_copy$일자리만족도 <- data_copy$E105011010


  

# 독립변수 정의

during <- select(data_copy, 요양기간, 회사관계유지, 사업장만족도)

after <- select(data_copy, 장해등급, 업무수행능력, 건강상태, 일자리만족도)




#### 상관관계 분석 ####

# 고용유지기간과 요양기간/회사관계유지/사업장만족도 간의 관계

cor.test(data_copy$요양기간, data_copy$work_period)
cor.test(data_copy$회사관계유지, data_copy$work_period)
cor.test(data_copy$사업장만족도, data_copy$work_period)


# 고용유지기간과 장해등급/업무수행능력/건강상태/일자리만족도 간의 관계

cor.test(data_copy$장해등급, data_copy$work_period)
cor.test(data_copy$업무수행능력, data_copy$work_period)
cor.test(data_copy$건강상태, data_copy$work_period)
cor.test(data_copy$일자리만족도, data_copy$work_period)





#### 다중회귀분석- 고용유지기간에 영향을 미치는 요인 ####

model1 <- lm(work_period ~ 요양기간 + 회사관계유지 + 사업장만족도, data = data_copy)

model2 <- lm(work_period ~ 장해등급 + 업무수행능력 + 건강상태 + 일자리만족도, data = data_copy)

summary(model1)

summary(model2)
