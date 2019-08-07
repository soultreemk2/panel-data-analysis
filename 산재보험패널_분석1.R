##### 목표: 산재근로자가 경험한 의료서비스가 건강과 삶의 질에 미치는 영향 분석 #####

install.packages("haven")
library(haven)
install.packages("dplyr")
library(dplyr)

data <- read_spss("C:/Users/YANG/Desktop/공모전준비/산재보험패널조사/01.SPSS/PSWCI_5TH_02_WIDE_DATA_V4.sav")
data_copy <- read_spss("C:/Users/YANG/Desktop/공모전준비/산재보험패널조사/01.SPSS/PSWCI_5TH_02_WIDE_DATA_V4.sav")


# 암호화 되어있는 변수 이름 재설정
data_copy <- rename(data_copy, explain = Ba01002001, evaluation = Ba01002002, treatment = Ba01002003)

# 독립변수 recoding
data_copy$gender <- data_copy$gender05 - 1
data_copy$period <- data_copy$con05 - 1
data_copy$grade <- 6 - data_copy$disa056
data_copy$service <- data_copy$medservice05 - 1	

# 종속변수를 dummy variable로 변환
data_copy <- transform(data_copy, 
                       health = ifelse(data_copy$Gb05001001 <= "2", 0, 1),
                       help = ifelse(data_copy$Gb05004001 <= "2", 0, 1),
                       satisfy =  ifelse(data_copy$Gb05008007 <= "3", 0, 1))

# 종속변수, 매개변수, 독립변수 설정
health_life <- subset(data_copy, select = c(health, help, satisfy))

medical_service <- subset(data_copy, select=c(explain, evaluation, treatment))

personal <- subset(data_copy, select=c(gender,period,grade,service))


# 분석대상자 추출
data_copy <- data_copy %>% filter(p05 == 1)
data_copy <- data_copy %>% filter(!is.na(evaluation) & !is.na(treatment))

dim(data)


#### model1 (건강 및 삶의 질 ~ 경험의료서비스) #####

## chisquare test - 관련성 분석 ##


# '전반적 건강상태'와 '주기적 회복평가/치료기간 적정/의사로부터의 상세한 설명' 사이의 관계

attach(data_copy)

chisq.test(medical_service$evaluation, health_life$health)

chisq.test(medical_service$treatment, health_life$health)

chisq.test(medical_service$explain, health_life$health)

# '일상생활도움'과  '주기적 회복평가/치료기간 적정/의사로부터의 상세한 설명' 사이의 관계

chisq.test(medical_service$evaluation, health_life$help)

chisq.test(medical_service$treatment, health_life$help)

chisq.test(medical_service$explain, health_life$help)

# '전반적만족도'와 '주기적 회복평가/치료기간 적정/의사로부터의 상세한 설명' 사이의 관계

chisq.test(medical_service$evaluation, health_life$satisfy)

chisq.test(medical_service$treatment, health_life$satisfy)

chisq.test(medical_service$explain, health_life$satisfy)


## logistic regression - 요인 분석 ##

# '전반적 건강상태'에 영향을 미치는 요인

logistic1 <- glm(health ~ explain + evaluation + treatment, data=data_copy, family = 'binomial')


# '일상생활도움'에 영향을 미치는 요인

logistic2 <- glm(help ~ explain + evaluation + treatment, data=data_copy, family = 'binomial')


# '전반적만족도'에 영향을 미치는 요인

logistic3 <- glm(satisfy ~ explain + evaluation + treatment, data=data_copy, family = 'binomial')




#### model2 (건강 및 삶의 질 ~ 경험의료서비스 + 인구사회학적특성) #####

## chisquare test - 관련성 분석 ##


# '전반적 건강상태'와 '성별/요양기간/장해등급/의료서비스경험' 사이의 관계

chisq.test(personal$gender, health_life$health)

chisq.test(personal$period, health_life$health)

chisq.test(personal$grade,  health_life$health)

chisq.test(personal$service, health_life$health)

# '일상생활도움'과 '성별/요양기간/장해등급/의료서비스경험' 사이의 관계

chisq.test(personal$gender, health_life$help)

chisq.test(personal$period, health_life$help)

chisq.test(personal$grade, health_life$help)

chisq.test(personal$service, health_life$help)


# '전반적만족도'와 '성별/요양기간/장해등급/의료서비스경험' 사이의 관계

chisq.test(personal$gender, health_life$satisfy)

chisq.test(personal$period, health_life$satisfy)

chisq.test(personal$grade, health_life$satisfy)

chisq.test(personal$service, health_life$satisfy)


## logistic regression - 요인 분석 ##

# '전반적 건강상태'에 영향을 미치는 요인

logistic1_1 <- glm(health ~ explain + evaluation + treatment + gender + period + grade + service, data=data_copy, family = 'binomial')

summary(logistic1_1)


# '일상생활도움'에 영향을 미치는 요인

logistic2_1 <- glm(help ~ explain + evaluation + treatment + gender + period + grade + service, data=data_copy, family = 'binomial')

summary(logistic2_1)


# '전반적만족도'에 영향을 미치는 요인

logistic3_1 <- glm(satisfy ~ explain + evaluation + treatment + gender + period + grade + service, data=data_copy, family = 'binomial')

summary(logistic3_1)

