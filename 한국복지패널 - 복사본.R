install.packages("foreign")
library(foreign)
library(aplyr)
install.packages("ggplot2")

library(ggplot2)

korea <- read.spss(file = "C:/Users/YANG/Desktop/공모전준비/한국복지패널조사/조사데이터/15년 데이터(beta4)_spss/Koweps_hpc10_2015_beta4.sav",
to.data.frame = T)

korea_copy <- read.spss(file = "C:/Users/YANG/Desktop/공모전준비/한국복지패널조사/조사데이터/15년 데이터(beta4)_spss/Koweps_hpc10_2015_beta4.sav",
                           to.data.frame = T)

# data 기본특성파악
head(korea_copy)
dim(korea_copy)

# 암호화 되어있는 변수 이름 재설정
korea_copy <- rename(korea_copy, 
                     sex = h10_g3,
                     birth = h10_g4,
                     marrige = h10_g10,
                     religion = h10_g11,
                     income = p1002_8aq1,
                     code_job = h10_eco9,
                     code_region = h10_reg7)

#### 성별에 따른 월급 비교 ####

# 1은 male, 2는 female
korea_copy$sex <- ifelse(korea_copy$sex == 1, "male", "female")
table(korea_copy$sex)

# 월급(income)변수 살펴보기
summary(korea_copy$income)

# 0은 결측치로 처리 / 결측치 제거
korea_copy$income <- ifelse(korea_copy$income == 0, NA, korea_copy$income)

sex_income <- korea_copy %>% filter(!is.na(income)) %>% group_by(sex) %>% summarise(mean_income = mean(income))

ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()



#### 나이에 따른 월급 비교 ####

korea_copy$age <- 2015 - korea_copy$birth + 1

age_income <- korea_copy %>% filter(!is.na(income)) %>% group_by(age) %>%
  summarise(mean_income = mean(income))

ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()



#### 연령대에 따른 월급 비교 ####

# 연령대(ageg) 변수 생성
korea_copy <- korea_copy %>% mutate(ageg = ifelse(age < 30, "초년", ifelse(age <= 59, "중년","노년")))
table(korea_copy$ageg)

ageg_income <- korea_copy %>% filter(!is.na(income)) %>% group_by(ageg) %>%
  summarise(mean_income = mean(income))

ggplot(data = ageg_income, aes(x = ageg, y = mean_income)) + geom_col() + scale_x_discrete(limits = c("초년","중년","노년"))


#### 연령대에 따른 성별 월급 차이 (세 변수간 관계) ####

sex_income_ageg <- korea_copy %>% filter(!is.na(income)) %>% group_by(ageg,sex) %>%
  summarise(mean_income = mean(income))

ggplot(data = sex_income_ageg, aes(x = ageg, y = mean_income, fill = sex)) + geom_col(position = "dodge") + scale_x_discrete(limits = c("초년","중년","노년"))


#### 나이에 따른 성별 월급 차이 (세 변수간 관계) ####

sex_income_age <- korea_copy %>% filter(!is.na(income)) %>% group_by(age,sex) %>%
  summarise(mean_income = mean(income))

ggplot(data = sex_income_age, aes(x = age, y = mean_income, col = sex)) + geom_line()



#### 직업에 따른 월급 비교 ####

# '직업분류코드' 파일을 통해 직업 변수에 명칭 부여

install.packages("readxl")
library(readxl)

job_list <- read_excel("C:/Users/YANG/Desktop/공모전준비/한국복지패널조사/Koweps_Codebook.xlsx",
                       col_names = T, sheet = 2)

korea_copy <- left_join(korea_copy, job_list, id = "code_job")

# 직업에 따른 월급 비교

job_income <- korea_copy %>% filter(!is.na(income) & !is.na(job)) %>%
  group_by(job) %>% summarise(mean_income = mean(income))

# 수입이 가장 높은 상위10개 직업

job_income_top10 <- job_income %>% arrange(desc(mean_income)) %>% head(10)
job_income_top10

# 수입이 가장 낮은 하위10개 직업

job_income_bot10 <- job_income %>% arrange(mean_income) %>% head(10)
job_income_bot10


# 그래프로 시각화

top10 <- ggplot(data = job_income_top10, aes(x = reorder(job, mean_income), y = mean_income)) + geom_col() + coord_flip()
bot10 <- ggplot(data = job_income_bot10, aes(x = reorder(job, -mean_income), y = mean_income)) + geom_col() + coord_flip()



#### 종교 유무에 따른 이혼율 비교 ####

# 1은 종교 있음, 2는 종교 없음

korea_copy$religion <- ifelse(korea_copy$religion == 1, "yes", "no")

# 혼인상태 코드표를 통해 명칭 부여 / 1,3 제외한 변수는 결측치 처리

korea_copy$doyoumarrige <- ifelse(korea_copy$marrige == 1, "marriage",
                                  ifelse(korea_copy$marrige == 3, "divorce", NA))

# 종교 유무에 따른 이혼율 비교 

religion_marriage <- korea_copy %>% filter(!is.na(doyoumarrige)) %>% 
  group_by(religion, doyoumarrige) %>% summarise(n = n()) %>% mutate(sum_n = sum(n)) %>%
  mutate(ratio = round(n/sum_n*100, 1))

# 이혼율 추출

religion_divorce_rate <- religion_marriage %>% filter(doyoumarrige == "divorce") %>%
  select(religion, ratio)


ggplot(data = religion_divorce_rate, aes(x = religion, y = ratio)) + geom_col()



