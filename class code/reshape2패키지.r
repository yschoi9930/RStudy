# reshape2 패키지
# install.packages("reshape2")
library(reshape2)

# melt(
#   data,
#   id.vars, #기준열 - 생략하면 모든열을 행방향으로 변환
#   measure.vars, # 변환열 - 생략하면 기준열을 제외한 모든 열 행방향으로 변환
#   variable.name = "variable", # 저장할 값과 변수명
#   ...,
#   na.rm = FALSE, # 결측치를 포함할 것인지 아닌지
#   value.name = "value",
#   factorsAsStrings = TRUE
# )

# vector 생성
name <- c("민철","지수","지영")
kor <- c(100,70,50)
eng <- c(80,70,100)
computer <- c(85,100,80)

eval_df <- data.frame(name,kor,eng,computer)
eval_df

# melt() 함수 사용하여 데이터 구조 변경
# wide -> long
melt(eval_df,
     id.var="name",
     measure.vars = c("kor","computer"))


# 열 이름도 같이 변경
melt(eval_df,
     id.var="name",
     measure.vars = c("kor","computer"),
     variable.name = 'subject',
     value.name = 'score')

#------------------------------------------------------------------
# airquality 데이터 세트 (R 기본 내장 데이터 셋)
# 1973년 5월 부터 9월 까지의 뉴욕 대기 오염 정도에 대한 기록

# R 내장 데이터 세트 확인
data()

# airquality 데이터 세트 사용
# 01. 데이터 세트 구조 파악하기
# 02. 변수명을 소문자로 변경
# 03. melt() 함수 실행
# 04. tail() 함수 이용하여 데이터 세트 뒷부분 확인
# 05. '월'과 '바람'에 따른 오존 확인하기
# 각 월에 오존 발생량에 바람이 영향이 있는가?
#   바람이 많으면 오존이 줄어드는지 확인

# 01. 데이터 세트 구조 파악
head(airquality)
View(airquality)
# 변수명을 소문자로 변경
# names()
names(airquality) <- tolower(names(airquality))
head(airquality)

# melt()  함수 적용
melt_test <- melt(airquality)
# No id variables; using all as measure variables

head(melt_test)
# View(melt_test)
tail(melt_test)

# '월'과 '바람'에 따른 오존 량 확인하기
# 기준열 : month, wind
# 변환열 : ozone

melt_test2 <- melt(airquality,
                   id.vars = c("month","wind"),
                   measure.vars = "ozone")

head(melt_test2)
View(melt_test2)

melt_test3 <- melt(airquality,
                   id.vars = c("month","wind"),
                   measure.vars = "ozone",
                   na.rm = TRUE)

head(melt_test3)
View(melt_test3)

#------------------------------------------------
#dcast() 함수 사용하기
#dcast(데이터세트, 기준열~변환열)

#melt()된 데이터를 dcast() 수행하는 것이 원칙
#월과 일을 기준으로 나머지 열을 변환해서 long형 데이터를 생성하시오

aq_melt<-melt(airquality,
              id.vars = c("month","day"),
              na.rm=FALSE)
View(aq_melt)

# melt()된 데이터를 dcast로 변환
ad_cast <- dcast(aq_melt,month+day~variable)
ad_cast

#-------------------------------------------------------------------
#원칙 : 
#기존 데이터 셋 -> 모든데이터 melt() -> cast로 원 데이터 셋 복원 가능
  
#기존 데이터 셋 -> 일부데이터 이용 melt() -> cast로 원데이터 셋 복원이 불가능 할 수도 있음
#이유 : 기준 열을 설정할 수 없으면(데이터의 조합이 유니크하지 않으면 복원 불가능)

# cast() 함수 사용시 기준열의 조합은 유니크해야 한다.
  
head(melt_test2)
cast_test <- dcast(melt_test2,month+wind~variable)

#----------------------------------
# acast() : 행렬, 배열로 반환

# 5-9월, 1-31일까지의 
# 오존, 태양복사, 바람, 온도의 측정값 출력
# 결과로 배열로 출력
acast_air <- acast(aq_melt, day~variable~month)
class(acast_air)

## 함수 인수 옵션 생략 시 
acast(aq_melt, day~variable) #에러 발생 : 기준열인  day에 중복된 값이 들어 있음
# Aggregation function missing: defaulting to length

acast(aq_melt, month+day~variable)
#       ozone solar.r wind temp
# 5_1     41     190  7.4   67
# 5_2     36     118  8.0   72
# 5_3     12     149 12.6   74
# 5_4     18     313 11.5   62

# 요약 함수 사용 가능

# 각 날짜별 관측요소의 평균을 배열형태로 반환
acast(aq_melt, day~variable,mean)
# 데이터에 NA값이 있으면 연산 불가능


aq_melt<-melt(airquality,
              id.vars = c("month","day"),
              na.rm=TRUE)

acast(aq_melt, day~variable,mean)

# 모든 관측치의 월별 평균데이터를 와이드 형태로 반환

acast(aq_melt, month~variable,mean)

# dcast() 함수에서의 요약
melt_test3
dcast(melt_test3, month~variable,mean)


### 연습문제 1
# baseball.csv

baseball_long <- read.csv('data/baseball.csv')
head(baseball_long)

#month 를 기준으로 pytpe열에 있는 모든 변수명을 열로 변환
baseball_wide <- dcast(baseball_long, month~pytpe)
baseball_wide


### 연습문제 2
emp_wide <- read.csv('data/2000-2013년 연령별실업율_연령별평균.csv')
View(emp_wide)

# 기준열 : 연령별로 지정
# variable.name ='년도'
# value.name='실업율'

emp_melt <- melt(emp_wide,
                 id.vars = '연령별',
                 variable.name = '년도',
                 value.name='실업율')

View(emp_melt)

emp_dcast <- dcast(emp_melt, 년도 ~ 연령별)
emp_dcast

# 연습문제
# 사용할 데이터 : 2000-2013년 연령별실업율_60세이상.csv 파일
over60_wide<-read.csv("data/2000-2013년 연령별실업율_60세이상.csv")
View(over60_wide)

# 평균행 제외
over60_wide2 <-over60_wide[-13,]
over60_wide2

# 평균행 제외
over60_wide2 <-over60_wide[-(nrow(over60_wide)),]

str(over60_wide)
# 월 변수의 형태를 숫자로 변환

over60_wide2$월 <- as.numeric(over60_wide2$월)
class(over60_wide2$월)

over60_melt <- melt(over60_wide2,
                  id.vars='월',
                  variable.name = '연도',
                  value.name='실업율')
View(over60_melt)

# 원 데이터로 복원
over_60_dcast <- dcast(over60_melt,월~연도)
over_60_dcast



# (3) 원하는 형태로 변환
#     연도를 기준으로 
#     연도 1 2 3 4 5 6 7 8 9 10 11 12
#     의 형태로 변경
over_60_dcast <- dcast(over60_melt,연도~월)
over_60_dcast




