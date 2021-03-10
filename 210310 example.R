# 서울시 지역별 미세먼지 농도 현황 분포를 비교한 후 
# 검정방법을 통해 지역별로 차이가 나는지 확인(성북구, 중구)
# 상자 그림으로 시각화

# 활용데이터 : 서울시 대기환경정보 웹사이트

library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

data <- read.xlsx()
getwd()

# 데이터 가져오기
# 1-3행은 스킵하고 가져오기
# dustdata_low : 원본 data

dustdata_low <- read_excel("data/period.xlsx", skip =3)
View(dustdata_low)
head(dustdata_low)

# 복사본 생성
dustdata_NA <- dustdata_low

# 데이터 전처리 : 이상치, 결측치 제거
# 전체 평균행과 각 일자별 평균행 - 제거
# 필요한 데이터 열만 추출

dustdata_NA <- dustdata_NA[,-c(4:8)]
View(dustdata_NA)
colnames(dustdata_NA) <-c('yyyymmdd','Area','Finedust')

str(dustdata_NA)
