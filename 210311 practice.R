# 지하철역 주변 아파트 가격 알압기
# 1. 공공데이터 다운로드
# 지하철역 정보 - 전처리
# 아파트 실 거래가

# 2. 지하철역 데이터 가공하기
# 지하철역 좌표정보 구하기

# 3. 아파트 실 거래가 데이터 가공하기
# 전용면적 별 거래 가격 확인 -> 거래량이 높은 평형을 선택
# 아파트 단지별 평균 거래금액 구하기
# 아파트 주소 정보 전처리 후 좌표 정보 구하기

# 4. 구글지도에 지하철 역과 아파트 가격 표시
# 마포구를 기준으로 표시


# 지하철역 데이터 가공
# 지하철역 정보에서 역명, 주소 추출하고 주소로 위경도 값 추출
library(readxl)
library(dplyr)
library(ggplot2)
library(reshape2)

library
rawdata <- read_xlsx("data/subwaydata.xlsx")
head(rawdata)

# 지하철역 위경도 얻어오기

newdata <- rawdata[,c(3:4,6)]
head(newdata)
str(newdata)
View(newdata)

geodata <- geocode(as.character(newdata$역주소))

finalcode <- cbind(newdata, geodata)
View(finalcode)

line2code <- finalcode %>% filter(호선 %in% c(2))

View(line2code)

write.csv(line2code, 'line2code')
