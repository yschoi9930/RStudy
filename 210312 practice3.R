# 지하철역 주변 아파트 가격 알아보기
# 1.공공데이터 다운로드
# 지하철역 정보 - 전처리
# 아파트 실 거래가

# 2.지하철역 데이터 가공하기
# 지하철역 좌표정보 구하기

# 3. 아파트 실 거래가 데이터 가공하기
# 전용면적 별 거래 가격 확인 - 거래가 높은 평형을 선택
# 아파트 단지별 평균 거래금액 구하기
# 주소정보 전처리 후 좌표정보 구하기

# 4. 구글지도에 지하철역과 아파트 가격 표시
# 마포구를 기준으로

# 지하철역 데이터 가공
# 지하철역 정보에서 역명, 주소 추출하고 주소로 위경도 값 추출

library(dplyr)
library(ggmap)

#원시데이터 가져오기
station_data <- read.csv('data/역별_주소_및_전화번호.csv')

# 변수 속성 확인
str(station_data)

# 지하철역 위경도 얻어오기
# goolge api 키 등록
register_google(key='')
# station_data 구 주소 열값을 이용해 위경도 추출
station_code <-geocode(as.character(station_data$구주소))

# 위경도, station_data 데이터프레임 합치기
station_code_final <- cbind(station_data,station_code)
head(station_code_final)

# station_code_final 저장하기 : line_2_lat_lng.csv
# 서울지도에 지하철 2호선 역을 표시하기
# http://rt.molit.go.kr
library(readxl)
# low 파일 읽어오기(불필요한 행 제거하기)
apart_data_low <- read.csv('data/apart2019.csv',skip=15)
head(apart_data_low)
str(apart_data_low)
View(apart_data_low)

# 필요한 열 선택
# 변수명 변경
# 데이터 타입 확인 후 사용가능 한 형태로 변환
# 데이터  내용이 코드로 되어 있는 경우 - 성별 남자 1, 여자 2
# 나이데이터 - 코드화되어 있으면 연령대이므로 변경해서 사용해야 함
# 이상치, 결측치 제거나 대체

# 데이터의 범위를 표준화

#--------------------
# 1. 필요변수 선택(시군구,번지,단지명,전용면적,거래금액)
apart_data <-apart_data_low[,c(1,2,5,6,9)]
head(apart_data)

# 2.변수명 변경
names(apart_data) <-c('시군구','번지','단지명','전용면적','거래금액')
head(apart_data)

nrow(apart_data)

# 대표 평형 데이터 추출하는 작업 : 대표평형 (거래량이 높은 평형을 선택)
# 거래량 데이터 문자->숫자
apart_data$전용면적 = as.numeric(apart_data$전용면적)
str(apart_data$전용면적)

# 전영면적의 값을 반올림하여 정수로 표현
apart_data$전용면적=round(apart_data$전용면적)
head(apart_data) 

# 거래 빈도가 높은 평형 확인
# 전용면적을 기준으로 빈도를 구함 - 빈도에 따라 내림차순 정렬해서 결과 확인
count(apart_data,전용면적) %>% 
  arrange(desc(n))
# 전용면적 85의 거래량이 가장 많음 - 대표면적으로 사용

# 전용면적 85인 데이터만 추출
apart_data_85 <- subset(apart_data,전용면적 == '85')
head(apart_data_85)

# 지도에 표식할 마포구 데이터만 추출
library(stringr)
apart_data_85 <- apart_data_85 %>% 
                 filter(str_detect(시군구,'마포구'))

head(apart_data_85)
nrow(apart_data_85)

# 거래금액 , 제거후 수치로 변환
apart_data_85$거래금액 <- gsub(',','',apart_data_85$거래금액)
head(apart_data_85)

# 거래금액을 정수형으로 변환하여 단지별 평균 구하고 새 변수에 할당
apart_data_85_cost <- aggregate(as.integer(거래금액)~단지명,apart_data_85,mean)
# View(apart_data_85_cost)  
nrow(apart_data_85_cost)

# 열이름 변경 as.integer(거래금액) -> 거래금액으로 변경
apart_data_85_cost<-rename(apart_data_85_cost,"거래금액"="as.integer(거래금액)")
head(apart_data_85_cost)

# apart_data_85 : 마포구 아파트 기본 정보 (단지명, 실거래금액...,주소) 단지명이 중복
# apart_data_85_cost : 단지명, 평균거래금액 단지명이 유니크

nrow(apart_data_85_cost)
nrow(apart_data_85)

View(apart_data_85)

# 단지 정보 추출 - apart_data_85서 중복된 단지명 행 제외시키고 하나만 추출함
# duplicated 함수 사용
apart_data_85 <-apart_data_85[!duplicated(apart_data_85$단지명),]
nrow(apart_data_85)

View(apart_data_85)

# 정보데이터셋(apart_data_85)과 평균표(apart_data_85_cost)를 병합(join)
apart_data_85 <-left_join(apart_data_85,apart_data_85_cost,by='단지명')
head(apart_data_85)
#거래금액.y가 평균 - 사용
apart_data_85 <- apart_data_85 %>% select('단지명','시군구','번지','전용면적','거래금액.y')
head(apart_data_85)
library(reshape)
apart_data_85<-rename(apart_data_85,거래금액=거래금액.y)
head(apart_data_85)

#------------------------데이터 가공 완료

# 지도 시각화 - 아파트 단지 위경도 수집
# 실습 : 단지 위경도 수집 

apart_address <- paste(apart_data_85$시군구, apart_data_85$번지) %>% data.frame()
head(apart_address) # 열 제목이 .으로 만들어짐

#.을 주소로 변경하여 저장
apart_address <- rename(apart_address,"주소"=".")
head(apart_address)

# 위경도 수집
apart_address_code <- as.character(apart_address$주소) %>% enc2utf8() %>% geocode()
head(apart_address_code)

# write.csv(apart_address_code, '마포아파트위경도.csv')
# 가공 데이터 및 수집 데이터 병합
mapapt_code_final <- cbind(apart_data_85,apart_address,apart_address_code)%>%
  select('단지명','전용면적','거래금액','주소',lon,lat)
head(mapapt_code_final)

write.csv(apart_address_code,'마포아파트2019거래금액_위경도.csv')

line2 <- read.csv('line2code.csv')
head(line2)

# 지도표식
ggseoul <- get_googlemap('마포구',zoom=13,maptype = 'roadmap')
ggmap(ggseoul)

# 마포를 중심으로 지하철 역 표시
ggmap(ggseoul) + geom_point(data =finalcode, aes(x=lon, y=lat), 
                            col='red',size=5)

library(formattable)

# 홍대입구역을 중심으로 지하철 역과 아파트 위치, 거래금액을 표식

get_googlemap('홍대입구역',zoom=14, maptype='roadmap') %>% ggmap() +
  geom_point(data=mapapt_code_final,
             aes(x=lon,y=lat), size=20, alpha=0.3, col = 'blue') +
  annotate("text",
           x=mapapt_code_final$lon, y=mapapt_code_final$lat,
           label=comma((round(mapapt_code_final$거래금액)),format='d'), face='bold') +
labs(title = '홍대입구역 주변 아파트 거래금액',
     caption='2019년 기준, 단위 : 억 원') +
theme(plot.title=element_text(face='bold', size=20,hjust=0.5))


seoul_map <- get_googlemap('마포',maptype='roadmap',zoom=12)
ggmap(seoul_map) + geom_point(data=line2,aes(x=lon,y=lat,colour=호선), size=3) +
  geom_text(data=line2, aes(label=역명, vjust=-1)) # vjust -> label 위치 조정

hongik <- get_googlemap('홍대입구역', maptype='roadmap', zoom=14)
ggmap(hongik) + 
geom_point(data=line2,aes(x=lon,y=lat,colour=호선),col='red', size=3) +
geom_point(data=mapapt_code_final,aes(x=lon,y=lat), col='blue', size=3) +
geom_text(data=mapapt_code_final, aes(label=단지명), vjust=-2, size=2) +
geom_text(data=mapapt_code_final, aes(label=거래금액), vjust=-1, size=2) 


View(mapapt_code_final)

apt_final_h <- head(arrange(mapapt_code_final, desc(거래금액)),5)
apt_final_l <- tail(arrange(mapapt_code_final, desc(거래금액)),5)
mapo <- get_googlemap("서강대역", maptype='roadmap',zoom=13)
ggmap(mapo) +
  geom_point(data=line2,aes(x=lon,y=lat,colour=호선),col='red', size=3) +
  geom_point(data=mapapt_code_final,aes(x=lon,y=lat,colour=거래금액), size=3, alpha=0.5) +
  scale_color_gradient(low= 'green', high = 'red') +
  geom_text(data=apt_final_h, aes(label=단지명), size=4, vjust=1) +
  geom_text(data=apt_final_h, aes(label=거래금액), size=4, vjust=-1) +
  geom_text(data=apt_final_r, aes(label=단지명), size=4, vjust=1) +
  geom_text(data=apt_final_r, aes(label=거래금액), size=4, vjust=-1)
  
