# 단계 구분도(Choropleth Map)
# 지역별 통계치를 색상의 차이로 표현한 지도
# 단계구분도를 보면 인구나 소득같은 특성이
# 지역별로 얼마나 다른지 쉽게 이해할 수 있음

# 단계 구분도 패키지
# 분포를 나타낼 data
# 지도 data

install.packages('ggiraphExtra')
library(ggiraphExtra)

# 1. 미국 주별 강력 범죄율을 단계 구분도로 표시

# 사용데이터 (범죄율 데이터)
head(USArrests)

# 지역명 변수가 따로 없고 행이름이 지역명으로 되어 있음
# 행 이름을 변수로 변환시켜 사용해야 함 : tibble rownames_to_column()

library(tibble)
crime <- rownames_to_column(USArrests, var='state')
head(crime)

# 지도 데이터 준비
# 단계구분도를 만들려면 각 구분할 지역의 경계선을 표시하는 위경도 정보가 있어야함
# R에 내장된 maps 패키지에 주별 위겨도 정보 데이터 state가 있음
# ggplot2패키지의 map_data() 함수를 이용해서 데이터 프레임으로 불러옴

install.packages('maps')
library(ggplot2)
library(maps)
state_map <- map_data('state')
str(state_map)
View(state_map)
View(crime)

# 지도 데이터의 지역 정보가 모두 소문자이므로 범죄데이터의 지역(state)정보를 소문자로 변환

crime$state <-tolower(crime$state)

# 단계구분도 만들기
# ggiraphExtra 패키지 ggChoropleth() 함수 사용

install.packages('mapproj')
library(mapproj)
ggChoropleth(data=crime, # 지도에 표현될 데이터
             aes(fill=Murder, # 색상으로 표현할 변수
                 map_id=state, # 지도와 매핑될 기준 변수
                 ), map=state_map) +
  ggtitle("주별 범죄 데이터") +
  theme(plot.title = element_text(face='bold', size=20, hjust=0.5), legend.position = 'bottom')

# interactive 인수 사용

install.packages('mapproj')
library(mapproj)
ggChoropleth(data=crime, # 지도에 표현될 데이터
             aes(fill=Murder, # 색상으로 표현할 변수
                 map_id=state, # 지도와 매핑될 기준 변수
             ), map=state_map, interactive = T) # interactive 적용시 다른 레이어 적용 안됨

# 색상변경 : 기본 파레트 -> palette = "OrRd'
ggChoropleth(data=crime, # 지도에 표현될 데이터
             aes(fill=Murder, # 색상으로 표현할 변수
                 map_id=state, # 지도와 매핑될 기준 변수
             ), map=state_map, interactive = T, palette='Blues')

# 표현변수 2개 사용
ggChoropleth(data=crime, # 지도에 표현될 데이터
             aes(fill=c(Murder, Rape), # 색상으로 표현할 변수
                 map_id=state, # 지도와 매핑될 기준 변수
             ), map=state_map, interactive = T, palette='Blues')

# 동적지도를 생성하고 제목을 추가하는 코드
F <-ggChoropleth(data=crime, # 지도에 표현될 데이터
             aes(fill=Murder, # 색상으로 표현할 변수
                 map_id=state, # 지도와 매핑될 기준 변수
             ), map=state_map, interactive = F) 

F <- F + ggtitle('주별 범죄 데이터') +
  theme(plot.title = element_text(face='bold', size=20, hjust=0.5))

library(ggiraph)
ggiraph(ggobj=F, zoom_max=10)

# 2. 대한민국 시도별 인구, 결핵환자 수 단계 구분도 만들기

# 대한민국 인구통계와 지도데이터를 포함하고 있는 패키지 사용 : kormaps2014 패키지

install.packages('devtools')

devtools::install_github("cardiomoon/kormaps2014")
library(kormaps2014)

# kormap1 : 2014년 한국 행정 지도(시도별)
# korpop1 : 시도 인구
# kormap2 : 2014년 한국 행정 지도(시군구별)
# korpop2 : 시군구 인구
# kormap3 : 2014년 한국 행정 지도(동읍면별)
# korpop3 : 읍면동 인구

# 변수 속성 확인
head(kormap1)
str(kormap1)
str(changeCode(korpop1))

km1 <- kormap1
kp1 <- korpop1

View(km1)
View(kp1)

# 변수명이 한글인 경우 오류가 발생할 수 있음
# 변수명 - 영문자로 변경(사용할 변수만)

library(dplyr)
kp1 <- rename(kp1, 
              pop=총인구_명,
              name = 행정구역별_읍면동)

str(kp1) # 코딩 방식에 따라 오류가 남, 엔코딩을 진행해줘야함
str(changeCode(kp1))

# kp1의 name 한글 깨짐 해결 iconv(data, from, to)
kp1$name <- iconv(kp1$name, 'UTF-8','CP949')

# -----------------------------------------------------
# 단계 구분도 작성
ggChoropleth(data=kp1,
             aes(fill=pop,
                 map_id=code,
                 tooletip=name),
             map=km1, interactive = T)

ggChoropleth(data=kp1,
             aesmap=km1)

# kormaps2014 패키지에 지역별 결핵 환자 수에 대한 정보가 들어있는 tbc데이터가
# 포함되어 있음
# tbc 데이터의 NewPts(결핵 환자 수) 변수를 이용해
# 시도별 결핵환자 수 단계 구분도 작성

data(tbc)
View(tbc)
str(tbc)
str(changeCode(tbc))
tbc$name1 <- iconv(tbc$name1, 'UTF-8','CP949')
ggChoropleth(data=tbc,
             aes(fill=NewPts,
             map_id=code,
             tooltip=name1),
             map=km1, palette = 'Blues', interactive= T)

NewPts <- NewPts + ggtitle("전국 결핵 환자 인구 수") + 
                   theme(plot.title = element_text(face='bold',size=20, hjust=0.5))
NewPts

ggChoropleth(data=korpop2,
             aes(fill=남자_명,
                 map_id=code,
                 tooltip=name1),
             map=kormap2, palette = 'Blues', interactive= T)

ggChoropleth(data=korpop3,
             aes(fill=여자_명,
                 map_id=code,
                 tooltip=name1),
             map=kormap3, palette = 'Reds', interactive= T)

# -----------------------------------------------------------
# 서울 자치구별 점포수 분포를 나타내는 단계구분도 작성

seoul <- read_excel('data/서울_자치구별_점포수.xlsx')
seoul_map <- read_excel('data/서울_map.xlsx')
View(seoul)
View(seoul_map)

seoul <- rename(seoul, name=행정구역별_읍면동)

ggChoropleth(data=seoul,
             aes(fill=점포수,
                 map_id=code,
                 tooltip=name),
             map=seoul_map, interactive= T, palette='Accent')

# ggplot으로 그려보기
# 인구분포도 ggplot으로 시각화하기
# 인구데이터 : kp1
ggplot(kp1, aes(map_id=code, fill=pop)) +
  geom_map(map=km1, colour='black', size=0.1) + # 지도 사이 줄선 
  expand_limits(x=km1$long, y=km1$lat) +
  ggtitle("2015년 시도별 인구 분포도도") + 
  theme(plot.title = element_text(size=20, hjust=0.5)) +
  scale_fill_gradientn(colours=c('white','orange','red')) +
  coord_map() # 일반 그래프가 아닌 지도 형태로 코디네이션 해줌

# 서울시 점포 단계구분도를 ggplot을 이용해서 그려보기
ggplot(seoul, aes(map_id=code, fill=점포수)) +
  geom_map(map=seoul_map, colour='black', size=0.1) +
  expand_limits(x=seoul_map$long,y=seoul_map$lat) +
  coord_map() +
  scale_fill_gradientn(colours=c('white','orange','red'))


