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
td_dust <- as_tibble(dustdata_low) # tibble 구조로 변환하는 코드
# tibble은 simple dataframe 형태
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

# 이상치 제거 - area 평균이 포함됨
# 이상치 값을 NA로 대체한 후에 해당 행 제거
# ifelse(조건, 참일 때 값, 거짓일 때 값)

class(dustdata_NA$Area)
dustdata_NA$Area <- ifelse(dustdata_NA$Area == '평균', NA, dustdata_NA$Area)
head(dustdata_NA)

# area값이 NA인 행은 제외

dustdata_NA_rm <- dustdata_NA %>% filter(!is.na(Area))

head(dustdata_NA_rm)

# 성북구 중구의 데이터만 추출
# 성북구와 중구의 미세먼지 농도는 차이가 있는가? 평균검정
dustdata_anal <- dustdata_NA_rm %>% filter(Area %in% c("성북구","중구"))

View(dustdata_anal)

# 데이터 현황 파악하기
# 2021년 1월 1일부터 3월 1일가지의 모든 날짜에 '성북구','중구가 다 포함되어 있는지
# count 함수 사용하여 날짜별 개수 출력
count(dustdata_anal,yyyymmdd)
View(count(dustdata_anal,yyyymmdd))

# finedust에 NA값이 있는지 확인
table(is.na(dustdata_anal$Finedust))

# 성북구와 중구의 데이터 분리
# subset() 함수 사용
dust_anal_area_sb <- subset(dustdata_anal, Area=='성북구')
dustdata_anal %>% filter(Area=='성북구')
dust_anal_area_sb

dust_anal_area_jg <- subset(dustdata_anal, Area=='중구')
dustdata_anal %>% filter(Area=='중구')
dust_anal_area_jg

# 두 구의 기초 통계량 도출
# describe() 함수

library(psych)
describe(dust_anal_area_sb$Finedust)
describe(dust_anal_area_jg$Finedust)

# boxplot을 이용한 분포 차이 확인

boxplot(dust_anal_area_sb$Finedust,dust_anal_area_jg$Finedust,
        main="Finedust_compare", xlab="Area", ylab="Finedust PM",
        names=c("성북구","중구"), col=c('blue', 'green'))

# 전반적으로 중구의 미세먼지 농도가 조금 높은 것으로 보임

# 데이터 검정
# 데이터 크기가 30개를 넘어 대표본이므로 정규분포를 따른다고 봄
# 표본의 크기가 30개를 넘어갈 시 z-test 사용

# 가설설정 - 정규분포인지 확인 - 분산 동질성 확인 - t test 검정 - 결론

# 가설설정
# 두 지역 간의 미세먼지 평균은 같은가?
# 귀무가설 : 두 지역 간의 평균은 차이가 없다
# 대립가설 : 성북구의 평균이 더 낮다

# z test
install.packages("BSDA")
library(BSDA)
z.test(dust_anal_area_sb$Finedust,dust_anal_area_jg$Finedust,
       sigma.x = 22.43, sigma.y = 23.46, alternative = 'less') # p-value : 0.06896
      # 두 지역의 평균은 차이가 없다


# t test
var.test(dust_anal_area_sb$Finedust,dust_anal_area_jg$Finedust)
## p-value : 0.7314 -> 두 개의 분산은 같다

t.test(dust_anal_area_sb$Finedust,dust_anal_area_jg$Finedust, 
       var.equal = T, alternative = "less") 
## p-value : 0.07029 -> 두 지역의 미세먼지 농도평균은 같다

View(dustdata_NA_rm) # long 형
table(is.na(dustdata_NA_rm))

# dcast를 이용해서 data를 wide형으로 변환
library(reshape2)
dustdata_wide <- dcast(dustdata_NA_rm, yyyymmdd~Area)
View(dustdata_wide)

# NA값의 개수 확인
table(is.na(dustdata_wide))
summary(dustdata_wide)

## 아래코드를 기준으로 함수 구성
# ifelse(is.na(dustdata_wide$구로구),46.30,dustdata_wide$구로구)
# return(ifelse(is.na(x), mn, x))

rm_na<-function(x,mn){
  return(ifelse(is.na(x), mn, x))
}

smean <- sapply(dustdata_wide[-1],mean,na.rm=T); smean
smean[1]
# dustdata_wide[-1] : yyyymmdd를 뺀 df 

rm_na(dustdata_wide$구로구,smean['구로구'])

class(dustdata_wide$구로구) # 벡터
class(dustdata_wide[2]) #데이터프레임
class(dustdata_wide[[2]]) #벡터

dustdata_wide[2]
dustdata_wide[[2]]
dustdata_wide$구로구
smean[1]

# 사용자 정의 함수 호출
rm_na(dustdata_wide[[2]],smean[1])
smean[1]
dustdata_wide[2]

for (i in 2:length(dustdata_wide)){
  dustdata_wide[i] <- rm_na(dustdata_wide[[i]],smean[i-1])
}

# NA값의 개수 확인
table(is.na(dustdata_wide))

## dustdata_wide를 long 형으로 변환하시오

library(reshape2)
dustdata_long<-melt(dustdata_wide, id.vars='yyyymmdd')

# 시간 경과에 따른 미세먼지 농도 변화 그래프
library(ggplot2)
ggplot(data=dustdata_wide, aes(x=yyyymmdd, y=중구), lax=2) +
  geom_line(group=1)

# 1월에 미세먼지가 가장 높은 날 표시
library(dplyr)
max_dust <- max(dustdata_wide[c(1:31),]$중구); max_dust

max_filter<-filter(dustdata_wide[c(1:31),],중구==max_dust); max_filter
max_day<-max_filter$yyyymmdd; max_day

library(ggplot2)
ggplot(data=dustdata_wide[c(1:31),], aes(x=yyyymmdd, y=중구), lax=2) +
  geom_line(group=1) +
  geom_point() +
  geom_vline(xintercept = max_day, col='red') +
  annotate("text", 
           x=14, # 살짝 옆으로 이동
           y=max_dust,
           label=as.character(max_dust),
           color='red')+ # 118 text 표시
  theme(axis.text.x=element_text(angle=90,hjust=1,size=5)) +
  theme(plot.title=element_text(face='bold', size=20,hjust=0.5))+
  # theme_bw() : 배경을 흰색으로 바꾼 것
  labs(y='중구', x='2021년 1월', 
       title = '2021년 1월 중구 미세먼지',
       caption='2021년 1월 1일부터 31일까지지')

# 각 구별 미세먼지 분포 확인(boxplot)
,fill=variable
View(dustdata_long)
ggplot(dustdata_long, aes(x=variable, y=value,fill=variable)) +
  geom_boxplot() +
  theme(axis.line.x = element_blank() ,
        axis.ticks.x= element_blank(),
        axis.text.x= element_blank()) +
  theme(legend.position = "bottom")

# 다른 방법
ggplot(dustdata_long, aes(x=variable, y=value,fill=variable)) +
  geom_boxplot() +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=13)) +
  theme(legend.position = "none")

# 시계열 그래프
ggplot(dustdata_long, aes(x=yyyymmdd, y=value)) +
  geom_line(aes(group=variable,col=variable)) +
  theme(axis.line.x = element_blank() ,
        axis.ticks.x= element_blank(),
        axis.text.x= element_blank()) +
  theme(legend.position = "bottom")

# 구별 미세먼지 평균 차이를 확인하는 막대그래프
# 구별로 색상을 다르게 작업하고, 평균이 높은 구 별로 정렬하여 출력
# 구별 미세먼지 농도의 최대값 차이, 최소값 차이를 한번에 확인할 수 있는
# 선그래프, 최대값과 최소값의 선 그래프가 한 그래프에 표현되도록 한다
  
gu_mean<-colMeans(dustdata_wide[-1]) # 각 열 평균을 반환
class(gu_mean)
head(gu_mean)

# 벡터형이므로 df로 변환
gu<-names(gu_mean) # 차후에  join key로 사용
gu_mean_df <- data.frame(gu_mean,gu)
head(gu_mean_df)

# 각 구별 미세먼지 최대, 최소값표 선출
gu_max <- apply(dustdata_wide[-1],2,FUN=function(x) max(x)); gu_max
gu_max_df <-data.frame(gu_max,gu)
gu_min <- apply(dustdata_wide[-1],2,FUN=function(x) min(x)); gu_min
gu_min_df <-data.frame(gu_min,gu)

# gu_mean-df : 각 구별 미세먼지 농도 평균값 테이블
# gu_max-df : 각 구별 미세먼지 농도 최대값 테이블
# gu_min-df : 각 구별 미세먼지 농도 최소값 테이블

gu_min_max_df <- left_join(gu_max_df, gu_min_df, by='gu'); gu_min_max_df
gu_total_df <- left_join(gu_min_max_df, gu_mean_df, by='gu')
View(gu_total_df)

# 구별 미세먼지 평균 차이를 확인하는 막대그래프
# 구별로 색상을 다르게 작업하고, 평균이 높은 구 별로 정렬하여 출력
# 구별 미세먼지 농도의 최대값 차이, 최소값 차이를 한번에 확인할 수 있는
# 선그래프, 최대값과 최소값의 선 그래프가 한 그래프에 표현되도록 한다


ggplot(gu_total_df, aes(reorder(x=gu,-gu_mean))) +
  geom_col(aes(y=gu_mean,fill=gu)) +
  geom_line(aes(y=gu_min),group=1,color='blue', size=2) +
  geom_line(aes(y=gu_max),group=1,color='red', size=2) +
  theme(axis.text.x=element_text(angle=90,hjust=1,size=10)) +
  theme(plot.title=element_text(face ='bold', size=20, hjust=0.5)) +
  labs(y='미세먼지 농도', x='자치구', 
       title = '2021년 구별 미세먼지 평균 수치',
       caption = '2021년 1월 1일부터 3월 1일까지')

# 지도 시각화
# 자치구별 미세먼지 평균 데이터를 이용해서 
# 서울시 지도에 평균 데이터를 색상으로 표식

# 지자체 위경도 얻어오기
register_google(key='AIzaSyCOgmf5KZZ19kjxRBLsZUnEpqRfNXrqdTk')
str(gu_mean_df) # chr로 되있어야 google map에서 search가 가능함(factor는 안됨)

gu_code <- geocode(gu_mean_df$gu)
View(gu_code)

code_df<- cbind(gu_mean_df,gu_code)
View(code_df)

# 지도 얻어오기
map <- get_googlemap('용산구',map='roadmap', zoom=11)
ggmap(map) +
  geom_point(data=code_df,
             aes(x=lon, y=lat),
             size=gu_mean/3,
             color=gu_mean,
             alpha=0.5) +
  scale_color_gradient(low="skyblue", high='blue')
  

