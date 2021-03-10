#aggregate() apply() sapply() tapply() mapply()

#aggregate()함수
#데이터 프레임을 대상으로 해서 통계 함수 적용시켜 줌
#dplyr 패키지와 유사한 기능을 갖고 있음
#특정열을 기준으로 통계량을 구해주는 함수

df_subway<-read.csv("data/1-4호선승하차승객수.csv")
View(df_subway)

# 해당 데이터를 추출한 날의 노선별 총 승차 인원수와 총 하차 인원수 구하기
# aggregate(계산될 열 ~ 기준될 열, 데이터, 통계함수)
aggregate(승차~노선번호,df_subway,sum)
aggregate(하차~노선번호,df_subway,sum)

aggregate(승차~노선번호,df_subway,mean)
aggregate(하차~노선번호,df_subway,mean)

# 노선변호별 승하차 총 인원수를 구하시오
aggregate(승차+하차~노선번호,df_subway,sum)

# 시간별 승차 총 인원수를 구하시오
aggregate(승차~시간,df_subway,sum)
class(aggregate(하차~시간,df_subway,sum))

#apply()계열 함수 정리
#특정 기준 방향으로 특정 함수를 적용해서 해당 결과를 얻는 함수
#dataset 행이나 열 전체에 특정 함수를 적용할 때 유용하다.

# apply() 계열 함수 정리
# apply(대상, 행/열,적용함수) : 행렬을 대상
#apply(데이터, 행/열, 적용함수)
#행/열 -> 1/2
# apply(데이터, 1, 적용함수) : 각 행에 대해 함수 적용
# apply(데이터, 2, 적용함수) : 각 열에 대해 함수 적용
data1 <- read.csv("data/data1.csv")
View(data1)

# 연도별 합계를 구하시오
# 연도는 열방향으로 수집되어 있음
ls(data1)
# 연령별 필드는 계산에서 제외
head(data1)
class(data1[-1])
apply(data1[-1],2,sum)
# 2008년부터 2012년까지 연도별 합계를 각각 구하시오.
data1[10:14]
apply(data1[10:14],2,sum)
# 연령별 합계를 모두 구하시오.
apply(data1[-1],1,sum)

#-----------------------------
data2<-read.csv("data/1-4호선승하차승객수.csv")
View(data2)
attach(data2)

# lapply(대상,적용함수)-리스트반환
# sapply(대상,적용함수)-벡터반환 :벡터/리스트를 대상
# 노선 번호에 상관없이 총 승차 인원수 및 총 하차 인원수를 구하시오.
lapply(data2[3:4],sum)
sapply(data2[3:4],sum)

#--------------------------------------
# tapply(출력값, 기준열, 적용함수) : 데이터프레임 대상
# 그룹화 해서 함수를 적용할 경우 사용
# 노선 번호별 총 승차 인원수를 구하시오.
class(tapply(승차,노선번호,sum)) # array
# 노선 번호별 승하차인원수의 최소값을 구하시오.
tapply(승차+하차,노선번호,min)
#-------------------------------------
# mapply(함수, 벡터1, 벡터2...)
# 여러개 인자를 전달해서 함수 적용한 결과를 반환
# 각 벡터들의 데이터의 길이는 동일하거나 배수 관계에 있어야 한다
vec1<-c(1,2,3,4,5,6,7,8,9,10)
vec2<-c(10,20,30,40,50)
vec3<-c(100,200,300,400,500)
mapply(sum,vec1,vec2,vec3)

#------------------------------------------------------------
# reshape 예제
# 
View(data2) #long 형
# 노선번호를 기준으로 각 시간대별 승차인원을 표현하는 df로 생성
library(reshape2)
# 승차인원  dcast
d<-dcast(data2[,c(1:3)],노선번호~시간)
View(d)
# 하차인원  dcast
d2<-dcast(data2[,c(1,2,4)],노선번호~시간)
View(d2)

# 시간대 기준으로 각 노선번호별 승차인원을 표현하는 df로 생성
library(reshape2)
# 승차인원  dcast
d<-dcast(data2[,c(1:3)],시간~노선번호)
View(d)
# 하차인원  dcast
d2<-dcast(data2[,c(1,2,4)],시간~노선번호)
View(d2)















