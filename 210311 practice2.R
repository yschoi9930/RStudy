rawdata <- read_xlsx("data/dustdata.xlsx")
View(rawdata)

# 성북구와 중구구의 미세먼지 농도 평균의 차이가 있는가?

# 필요 없는 데이터 제거
data_anal <- rawdata %>% filter(area %in% c("성북구","중구"))
View(data_anal)

# 잘 제거 되었는지 확인
c <- count(data_anal, yyyymmdd)
count(c,count == 2) 
# 두개 값이 있는지 한번에 확인하려고 이 방법을 사용해봤습니다!
# 눈으로 확인하는 방법 외에 다른 방법이 있을까요?

# NA 값 확인
table(is.na(data_anal$finedust))

# 성북구와 중구의 데이터 분리
# subset() 함수 사용
data_anal_sb <- subset(data_anal, area=='성북구') ; data_anal_sb
data_anal_jg <- subset(data_anal, area=='중구') ; data_anal_jg

# 데이터 기초 통계량 확인
describe(data_anal_sb$finedust)
describe(data_anal_jg$finedust)

# boxplot을 사용하여 분포 확인
boxplot(data_anal_sb$finedust,data_anal_jg$finedust,
        xlab="area", ylab="Finedust", names=c("성북구","중구"), col=c("red","blue"))

# 전반적으로 성북구의 농도 평균이 높아보임

# 가설검정
# 귀무가설 : 두 지역 평균의 차이가 없다
# 대립가설 : 성북구의 평균이 더 높다
z.test(data_anal_sb$finedust,data_anal_jg$finedust, sigma.x = 20.14, sigma.y = 17.44,
       alternative = "greater")
# p-value : 0.002297이므로 대립가설 채택, 성북구의 평균이 더 높다

## ttest

var.test(data_anal_sb$finedust,data_anal_jg$finedust)
# p : 0.1145 대립가설 채택

t.test(data_anal_sb$finedust,data_anal_jg$finedust, var.equal = T, alternative = "greater")
