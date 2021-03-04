
data(iris)
# 산점도 확인
plot(Petal.Length, Sepal.Length, data=iris)

# 회귀선 작성
abline(lm(Petal.Length ~ Sepal.Length, data=iris))

# 여러 변수간 상관도, 상관계수 확인
plot(iris[,1:4])
cor(iris[,1:4])

install.packages("psych")
library(psych)
pairs.panels(iris)

corr.test(iris[,1:4])

library(car)
head(cars)


# 실습예제
# 기본 통계값과 산점도
describe(cars)
summary(cars)
plot(cars)

# 산점도에 선형회귀식 추가
abline(lm(dist ~ speed,data=cars))

# 공분산과 상관계수 값
cov(cars)
cor(cars)

# 유의도 검정 및 결과
corr.test(cars)
## p<0.05 귀무가설 기각, 상관도가 유의하다

pairs.panels(cars)
