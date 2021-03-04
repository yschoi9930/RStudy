# 모델간 비교
anova(carFit)
(full <- lm(dist ~ speed, data=cars))
(reduce <- lm(dist ~ 1, data=cars))
anova(reduce, full)

## 회귀모형 가정에 대한 검정

# 1. 오차항의 정규성 검정
shapiro.test(residuals(carFit))

# 2. 등분산성, 정규성, 이상치인지 확인

par(mfrow=c(2,2))
plot(carFit)
plot(carFit, which=1)
plot(carFit, which=2)
plot(carFit, which=3)
plot(carFit, which=4)
plot(carFit, which=5)
plot(carFit, which=6)

par(mfrow=c(1,1))
plot(cars$speed, cars$dist)
abline(coef(carFit))

# 예측값의 신뢰구간 그리기

# 예측하기 위한 X

newd <-data.frame(speed=seq(min(cars$speed), 
                            max(cars$speed), .2))

# 예측값과 신뢰구간
ys <- predict(carFit, newdata=newd, interval="confidence")

# 시각화
matplot(newd, ys, type='n')
matlines(newd, ys, lty=c(1,2,3), col=1)




# 실습문제 1
exc <- c(1095,1110,1086,1074,1098,1105,1163,1124,1088,1064)
exp <- c(49,52,48,49,50,51,50,51,49,48)
data <- data.frame(exc, exp) ; data

## 1) 데이터 구조와 특징 파악
summary(data)
str(data)
describe(data)
plot(data)

## 2) 회귀모형 가정에 대한 평가 - 상관관계 체크
cor(data$exc, data$exp) # 0.58
psych::pairs.panels(data)


## 3) 회귀식(모형) 추정

fit <- lm(exp ~ exc, data =data)
fit
plot(data)
abline(coef(fit))

## 4) 회귀모형에 대한 평가 

summary(fit)
anova(fit)
(full <- lm(exp ~ exc, data=data))
(reduce <- lm(exp ~ 1, data=data))
anova(reduce, full)

# 데이터 수정
data <- data[-7,]
cor.test(data$exc, data$exp)
psych::pairs.panels(data) # 1,4번 그래프가 정규분포 모양(대칭)일 수록 좋은것

## 4.1) 오차항 정규성 검정
shapiro.test(resid(fit)) # p>0.05 정규성을 따른다

## 4.2) 등분산성, 정규성, 이상치 확인
par(mfrow=c(2,2))
plot(fit, which=1) # 추정값과 잔차에 대한 좌표값, 관계가 있으면 안됨(변수의 선형성)
plot(fit, which=2) # 자료 값의 정규분포와의 비교를 통해 정규성 검정
plot(fit, which=3) # 표준화된 잔차, 추정값,관계가 있으면 안됨(등분산성)
plot(fit, which=4) 
plot(fit, which=5) # 이상치 확인 
plot(fit, which=6)

fitted(fit)

## 5) 회귀식을 통한 예측 : 환율이 1100인 경우 수출액은?

predict(fit, newdata=data.frame(exc=1100)) ### 결과 값 : 49.68




# 실습문제 2
year <- c(26,16,20,7,22,15,29,28,17,3,1,16,19,13,27,4,30,8,3,12)
sal <- c(1267,887,1022,511,1193,795,1713,1477,991,455,324,
         944,1232,808,1296,486,1516,565,299,830)
data_2 <- data.frame(year, sal) ; data_2

## 1) 데이터 구조와 특징 파악
describe(data_2)
summary(data_2)

## 2) 회귀모형 가정에 대한 평가 - 상관계수 확인
cor(data_2$year, data_2$sal) # 0.97
psych::pairs.panels(data_2)
cor.test(data_2$year, data_2$sal) # 상관있다

plot(data_2)


## 3) 회귀식(모형) 추정

fit_2 <- lm(sal ~ year, data=data_2)
abline(fit_2)

## 4) 회귀모형에 대한 평가
summary(fit_2) # p<0.05, 회귀식 채택
anova(fit_2)

## 4.1) 오차항 정규성 검정
shapiro.test(resid(fit_2)) # 정규성 충족

## 4.2) 등분산성, 정규성, 이상치 확인
par(mfrow=c(2,2))
plot(fit_2, which=1)
plot(fit_2, which=2)
plot(fit_2, which=3)
plot(fit_2, which=4) # 이상치 확인
plot(fit_2, which=5) # 이상치의 위치 확인
plot(fit_2, which=6) # 관계정도를 구현(빨간줄 위주로 체크, 
                     # 그 외의 값은 이상치로 주시)



## 5) 회귀식을 통한 예측 : 근무연수가 25년인 경우 연봉은 얼마일까?

predict(fit_2, newdata=data.frame(year=c(25,27))) # 1325.44 

