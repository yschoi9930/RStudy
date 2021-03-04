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


# multiple linear regression

install.packages("dplyr")
library(dplyr)

class(state.x77)
colnames(state.x77)
states <- as.data.frame(state.x77) %>% select(1:3,5,7)
states
states <- as.data.frame(state.x77[,c("Murder","Population","Illiteracy",
                                     "Income", "Frost")]
                        
view(states)
cor(states)

install.packages('corrgram')
library(corrgram)
psych::pairs.panels(states)
corrgram(states)

install.packages('corrplot')
install.packages('GGally')
library(corrplot)
corrplot(cor(states))
GGally::ggpairs(states)

fit <- lm(Murder ~ Population + Illiteracy + Income +Frost, data=states)
summary(fit)
par(mfrow=c(1,1))

plot(fit)

shapiro.test(residuals(fit))

data(mtcars)
colnames(mtcars)
fitcars <- lm(mpg ~ hp * wt , data=mtcars) # = hp + wt + hp:wt
summary(fitcars)

library(effects)
plot(effect('hp:wt', fitcars, list(wt=c(2.2,3.2,4.2))), multiline=T)

fit_3 <- lm(Murder ~ Population + Illiteracy + Income + Frost, data =states)
confint(fit_3)

# 정규성 검정
library(car)
qqPlot(fit_3, labels=row.names(states), simulate=T, 
       id.method="identify")

# 이상치 검토
fitted(fit_3)["Nevada"]
resid(fit_3)["Nevada"]
rstudent(fit_3)["Nevada"]

# 오차의 독립성, 자기상관 검토(종속 변수의 상관성)
durbinWatsonTest(fit_3) # p = 0.246 -> 귀무가설 채택 

# 선형성 : component & residual plot
crPlots(fit) # 각각의 성분을 나눠 더 디테일하게 보여줌 plot(fit, which=1)

# 등분산성 검정
ncvTest(fit) # p = 0.186 -> 귀무가설 채택
spreadLevelPlot(fit_3) # 시각화

# 분포의 모양(치우침, 뾰족함, 이분산성, 선형모델 가정 전반에 대한 검정 수행)
install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit_3)
summary(gvmodel) # p > 0.05 여야 해당 속성들을 충족한다는 것을 의미

# 다중공선성 측정을 위한 분산팽창 계수
vif(fit)
sqrt(vit(fit)) >2 # 2보다 크면 독립변수 간 상관성 존재

# 이상치, 지래점, 영향치 모두 보여주는 plot
influencePlot(fit_3)

data(trees)

# 실습문제
# 1) 나무 둘레로 나무의 볼륨을 예측하는 단순선형 회귀모델을
# 나무둘레가 8.5,9.0,9.5 일때, 나무의 볼륨을 예측하라

head(trees)
describe(trees)
scatterplotMatrix(trees, spread=F)

cor(trees$Girth, trees$Volume)
cor.test(trees$Girth, trees$Volume) # p < 0.05 상관관계

plot(trees$Girth, trees$Volume)

fit_trees <- lm(Volume ~ Girth, data = trees)
summary(fit_trees) # 회귀식 채택
abline(fit_trees)

predict(fit_trees, newdata = data.frame(Girth=c(8.5,9.0,9.5)))
# 6.116320  8.649249 11.182177 

# 2) Girth, Height으로 Volume 예측

fit_trees2 <- lm(Volume ~ Girth + Height, data=trees)
summary(fit_trees2) # 회귀식 채택

# 모델 검토

par(mfrow=c(2,2))
plot(fit_trees2)

## 1) 정규성, 선형성, 등분산성

library(car)
car::qqPlot(fit_trees2) # 정규성
car::durbinWatsonTest(fit_trees2) # 정규성성

car::crPlots(fit_trees2) # 선형성

car::ncvTest(fit_trees2) # 등분산성 귀무가설 채택
car::spreadLevelPlot(fit_trees2) 

# 선형모델의 전반적인 검증, 분포의 모양 체크
gvmodel <- gvlma(fit_trees2)
summary(gvmodel) # Not satisfied가 존재

# 다중공선성 측정
car::vif(fit_trees2)
sqrt(vif(fit_trees2)) # 2보다 작기에 상관성 없음

# 이상치, 지래점, 영향치 검토
car::outlierTest(fit_trees2)
car::avPlots(fit_trees2)
car::influencePlot(fit_trees2)
