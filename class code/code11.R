########################################
###  2021-03-04
###  OLS: regression model 
########################################

# cars dataset
data(cars)
View(cars)
plot(cars)
cor(cars$speed, cars$dist)
library(psych)
psych::pairs.panels(cars)

## 단순선형회귀모델 생성
carFit <- lm(dist ~ speed, data=cars)
carFit
summary(carFit)

## 회귀모델 세부내용

# 회귀계수
coef(carFit)

# 적합(추정)된 Y값
fitted(carFit)
fitted(carFit)[1:4]

# 잔차 = Yi- 추정된 Yi
residuals(carFit)
residuals(carFit)[1:4]
data.frame(cars$dist, fitted(carFit), residuals(carFit))
fitted(carFit)[1:4] + residuals(carFit)[1:4]
cars$dist[1:4]

# 회귀계수의 신뢰구간
confint(carFit)

# 잔차제곱합
deviance(carFit)

## 예측과 신뢰구간

help("predict")
help("predict.lm")
coef(carFit) ## -17.579095 + 3.932409 * 3
predict(carFit, newdata = data.frame(speed=c(4.5, 9)))

# 예측하고자 하는 새로운 데이터는 모델 적합하기 위해 사용한
# 데이터의 범위 내에서 
coef(carFit)[1] + coef(carFit)[2]* c(4.5, 9)
predict(carFit, newdata = data.frame(speed=c(4.5, 9)),
        interval = "confidence")
predict(carFit, newdata = data.frame(speed=c(4.5, 9)),
        interval = "prediction")

## 회귀모형 평가

summary(carFit)
plot(carFit)
plot(cars$speed, cars$dist)
abline(coef(carFit))

# 모델간 비교
anova(carFit)
(full <- lm(dist ~ speed, data=cars))
(reduce <- lm(dist ~ 1, data=cars))
anova(reduce, full)

## 회귀모형 가정에 대한 검정

# 1.오차항의 정규성 검정
shapiro.test(residuals(carFit))

# 2. 등분산성, 정규성, 이상치 확인

help(plot.lm)

par(mfrow=c(2,2))
plot(carFit)   # enter key
plot(carFit, which=1)
plot(carFit, which=2)
plot(carFit, which=3)
plot(carFit, which=4)
plot(carFit, which=5)
plot(carFit, which=6)

par(mfrow=c(1,1))
plot(cars$speed, cars$dist)
abline(coef(carFit))

## 예측값의 신뢰구간 그리기

# 예측하기 위한 X
newd <- data.frame(speed=seq(min(cars$speed),
                             max(cars$speed), .2))
# 예측값과 신뢰구간
ys <- predict(carFit, newdata = newd, interval = "confidence")

# 시각화
matplot(newd, ys, type='n')
matlines(newd, ys, lty=c(1, 2, 2), col=1)
?matplot

## 실습

# 1
exc <- c(1095, 1110, 1086, 1074, 1098,
         1105, 1163, 1124, 1088, 1064)
exp <- c(49, 52, 48, 49, 50,
         51, 50, 51, 49, 48)
exData <- data.frame(exc, exp)
psych::describe(exData)
summary(exData)
plot(exData)
boxplot(exc)
psych::pairs.panels(exData)
cor.test(exData$exc, exData$exp)

tfit1 <- lm(exp~exc,data=exData)
summary(tfit1)
shapiro.test(residuals(tfit1))
plot(tfit1)

##데이터 수정
exData2 <- exData[-7,]
psych::pairs.panels(exData2)
cor.test(exData2$exc, exData2$exp)

tfit2 <- lm(exp~exc, data=exData2)
plot(tfit2)
summary(tfit2)
coef(tfit2)
residuals(tfit2)
fitted(tfit2)

predict(tfit2, newdata=data.frame(exc=c(1100)), 
        interval = 'confidence')
coef(tfit2)[1]+coef(tfit2)[2]*1100


nExc <- seq(min(exData$exc), max(exData$exc),10)
length(nExc)
pr <- predict(tfit2,newdata = data.frame(ne=nExc),
        interval = "confidence")
pr
anova(tfit1, tfit2)
matplot(nExc, pr, type='n')
matlines(nExc, pr, lty=c(1, 2, 2), col=1)


## 2
year <-c(26,16,20,7,22,15,29,28,17,3,
      1,16,19,13,27,4,30,8,3,12)
salary <-c(1267,887,1022,511,1193,
      795,1713,1477,991,455,
      324,944,1232,808,1296,
      486,1516,565,299,830)

dataS <- data.frame(year, salary)
psych::describe(dataS)
summary(dataS)
plot(dataS)
boxplot(salary)
psych::pairs.panels(dataS)
cor.test(dataS$year, dataS$salary)

sfit1 <- lm(salary~year,data=dataS)
summary(sfit1)
shapiro.test(residuals(sfit1))
plot(sfit1)
predict(sfit1, newdata=data.frame(year=c(25)))

##데이터 수정

dataS <- dataS[-7,]
psych::pairs.panels(dataS)
cor.test(dataS$year, dataS$salary)

sfit2 <- lm(salary~year, data=dataS)


####################################################
# multiple linear regression

# install.packages("dplyr")
library(dplyr)

class(state.x77)
colnames(state.x77)
states <- as.data.frame(state.x77) %>% select(1:3,5,7)
states

states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income","Frost")])
View(states)
cor(states)

library(psych)
install.packages('corrgram')
library(corrgram)
pairs.panels(states)
corrgram::corrgram(states)

install.packages('corrplot')
library(corrplot)
corrplot::corrplot(cov(states))
GGally::ggpairs()

fit <- lm(Murder ~ Population + Illiteracy + Income + Frost,
          data= states)
summary(fit)

plot(fit)

shapiro.test(residuals(fit))

data(mtcars)
colnames(mtcars)

# 상호작용이 있는 모델

fitCar <- lm(mpg ~  hp * wt, data=mtcars)
summary(fitCar)

library(effects)
plot(effect('hp:wt', fitCar, ,list(wt=c(2.2, 3.2, 4.2))),
     multiline=T)

describe(mtcars$wt)
plot(effect('hp:wt', fitCar, ,list(wt=c(2.5, 3.5, 4.5))),
     multiline=T)


library(car)
scatterplotMatrix(states, spread=FALSE, 
                  smoother.args=list(lty=2), 
                  main="Scatter plot Matrix")

#회귀모형진단
states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income","Frost")])

fit <- lm(Murder~Population + Illiteracy + Income + Frost, data=states)


confint(fit)  # 회귀계수에 대한 신뢰구간

View(women)
plot(women)
pairs.panels(women)

fitW <- lm(weight ~ height + I(height^2), data=women)
summary(fitW)

par(mfrow=c(2,2))
plot(fitW)

dev.off()
neww <- women[-c(13,15),]
newfit <- lm(weight ~ height + I(height^2), data=neww)
plot(newfit)
summary(newfit)


###################################
## car 패키지에서 제공하는 회귀모델의 가정과 관련된 특성을 검정하는 함수들


states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income","Frost")])
fit <- lm(Murder~Population + Illiteracy + Income + Frost, data=states)
par(mfrow=c(2,2))
plot(fit)
summary(fit)

library(car)
# 정규성 검정 : car::qqPlot()

car::qqPlot(fit, labels=row.names(states), id.method="identify",
       simulate=TRUE, main="Q-Q Plot")
# plot(fit, which=2) 유사

# 이상치 검토 
summary(states)
states["Nevada",]    # 'nevada' 의 데이터
fitted(fit)["Nevada"]
residuals(fit)["Nevada"]

rstudent(fit)["Nevada"]

# 잔차의 정규성 검토하기 위한 함수
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(fit)

# 오차의 독립성 검증
# 자기상관 검토(종속변수의 상관성) : 더빈-왓슨 검정

car::durbinWatsonTest(fit)

#선형성 검토 : Component & Residual plot

car::crPlots(fit)  # plot(fit, which=1)

# 등분산성 검정

car::ncvTest(fit)  # 검정통계량
car::spreadLevelPlot(fit)  # 시각화


# 선형 모델의 전반적인 검증
# 분포의 모양(치우침, 뾰족함, 이분산성, 선형모델 가정 전반에 대한 검정 수행)

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel)


# 다중공선성 측정을 위한 분산팽창계수

car::vif(fit)
sqrt(vif(fit)) > 2   # 2보다 크면 독립변수들간의 상관성 존재


# 이상치 검토
car::outlierTest(fit)

# 큰 지래점을 표시하는 함수

hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(fit)

help(hatvalues)
hatvalues(fit)
fitted.values(fit)
fitted(fit)

# 쿡 거리를 이용하여 영향치를 검토하기 위한 코드

cutoff <- 4/(nrow(states)-length(fit$coefficients)-2)
plot(fit, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

car::avPlots(fit, ask=FALSE, onepage=TRUE, id.method="identify")

# 이상치, 영향치, 지래점을 검토 표시하는 함수

car::influencePlot(fit, id.method="identify", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")


# 모델이 정규성 가정에 위배될 경우 반응변수 변환 필요
# 변수 변환을 위해 y^r 최대우도법으로 지수(r)를 산출하는 함수
# car::powerTransform()

summary(powerTransform(states$Murder))

# 예측(설명)변수의 변환을 위한 지수 산출
boxTidwell(Murder~Population+Illiteracy,data=states)


#########################################
# 변수 선택
##

library(MASS)

fit <- lm(Murder~Population + Illiteracy + Income + Frost,
          data=states)
stepAIC(fit, direction = "forward")
stepAIC(fit, direction = "backward")
stepAIC(fit, direction = "both")

View(mtcars)


######################
## 최선의 모델 선택

## 모델 비교 : anova() , AIC()

states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy","Income","Frost")])
fit1 <- lm(Murder~Population + Illiteracy + Income + Frost,
           data=states)

fit2 <- lm(Murder~Population + Illiteracy, data=states)
anova(fit1, fit2)

AIC(fit1, fit2)

# 변수의 선택 : 
# stepwise - MASS::stepAIC()

library(MASS)
states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy","Income","Frost")])
fit <- lm(Murder~Population + Illiteracy + Income + Frost,
          data=states)
stepAIC(fit, direction="backward")


## leaps::regsubsets()

library(leaps)
?leaps  ## all substes regression

states <- as.data.frame(state.x77[,c("Murder", "Population",
                                     "Illiteracy","Income","Frost")])
leaps <- regsubsets(Murder~Population + Illiteracy + Income + Frost,
                    data=states, nbest=4)
plot(leaps, scale = "adjr2")

library(car)
subsets(leaps, statistic="cp",
        main="Cp Plot for All Subsets Regression")

abline(1,1,lty=2,col="red")
