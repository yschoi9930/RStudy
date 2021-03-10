#####################################
## 2021-03-05
## 다항 회귀모델, 가변수 회귀모델


## 실습. 아이스크림 판매량 예측

## lm() 모델 적용 예

temp <- c(38.8, 34.0, 39.0, 38.8, 36.2, 
          30.4, 32.2, 34.1, 35.0, 42.2, 
          37.3, 32.6, 31.6, 34.1, 34.1,
          33.8, 35.8, 32.3, 36.3, 31.3)
sale <- c(423000, 207900, 464600, 460000, 264500,
          107500, 161600, 131200, 206000, 910400,
          338600, 138300, 157400, 172100, 153000,
          127200, 200600, 116100, 265200, 132500)
ice <- data.frame(temp, sale)

library(psych)
psych::describe(ice)
psych::pairs.panels(ice)

iceFit <- lm(sale ~ temp, data=ice)
summary(iceFit)
plot(iceFit)

library(car)
durbinWatsonTest(iceFit)

shapiro.test(residuals(iceFit))
# 종속변수가 정규성을 만족하지 않으므로 변수변환 필요

pt <- powerTransform(ice$sale)
summary(pt)

#####  y의 log 변환 ##################

ice$sale <- log(ice$sale)
iceFit2 <- lm(sale ~ temp, data=ice)
pairs.panels(ice)
summary(iceFit2)

shapiro.test(residuals(iceFit2))
# 종속변수 정규성 만족

plot(iceFit2)
# 추정회귀식 : log(sale) = 6.06 + 0.18*temp
# 온도가 1도 상승할 때 log(판매액)은 6.24 증가
# 판매액은 exp(6.24)= 512.9 원 증가

pred <- predict(iceFit2, newdata = data.frame(temp=c(35, 40, 45)),
                interval = 'confidence')
# 예측 결과
pred[,1]
# 1        2        3 
# 12.27744 13.16595 14.05446 
exp(pred[,1])
# 1         2         3 
# 214795.0  522275.6 1269917.0 


#################################################
# polynomial regression
#

x <- c(0.04, 0.07, 0.11, 0.13, 0.20,
       0.27, 0.39, 0.42, 0.52, 0.56,
       0.61, 0.75, 0.78, 0.86, 0.89,
       0.92, 0.94, 0.97, 0.98, 0.99)
y <- c(1.42, 1.41, 1.37, 1.34, 1.26,
       1.21, 1.13, 1.08, 1.05, 1.04,
       1.02, 0.96, 0.98, 0.97, 0.98,
       0.97, 0.99, 0.98, 0.98, 0.98)
plot(x, y)
fit <- lm(y~x)
summary(fit)
abline(fit)
plot(lm(y~x), which=1)

help(poly)  # 다항 함수식
poly(x, 2)
x*x
poly_lm <- lm(y~poly(x, 2, raw=TRUE))  # y = b0 + b1* X + b2 * X^2
plot(poly_lm, which = 1)
summary(poly_lm)
anova(poly_lm)
fit
anova(fit, poly_lm)

# 다항회귀식 플롯팅

poly_lm$coefficients
b0 <- poly_lm$coefficients[1]
b1 <- poly_lm$coefficients[2]
b2 <- poly_lm$coefficients[3]

ft <- function(x){
  b0 + b1*x + b2*x^2
}

plot(x, y)
abline(fit))
curve(ft, 0, 1, add=TRUE)

plot(x, y)
abline(fit)
lines(x, fitted(poly_lm))

## women dataset
View(women)

# 실습문제. R의 women 데이터세트를 이용하여
1. 데이터구조와 특징을 파악
2. 단순선형모형에 대한 가정 평가 : plot() 이용
3. 단순선형모형이 적합한지? 아니면 다항 회귀를 해야하는지?
4. 3번에서 결정한 회귀모형으로 회귀식 추정
5. 3번에서 결정한 회귀모형 평가
6. 단순선형과 2차다항회귀모형 비교
7. 추정된 단순선형과 2차다항회귀식 시각화



## 실습문제1. R의 women 데이터셋을 이용하여
# 1. 데이터구조와 특징 파악
str(women)
psych::pairs.panels(women)

# 2. 단순선형모형에 대한 가정 평가 : plot() 이용
fit <- lm(weight~height, data=women)
summary(fit) # p < 0.05 회귀식 채택

plot(women)
abline(fit)

plot(fit, which = 1) # 특정 형태를 갖고 있으므로 선형성 만족 못함(비선형)

# 3. 단순선형모형이 적합한지? 아니면 다항 회귀를 해야하는지?
단순선형모형이 적합하지 않다. 데이터 값들이 회귀식에서 벗어난다.
따라서 다항회귀를 해야한다.

# 4. 3번에서 결정한 회귀모형으로 회귀식 추정
poly(women$height, 2)
poly_lm <- lm(women$weight~poly(women$height, 2, raw=T)) # y = b0 + b1*x + b2*x^2
plot(poly_lm, which = 1)

# 5. 3번에서 결정한 회귀모형 평가
summary(poly_lm)
# Multiple R-squared:  0.9995,	Adjusted R-squared:  0.9994 
# F-statistic: 1.139e+04 on 2 and 12 DF,  p-value: < 2.2e-16
anova(poly_lm)

# 6. 단순선형과 2차 다향회귀모형 비교
anova(fit, poly_lm)
# 데이터가 달라서 오류가 뜨는듯....원래는 잘 나와야 함
# 경고메시지(들): 
#   In anova.lmlist(object, ...) :
#   응답 ‘"women$weight"’를 가지는 모델들은 제거되었습니다.  
# 그 이유는 응답이 모델 1과 다르기 때문입니다

# 7. 추정된 단순선형과 2차 다항회귀식 시각화
plot(women)
abline(fit)
lines(women$height,fitted(poly_lm))


### 아이스크림 데이터 다항회귀
temp <- c(38.8, 34.0, 39.0, 38.8, 36.2, 
          30.4, 32.2, 34.1, 35.0, 42.2, 
          37.3, 32.6, 31.6, 34.1, 34.1,
          33.8, 35.8, 32.3, 36.3, 31.3)
sale <- c(423000, 207900, 464600, 460000, 264500,
          107500, 161600, 131200, 206000, 910400,
          338600, 138300, 157400, 172100, 153000,
          127200, 200600, 116100, 265200, 132500)
ice <- data.frame(temp, sale)
iceFit <- lm(sale ~ temp, data=ice)
plot(iceFit, which = 1)
poly_lm_ice <- lm(sale~poly(temp, 2, raw=TRUE), data=ice)  # y = b0 + b1* X + b2 * X^2
summary(poly_lm_ice)
plot(poly_lm_ice, which = 1)

b0 <- poly_lm_ice$coefficients[1]
b1 <- poly_lm_ice$coefficients[2]
b2 <- poly_lm_ice$coefficients[3]

ft <- function(x){
  b0 + b1*x + b2*x^2
}

plot(ice)
abline(iceFit)
curve(ft, min(temp), max(temp), add=TRUE)

##############################################################
# 가변수 회귀분석
# dummy variables

#  독립변수 중에 범주형 변수 존재

x1 <- c(507,391, 488, 223, 274, 287, 550, 457, 377, 101,
        170, 450, 309, 291, 375, 198, 641, 528, 500, 570)
x2 <-c("F","F","F","F","F","F","F","F","F","F",
       "M","M","M","M","M","M","M","M","M","M")
y <- c(1096, 759, 965, 698, 765, 703, 968, 805, 912, 588,
       281, 527, 439, 318, 412, 370, 1044, 537, 649, 807)
plot(x1, y, pch=x2)

x2 <-c("F","F","F","F","F","F","F","F","F","F",
       "C","C","C","C","C","C","C","C","C","C")


tlm <- lm(y~x1)  # 단순선형 y = b0 + b1*X1 
summary(tlm)

fm_lm <- lm(y~x1+x2) # 다중선형 y = b0 + b1*X1 + b2*X2M
summary(fm_lm)
model.matrix(fm_lm)
 # 추정된 회귀식  y = 399.19 + 1.17*X1 - 331.5 *X2M
 # if M = 1,   y = 399.19 - 331.5 + 1.17*X1  = 57.7 +  1.17*X1
 # if M = 0(F),   y = 399.19 + 1.17*X1

int_lm <- lm(y~x1+x2+x1:x2)
summary(int_lm)
model.matrix(int_lm)
# 추정된 회귀식  y = 476 + 0.96*X1 - 476 *X2M + 0.38*X1:X2M
# if M = 1,  y = 476 + 0.96*X1 - 476 *1 + 0.38*X1 = 1.44x1
# if M = 0(F),   y = 476 + 0.96*X1


anova(tlm, fm_lm, int_lm)

# 다중선형 y = b0 + b1*X1 + b2*X2M 회귀선 시각화

fm_coef <- coefficients(fm_lm)
fm_coef
plot(x1, y, pch=x2)
abline(tlm)
abline(fm_coef[1], fm_coef[2], lty=2, col="red")
abline(fm_coef[1]+fm_coef[3],
       fm_coef[2], lty=3,
       col="blue")
legend(locator(1), 
       c("F","F+M", "M"),
       lty=c(2,1,3),
       col=c("red", "black", "blue"))

# 다중선형 y = b0 + b1*X1 + b2*X2 + b3*X1:X2 회귀선 시각화
int_coef <- coefficients(int_lm)

plot(x1, y, pch=x2)
abline(tlm)
abline(int_coef[1], int_coef[2],
       lty=2, col="red")
abline(int_coef[1]+int_coef[3],
       int_coef[2]+int_coef[4],
       lty=3, col="blue")
legend(locator(1), c("F","F+M", "M"),
       lty=c(2,1,3),
       col=c("red", "black", "blue"))



