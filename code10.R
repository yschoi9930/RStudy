###########################
## 2021-03-03
##
## correaltion

## iris data

library(dplyr)
irisData <- iris %>% select(Sepal.Length, Petal.Length)
head(irisData)

## 기본 통계치와 관계 확인(시각화)
library(psych)
pairs.panels(irisData)
describe(irisData)
plot(Petal.Length ~ Sepal.Length, data=irisData)
abline(lm(Petal.Length ~ Sepal.Length, data=irisData),
       col="red", lty=4)
     

## 상관계수와 가설검정
cor(irisData)
cor(irisData, use = "complete.obs", method="pearson")
cor.test(irisData$Sepal.Length, irisData$Petal.Length)

## iris 두 변수들간의 상관관계
plot(iris[, 1:4])
psych::pairs.panels(iris)
psych::pairs.panels(iris, lm=TRUE)
pairs(iris[,1:4])
cor(iris[, 1:4])

op <- par(no.readonly = TRUE)
par(mfrow=c(2,2))
attach(iris)
plot(Sepal.Length, Sepal.Width)
abline(lm(Sepal.Width~Sepal.Length))
plot(Sepal.Width, Petal.Length)
abline(lm(Petal.Length~Sepal.Width))
plot(Petal.Length, Petal.Width)
abline(lm(Petal.Width~Petal.Length))
plot(Petal.Width, Sepal.Length)
abline(lm(Sepal.Length~Petal.Width))
detach(iris)
par(op)


## 여러 개의 상관관계 유의성 검정
library(psych)

corr.test(iris[,1:4])
irisCor <- cor(iris[, 1:4])
irisCor
corr.test(irisCor)



## 상관계수 행렬의 시각화 함수들

install.packages("corrgram")
library(corrgram)
corrgram(cor(iris[,1:4]), upper.panel = panel.pie)
corrgram(cor(iris[,1:4]), upper.panel = panel.conf)

install.packages("corrplot")
library(corrplot)
corrplot(cor(iris[,1:4]), method = "ellipse")

install.packages("GGally")
library(GGally)
GGally::ggpairs(iris[,1:4])


## 상관계수 히트맵

install.packages("ggplot2")
library(ggplot2)

install.packages("reshape2")
library(reshape2)

irisCor <- cor(iris[, 1:4])
irisCor
irisMelt <- melt(irisCor, varnames=c("x","y"),
                 value.name="Correlation")
irisMelt

library(scales)

ggplot(irisMelt, aes(x=x, y=y)) +
  geom_tile(aes(fill=Correlation)) +
  scale_fill_gradient2(low=muted("red"),
                       mid="white", high="steelblue",
                       guide=guide_colorbar(ticks=FALSE,
                                            barheight = 10),
                       limits=c(-1,1)) +
  theme_minimal() +
  labs(x=NULL, y=NULL)



## 비정방행렬의 상관계수행렬
## state.x77  

head(state.x77)
states <- state.x77[,1:6]
x <- states[, c("Population","Income","Illiteracy","HS Grad")]
y <- states[, c("Life Exp", "Murder")]

# 비정방행렬의 상관계수 행렬
cor(x, y) 


## partial correlation
install.packages("ggm")
library(ggm)
colnames(states)
pcor(c(1,5,2,3,6), cov(states))

pcr <- pcor(c(1,5,2,3,6), cov(states))
pcor.test(pcr, c(2,3,6), length(states))