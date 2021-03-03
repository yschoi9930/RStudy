########################
## practice : ANOVA
########################

# 1

ya <- c(257,205,206,231,
        190,214,228,203)
yb <- c(201,164,197,185)
yc <- c(248,165,187,220,
        212,215,281)
yd <- c(202,276,207,204,
        230,227)
weight <- c(ya, yb, yc, yd)

dietType <- rep(c("A", "B", "C", "D"),
                c(length(ya),
                  length(yb),
                  length(yc),
                  length(yd)))

dietType <- factor(dietType)

mouseWeight <- data.frame(dietType, weight)
str(mouseWeight)
mouseWeight

# 그룹별 평균과 표준편차

attach(mouseWeight)
table(dietType)
aggregate(weight, by=list(dietType),
          FUN=mean)

aggregate(weight, by=list(dietType),
          FUN=sd)

detach(mouseWeight)

## 2 일원분산분석: 가정 검토
## 정규성검정
shapiro.test(mouseWeight$weight)
# p-value = 0.09846

install.packages("car")
library(car)
qqPlot(lm(weight ~ dietType,
          data=mouseWeight),
       simulate=TRUE,
       main="Q-Q Plot",
       labels=FALSE)

## 등분산성 검정
bartlett.test(weight ~ dietType,
              data=mouseWeight)


## 3. 그룹별 평균에 대한 시각화

dev.off()
install.packages("gplots")
library(gplots)

plotmeans(weight ~ dietType,
          data=mouseWeight,
          xlab="dietType",
          ylab="weight",
          main="Mean Plot with 95% CI")


## 식이요법에 따라 체중의 차이가 있는지?

fit <- aov(weight ~ dietType,
           data=mouseWeight)

summary(fit)  # p-value : 0.346


## 4. 다중비교

op <- par(no.readonly = T)
par(las=2)
margin(5,4,6,2)

install.packages("multcomp")
library(multcomp)
tuk <- glht(fit,
            linfct=mcp(dietType="Tukey"))

plot(cld(tuk, level=.05),
     col="skyblue")

plot(TukeyHSD(fit))



## 2번

response <- c(12,14,12,20,
              4,18,6,17,
              20,22,16,17)

# 품종
trt1 <- rep(c("A1","A2","A3"),
            each=4)

# 비료의 종류
trt2 <- rep(c("B1","B2","B3","B4"),
            times=3)

tomato <- data.frame(trt1, trt2, response)
str(tomato)
tomato

## 품종,비료 데이터 구성하는 다른 방법

trt1<- gl(n=3, k=4, length=12,
          labels=c("A1","A2","A3"))
trt2<- gl(n=4, k=1, length=12,
          labels=c("B1","B2","B3","B4"))


# 그룹별 평균과 표준펀차

attach(tomato)
table(trt1, trt2)
aggregate(response, by=list(trt1),
          FUN=mean)

aggregate(response, by=list(trt2),
          FUN=mean)

aggregate(response, by=list(trt1),
          FUN=sd)

aggregate(response, by=list(trt2),
          FUN=sd)

detach(tomato)

## 이원분산분석

fit2 <- aov(response~trt1+trt2,
            data=tomato)

fit2
summary(fit2)

## 검정결과
##
##           F value Pr(>F)
##  trt1          3.280  0.109
##  trt2          2.338  0.173
##
## 두 요인 모두 생산량에 영향을 주지 않음


## 그룹별 평균 플롯

install.packages(c("effects", "gplots"))
library(effects)
library(gplots)
plotmeans(response ~ interaction(trt1, trt2, sep=" "),
          connect=list(c(1,5,9),c(2,6,10),
                       c(3,7,11), c(4,8,12)),
          col=c("red", "darkgreen", "skyblue", "lightgray"),
          main = "Interaction Plot with 95% CIs",
          xlab="Trt2 and Trt1 Combination")

install.packages("HH")
library(HH)
interaction2wt(response~trt1*trt2,
               data=tomato)
