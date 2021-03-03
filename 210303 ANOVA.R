## Turkey

install.packages('multcomp')
library(multcomp)


## 1. 정규성
install.packages("car")
library(car)
par(las=1)
par(mar=c(5,4,4,2))
qqPlot(lm(response ~ trt, data =cholesterol),
       simulate=T, main="Q-Q Plot", labels=F)

head(car)

## 2. 등분산성
# bartlett test
bartlett.test(response ~ trt, data =cholesterol)

# levene test
install.packages('lawstat')
library(lawstat)
levene.test(cholesterol$response, cholesterol$trt
            lacation='mean', correction.method ='zero.correction')

# fligner test()
# HH:hov()

###이상치 검토
library(car) ; outlierTest(fit)


# one way ANOVA

data(litter, package='multcomp')
View(litter)
str(litter)

attach(litter)
table(dose)
aggregate(litter$weight, by=list(dose=dose), FUN=mean)
aggregate(litter$weight, by=list(dose=dose), FUN=sd)

fit<-aov(weight ~ gesttime + dose, data = litter)
summary(fit)


# 공변량 요인(gesttime)을 제거한 주요인(dose)의 처리수준별 평균 계산
install.packages('effects')
library(effects)
effect("dose", fit)

library(multcomp)
contrast <- rbind("no drug vs drug" = c(-3,-1,-1,-1))
contrast
summary(glht(fit, linfct = mcp(dose=contrast)))

# 모형에 대한 가정 평가 : 정규성, 등분산성, 독립성

fit2 <- aov(litter$weight ~ litter$gesttime*litter$dose, data=litter)
summary(fit2)

install.packages('HH')
library(HH)
ancova(litter$weight ~ litter$gesttime + litter$dose, data=litter)
detach(litter)


fac1 <- gl(n=3, k=9, length=27, labels =c('Store A','Store B', 'Store C')) ; fac1
fac2 <- gl(n=3, k=3, length=27, labels =c('강남','홍대','종로')) ; fac2
consumer <- c(1,4,1,2,2,3,2,3,2,4,4,3,3,2,3,4,2,3,4,3,4,4,3,3,2,4,4)
data <- data.frame(consumer,fac1,fac2)
data

fit<-aov(consumer ~ fac1 + fac2, data = data)
summary(fit)

y2 <- c(700,820,710,830,540,680,530,710,450,590,470,590,460,600,470,610)
fac <- gl(n=3, k=4, length=16, labels=c('소형','준중형','중형','대형'))
group <- gl(n=2,k=2,16, length=16,labels=c("제조사A", "제조사B"))
data <- data.frame(y=y2, factor=fac, group=group) ; data


# 실습문제 1

# 1) 데이터 작성
group <- c(rep("A",8),rep("B",4),rep("C",7),rep("D",6)) ; group
group_ex <- rep(c("A","B","C","D"), c(8,4,7,6)) ; group_ex
weight <- c(257,205,206,231,190,214,228,203,201,164,197,185,
          248,165,187,220,212,215,281,202,276,207,204,230,227)
diet <- data.frame(group, weight); diet
str(diet)
diet$group<-as.factor(diet$group)
str(diet)
aggregate(weight, by=list(group=group), FUN=mean)
aggregate(weight, by=list(group=group), FUN=sd)

# 2.1) 정규성 
library(car)
par(las=1)
par(mar=c(5, 4, 4, 2))
qqPlot(lm(weight ~ group, data = diet),
       simulate=T, main="Q-Q Plot", labels=F)


# 2.2) 등분산성
library(lawstat)
bartlett.test(weight ~ group, data = diet)
levene.test(diet$weight, diet$group, 
            location="mean", correction.method = "zero.correction")
# p>0.05, 귀무가설 채택 -> 등분산성 만족


# 3) 검정 수행 및 결과 기술
fit <- aov(weight ~ group)
summary(fit)


# p>0.05, 귀무가설 채택 -> 차이가 없다.


# 4) 사후검정
## 1. 터키 방법
tukey <- TukeyHSD(fit, conf.level = 0.95)
tukey
plot(tukey)

library(multcomp)
tuk <- glht(fit, linfct=mcp(group="Tukey"))
plot(tuk)

library(gplots)
plotmeans(weight ~ group)

# 유의확률이 0.05 미만인 그룹이 없다

# 실습문제 2
qty<-matrix(c(12,14,12,20,4,18,6,17,20,22,16,17), ncol=3); qty
dimnames(qty)<- list(c("B1","B2","B3","B4"), c("A1","A2","A3"))
qty
# -> 이렇게 하면 안됨

species <- gl(3,4,12, labels=c("A1","A2","A3")); kind
fert <- gl(4,1,12, labels=c("B1","B2","B3","B4")); fert
qty<-c(12,14,12,20,4,18,6,17,20,22,16,17)
tomato <-data.frame(qty, species, fert); tomato
str(tomato)
aggregate(qty, by=list(species=species), FUN=mean)
aggregate(qty, by=list(fert=fert), FUN=mean)

# 1) 품종에 따른 생산량 차이
fert_aov <- aov(qty ~  fert)
summary(fert_aov)
# p>0.05 귀무가설 채택 -> 품종에 따른 차이가 없다

# 2) 비료에 따른 생산량 차이
species_aov<-aov(qty ~ species)
summary(species_aov)
# p>0.05 귀무가설 채택 -> 비료에 따른 차이가 없다

# 3) 품종과 비료의 상호작용의 영향
total_aov<-aov(qty ~ species + fert, data=tomato)
total_aov2<-aov(qty ~ species*fert, data=tomato)

summary(total_aov)
summary(total_aov2)

# 교호작용 없음

# 4) 그래프
# 아래 방법을 활용해서 그려보기(교호작용이 있을 시)

interaction.plot(species, fert, qty, type="b",
                 col=c("red","blue"), pch=c(16, 18),
                 main = "Interaction between fert and species")

install.packages("gplots")
library(gplots)
plotmeans(qty ~ interaction(species, fert, sep=" "),
          connect=list(c(1,3,5),c(2,4,6)),
          col=c("red", "darkgreen"),
          main = "Interaction Plot with 95% CIs",
          xlab="Treatment and Dose Combination")

library(HH)
interaction2wt(qty~species*fert, data=tomato)


