##########################################
##  2021-03-08

###########################################
##  cluster analysis
###########################################
# 데이터 척도화

air <- airquality[, 3:4]
View(air)

# 방법1. 평균 0, 표준편차1 표준화
df1 <- apply(air, 2, function(x) {(x-mean(x)/sd(x))}) # scale()
df1

# 방법2. 최대값으로 나누어줌
df2 <- apply(air, 2, function(x) {x/max(x)})
df2

# 방법3. 평균으로 뺸 편차를 MAD()로 나누어줌
df3 <- apply(air, 2, function(x) {(x-mean(x)/mad(x))})
df3

# 이상치 탐색
install.packages('outliers')
library(outliers)
outliers::outlier(airquality)

summary(airquality)

library(psych)
res <- psych::outlier(airquality[,1:4])
airquality[c(5, 9, 25, 48, 117),]

# 군집분석 알고리즘 수행

install.packages('flexclust')
library(flexclust)

data(nutrient, package = "flexclust")
head(nutrient)
View(nutrient)
str(nutrient)

row.names(nutrient) <- tolower(row.names(nutrient))
nutrient.scaled <- scale(nutrient) # 데이터 변환
nutrient.scaled

d <- dist(nutrient) # Euclidean Distance
?dist
class(d)

d <- dist(nutrient.scaled) # Euclidean Distance
d

# 계층적 군집분석 함수
?hclust
fit.average <- hclust(d, method = "average")
plot(fit.average, hang=1, cex=.8, main="Average Linkage Clustering")

# 군집 수 결정
install.packages('NbClust')
library(NbClust)
devAskNewPage(ask=TRUE)
nc <- NbClust(nutrient.scaled, distance = "euclidean",
              min.nc = 2, max.nc = 15, method = "average")
nc$Best.nc
class(nc$Best.nc)

table(nc$Best.nc[1,])

barplot(table(nc$Best.nc[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of clusters chosen by 26 Criteria")

## => 군집의 수는 4개

# 지정한 군집수로 자르기 : 최종군집방안 구하기
clusters <- cutree(fit.average, k=5)
table(clusters)
aggregate(nutrient, by=list(cluster=clusters), median)

aggregate(as.data.frame(nutrient.scaled),
          by=list(cluster=clusters), median)
rect.hclust(fit.average, k=5)



#### k-means

wssplot <- function(data, nc=15, seed=1234) {
  wss <- (nrow(data)-1) * sum(apply(data, 2, var))
  for (i in 2:nc) {
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

install.packages('rattle')
library(rattle)
data(wine, package='rattle')
View(wine)
str(wine)
head(wine)

# wine <- read.csv("data/wine.csv")

df.wine <- scale(wine[,-1])
wssplot(df.wine)

set.seed(1234)
devAskNewPage(ask=T)
nc <- NbClust(df.wine, min.nc = 2, max.nc = 15, method = 'kmeans')

set.seed(1234)
fit.km <- kmeans(df.wine, 3, nstart = 25)
fit.km$size
fit.km$centers

aggregate(wine[,-1], by=list(cluster=fit.km$cluster), mean)

## 와인품종과 군집소속 간의 교차표

ct.km <- table(wine$Type, fit.km$cluster)
ct.km

library(flexclust)
flexclust::randIndex(ct.km)

#### PAM (Partitioning Around medoids)

library(cluster)
set.seed(1234)
fit.pam <- pam(wine[-1], k=3, stand=TRUE)
fit.pam$medoids

clusplot(fit.pam, main="Bivarate Cluster Plot")

ct.pam <- table(wine$Type, fit.pam$clustering)
ct.pam


################################################
### Principal Component Analysis
################################################


install.packages('HSAUR')
library(HSAUR)

data("heptathlon", package='HSAUR')

View(heptathlon)
str(heptathlon)

summary(heptathlon)
hep <- heptathlon

# hurdle, run200m, run800m 는 데이터변환
hep$hurdles <- max(hep$hurdles) - hep$hurdles
hep$run200m <- max(hep$run200m) - hep$run200m
hep$run800m <- max(hep$run800m) - hep$run800m

# 총점score 제외
hep <- hep[,-8]

var(hep) # 다변량 자료에 대해서 공분산을 계산

colMeans(hep)
S <- cov(hep)   # 공분산행렬
S 
R <- cor(hep)   # 상관계수행렬
R

options(digits = 3)
eigen(S)   # S의 고유값과 고유벡터
eigen(R)   # R의 고유값과 고유벡터

# 주성분분석 : princomp()
hep.pca <- princomp(hep, cor=TRUE)
hep.pca
summary(hep.pca)
hep.pca$sdev
hep.pca$loadings  # 주성분 계수
hep.pca$scores    # 각 개체에 대한 주성분 값

all <- cbind(hep, hep.pca$scores)
all
all[order(hep.pca$scores[,1]),]


hep.pca.cov <- princomp(hep)  # 공분산행렬을 이용한 주성분 분석
hep.pca.cov
summary(hep.pca.cov)
hep.pca.cov$loadings

(hep.pca$sdev)^2

screeplot(hep.pca, type='lines', pch=1, main='scree plot')

hep.pca$scores[,1:2]

# prcomp() 함수 사용 
hep.pca2 <- prcomp(hep, center=T, scale.=T)   # 변수별 단위 표준화
hep.pca2

biplot(hep.pca, main='Biplot')


#################################################
### Exploring Factor Analysis(EFA)
#################################################

#주성분 요인법을 이용한 요인분석

# 데이터 읽기

med <- read.table("data/medFactor.txt", header=T)
head(med)
View(med)
str(med)

summary(med)
boxplot(med)

par(mfrow=c(1,1))

devAskNewPage(ask=F)
# 요인분석 수행

install.packages('GPArotation')
library(psych)
library(GPArotation)


med.factor <- principal(med, rotate = 'none')
med.factor
names(med.factor)
summary(med.factor)
med.factor$values     # 고유값 > 1
plot(med.factor$values, type='b')
# 요인 수 결정

# 요인수 지정한 후 직교회전을 적용한 요인분석
med.varimax <- principal(med, nfactors = 3, rotate = 'varimax')
med.varimax

biplot(med.varimax)

med.varimax2 <- principal(med, nfactors = 2, rotate = 'varimax')
med.varimax2

biplot(med.varimax2)

# 최우추정법을 이용한 요인분석
med.fact <- factanal(med, factors = 3, rotation = 'oblimin')
med.fact

med.fact2 <- factanal(med, factors = 3, rotation = 'varimax')
med.fact2
