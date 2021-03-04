#####################
## 데이터 전처리
#####################

# 결측치 처리

z1 <- c( 1, 3, 4, NA, 5, 10, NA)
sum(z1, na.rm = T)

z2 <- c(2, 4, NA, 7, -10, 9, NA)

z1[is.na(z1)] <- 0   # 결측치를 0으로 대체
z2
z3 <- na.omit(z2)
z3 <- as.vector(na.omit(z2))  # 결측치 제거
z3

sum(z1)
?sum

rm(list=ls())

x <- iris
head(x, 10)
# str(x)
# summary(x)
x[3,5] <- NA
x[1,3] <- NA
x[2,4] <- NA
head(x)

sum(is.na(x)) # 결측치 개수 계산

# 열별로 결측치 개수 계산
ncol(x)

for(i in 1:ncol(x)){
  cat(colnames(x)[i], ':', sum(is.na(x[,i])), '\n')
}

cntNA <- function(y){
  return(sum(is.na(y)))
}

cntNA(x[,1])
apply(x, 2, cntNA)

colSums(is.na(x))
colSums(x)

# 행기준으로 결측치 개수를 계산

rowSums(is.na(x))



for(i in 1:nrow(x)){
  cat(rownames(x)[i], ':', sum(is.na(x[i,])), '\n')
}

cntNA(x[1,])
apply(x, 1, cntNA)

# 데이프레임에서 결측치 제외하고 새로운 데이터프레임을 생성

y <- x[complete.cases(x),]

install.packages('carData')
library(carData)
str(UN)
dim(UN)
head(UN)

sum(complete.cases(UN))
apply(UN, 1, cntNA)


mean(UN$lifeExpF, na.rm = T)
newUN <- UN[complete.cases(UN),]
dim(newUN)
mean(newUN$lifeExpF)
cntNA(UN$li)
      
?state.x77
str(state.x77)
st<-data.frame(state.x77)
str(st)
head(st)
boxplot(st$Income)
out<-boxplot.stats(st$Income)$out
rownames(st$Income==out)
rownames(st)[st$Income==out]


st$Income %in% out
st$Income[st$Income %in% out]<- NA
st$Income

nst<-st[complete.cases(st),]


x<- c(1,2,3,4,5,6,7,8)
newdata<-scale(x); newdata
mean(newdata)

airquality
transform(airquality, Ozone = -Ozone)
transform(airquality, new=-Ozone, Temp =(Temp-32)/1.8)

x<-c("ab","acd","fghi")
length(x)

nchar(x)
grep("c", c("b","B","A"))

install.packages("magtittr")
library(magrittr)

x<-c(1,4,9,4,-1,30)
mean(x)
x %>% mean
x[3]<-NA
x %>% is.na %>% sum

iris[order(iris$Sepal.Length),]
head(iris)

?order

install.packages("carData")
library(carData)

head(Highway1)
str(Highway1)

Highway1
Highway1[,1]
order(Highway1$rate, decreasing=T)
Highway1$rate
Highway1[order(Highway1$rate, decreasing=T),]
head(sort(highway1$len, decreasing = T), n=10)
?sort
len_longest <- head(sort(Highway1$len, decreasing=T), n=10)
sum(len_longest)

low_adt <- tail(sort(Highway1$adt, decreasing = T), n = 10)


str(Highway1)
levels(Highway1$htype)
nd <- split(Highway1, Highway1$htype)
nd

subset(Highway1, Highway1$len>= mean(Highway1$len), select = c('rate','slim','htype'))

a<-data.frame(name=c('Lee','Choi','Kim'), math=c(75,80,90))
b<-data.frame(name=c('Kim','Park','Choi'), eng = c(100,50,80))
a
b

merge(a,b)
merge(a,b,all=T)


n<-levels(Highway1$htype)
combn(n,2)

str(KosteckiDillon)
tot.mean<-mean(KosteckiDillon$dos)
tot.mean
idx<-sample(nrow(KosteckiDillon), nrow(KosteckiDillon)*0.01) ; idx
sdata<-(KosteckiDillon[idx,'dos']); sdata
mean(sdata)
tot.mean<-mean(KosteckiDillon$dos)
tot.mean
for(rate in (1:5)*0.1){
  idx<-sample(nrow(KosteckiDillon), nrow(KosteckiDillon)*rate)
  sdata <- KosteckiDillon[idx,'dos']
  smean<-mean(sdata)
  cat('Diff:', rate, tot.mean-smean,'\n')
}


apply(KosteckiDillon[,c('time','age','airq')],2,mean)
apply(KosteckiDillon[,c('time','age','airq')],2,mean,trim=0.1)
apply(KosteckiDillon[,c('time','age','airq')],2,max)

r1<-apply(KosteckiDillon[,c('time','age','airq')],2, mean)
r2<-lapply(KosteckiDillon[,c('time','age','airq')], mean)

class(r1); class(r2)

m<- list(a=c(1,2,3), b=c(5,6,7))
m
m.mean<-lapply(m, mean)

class(m.mean)

vector.m<-unlist(m.mean); 
vector.m
a<-lapply(iris[,c(1:4)],mean);a
test<-unlist(lapply(iris[,c(1:4)],mean))
test

sapply(m, function(x){x>5})
lapply(m, function(x){x>5})
tapply()

class(tapply(1:10, 1:10 %% 2==1, sum))
tapply(iris$Sepal.Length, iris$Species, mean)

x<- matrix(1:8,ncol=2,dimnames=list(c("봄","여름","가을","겨울"),c("남","여"))) ;x

gidx<-list(c(1,1,2,2,1,1,2,2),c(1,1,1,1,2,2,2,2))
tapply(x,gidx,sum)

install.packages("doBy")
library(doBy)

summaryBy(Sepal.Width+Sepal.Length ~ Species, iris)
orderBy(~ Species + Sepal.Length, iris)

quantile(iris$Sepal.Length, seq(0,1,0.2))
quantile(iris$Sepal.Length)


1,3,5,7

install.packages("carData")
library(carData)
head(carData::Chile)
ncol(carData::Chile)
a<-sum(is.na(carData::Chile[,1]));a
ncol
for (i in 1:ncol(carData::Chile)){
  cat(colnames(carData::Chile)[i], ":" , sum(is.na(carData::Chile[,i])),"\n")
}

tmp<-airquality
tmp(is.na(tmp[,Ozone]))<-NA
tmp[is.na(tmp$Ozone),'Ozone']<-0
      