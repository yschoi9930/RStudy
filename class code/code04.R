#####################
## 데이터 전처리
#####################

# 결측치 처리

z1 <- c( 1, 3, 4, NA, 5, 10, NA)
sum(z1, na.rm = T)

z2 <- c(2, 4, NA, 7, -10, 9, NA)

z1[is.na(z1)] <- 0   # 결측치를 0으로 대체
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

for(i in 1:ncol(x)){
  cat(colnames(x)[i], ':', sum(is.na(x[,i])), '\n')
}

cntNA <- function(y){
  return(sum(is.na(y)))
}

cntNA(x[,1])
apply(x, 2, cntNA)

colSums(is.na(x))

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
cntNA(UN$lifeExpF)

colMeans(newUN[, c('pctUrban', 'infantMortality')])


# 이상치 탐지 : 상자그림

?state.x77
st <- data.frame(state.x77)
boxplot(st$Income)
out <- boxplot.stats(st$Income)$out

# 결측치에 해당하는 주 이름 출력
rownames(st)[st$Income == out]

# 이상치 제거
st$Income[st$Income %in% out] <- NA
nSt <- st[complete.cases(st),]
head(nSt)


st[st$Incom==out, ]

# 데이터 변환

x <- c(1, 2, 3, 4, 5, 6, 7, 8)
?scale()
newdata <- scale(x)
newdata

newdata <- scale(x)*2 + 10
newdata


str(airquality)
head(airquality)

transform(airquality, Ozone=-Ozone)
transform(airquality, new=-Ozone, Temp=(Temp-32)/1.8)

# 데이터 프레임의 행과 열을 전치
t(mtcars[1:5, 1:4])


# 자료형 변환

class("2021-02-23")
date1 <- as.Date("2021-02-23")
as.numeric(date1)
date2 <- as.POSIXct("2021-02-23")
as.numeric(date2)

runif(10)
runif(10)
set.seed(100)
runif(10)
set.seed(100)
runif(10)
runif(10)
options(digits = 3)
runif(10)

# 문자열 관련 함수

x <- c("ab", "acd", "fghi")
length(x)

nchar(x)
substr("hahahhoho$$$", 2,7)
grep("A", c("bA", 'B', 'A'))
sub("*", '.', c("b*", '**B', 'A'))
sub("\\s", '.', c("Hello World"))
strsplit("123-456-790", "-")

# pipe : magrittr 패키지 %>%
install.packages("magtittr")
library(magrittr)

x <- c(1, 4, 9, 4, -1, 30)
mean(x)

x %>% mean

x[3] <- NA
x

x %>% is.na %>% sum


# 정렬 : sort(), order()

iris[order(iris$Sepal.Length),]
head(iris)
head(iris[order(iris$Sepal.Length),])
?order

iris[order(iris$Sepal.Length, decreasing = T, ),]
iris[order(iris$Species, iris$Sepal.Length, decreasing = F), ]


# carData 패키지 : Highway1 

install.packages('carData')
library(carData)

head(Highway1)
str(Highway1)

# 사고율(rate) 기준으로 데이터를 내림차순으로 정렬
Highway1[order(Highway1$rate, decreasing = T),]

# 구간의 길이(len)가 가장 긴 상위 10개 구간의 총길이 계산

len_longest <- head(sort(Highway1$len, decreasing=T), n=10)
sum(len_longest)

sum(head(Highway1[order(Highway1$len,decreasing = T),],10)['len'])

sum(head(Highway1[order(Highway1$len,decreasing = T),'len'], n=10))



# 일일교통량(adt)이 가장 작은 10개 구간의 일일교통량(adt)과  사고율(rate) 조사

tmp <- Highway1[order(Highway1$adt),c('adt','rate')]
tmp[1:10,]


# 제한속도(slim)가 높은 상위 5개 구간의 길이(len), 일일교통량(adt), 사고율(rate) 조사

tmp <- Highway1[order(Highway1$slim, decreasing = T),c('len', 'adt','rate')]
tmp[1:5,]

str(Highway1)
Highway1[c(1,3,10), 1]

rownames(Highway1)
colnames(Highway1)

order(Highway1$slim, decreasing = T)

Highway1[c(1,3,10), c('rate', 'adt')]


# 데이터 분리 : subset(), split()

str(Highway1)
levels(Highway1$htype)

nd <- split(Highway1, Highway1$htype)
str(nd$FAI)

summary(Highway1)

subset(Highway1, Highway1$len >= mean(Highway1$len), select = c('rate', 'slim', 'htype') )


# 데이터 병합 : merge()

a <- data.frame(name=c('Lee', 'Choi', 'Kim'), math=c(75, 80, 90))
b <- data.frame(name=c("Kim", "Park", "Choi"), eng=c(100, 50, 80))
a
b

cbind(a,b)
merge(a,b)
merge(a,b, all=T)

# 데이터 샘플링 :
# 복원, 비복원 추출 : sample()

sample(1:10, 3, replace = F)

idx <- sample(1:nrow(iris), 50, replace = F)
iris[idx,]

# 샘플링 
sample(1:10, 3, replace = T)
set.seed(1234)
sample(1:10, 3, replace = T)
set.seed(1234)
sample(1:10, 3, replace = T)


# 조합
combn(1:5, 3)

x <- c('red', 'green', 'blue', 'purple', 'yellow')
combn(x, 2)

# 실습. carData > Highway1데이터에 htype

n <- levels(Highway1$htype)
combn(n, 2)

# 실습. KosteckiDillon 데이터 세트 이용
# 전체 관측값의 10%, 20%, 30%, 40%, 50%를 샘플링했을 때 
# 평균치료일수(dos)가 전체데이터의 치료일수와 얼마나 차이가 나는가?

str(KosteckiDillon)
tot.mean <- mean(KosteckiDillon$dos)
tot.mean
cat("Total Mean =", tot.mean,'\n')
for(rate in (1:5)*0.1){
  idx <- sample(nrow(KosteckiDillon), nrow(KosteckiDillon)*rate)
  sdata <- KosteckiDillon[idx, 'dos']
  smean <- mean(sdata)
  cat('Diff(', rate*100, '%) :',tot.mean - smean, '\n')
}

sData$dos

head(KosteckiDillon)

apply(KosteckiDillon[,c('time', 'age', 'airq')], 2, mean)
apply(KosteckiDillon[,c('time', 'age', 'airq')], 2, mean, trim=0.1)
apply(KosteckiDillon[,c('time', 'age', 'airq')], 2, max)
apply(KosteckiDillon[,c('time', 'age', 'airq')], 2, range)

r1 <- apply(KosteckiDillon[,c('time', 'age', 'airq')], 2, mean)

r2 <- lapply(KosteckiDillon[,c('time', 'age', 'airq')], mean)

class(r1) ; class(r2)

m <- list(a=c(1,2,3), b=c(5,6,7))
m
r <- lapply(m, mean)

# list구조를 벡터로 변환 함수
unlist(r)
unlist(lapply(iris[, c(1:4)], mean))

# sapply() : lapply()와 유사, 반환값이 벡터, 행렬

r3 <- sapply(KosteckiDillon[,c('time', 'age', 'airq')], mean)
r3
sapply(KosteckiDillon[,c('time', 'age', 'airq')], range)
sapply(m, mean)

sapply(m, function(x) { x >5})
lapply(m, function(x) { x >5})

# tapply(벡터, 인덱스, 함수, ...) : 그룹별로 함수를 적용

tapply(1:10, 1:10 %% 2 == 1, sum)

tapply(iris$Sepal.Length, iris$Species, mean)
class(tapply(iris$Sepal.Length, iris$Species, range))

x <- matrix(1:8, ncol=2, dimnames = list(c("봄", "여름", "가을", "겨울"), c("남", "여")))
x

gidx <- list(c(1,1,1,2,1,1,2,2), c(1,1,1,1,2,2,2,2))

tapply(x, gidx, sum)

# mapply(함수, ) :sapply()

mapply(mean, iris[,1:4])
mapply(rnorm, c(10, 20, 30), c(10, 0, 100), c(2, 1, 10))

rnorm(10, 10, 4)

# apply(), lapply(), tapply(), sapply(), mapply()


# doBy::summaryBy(), orderBy()

install.packages("doBy")
library(doBy)

summaryBy(Sepal.Width + Sepal.Length ~ Species , iris)

orderBy(~ Species + Sepal.Length , iris)

quantile(iris$Sepal.Length, seq(0,1,0.2))

# 문제.

Students <- c("John Davis", "Angela Williams", "Bullwinkle Moose", 
              "David Jones", "Janice Markhammer", "Cheryl Cushing",
              "Reuven Ytzrhak", "Greg Knox", "Joel England",
              "Mary Rayburn")

name <- strsplit(Students, " ")

name[[2]]

Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)


## 422쪽 연습문제 1, 3, 5, 7번 

# 11-1

i <- 0
str(carData::Chile)
tdata <- carData::Chile
for(i in ncol(tdata)){
  cat(colnames(tdata)[i], ':', sum(is.na(tdata[,i])), '\n')
}

# 11-3

tmp <- airquality
tmp[is.na(tmp$Ozone), 'Ozone'] <- 0
tmp


# 11-5

airquality[order(airquality$Solar.R, decreasing = T), c('Month', 'Day', 'Solar.R')]

# 11-7
sample()
set.seed(1234)
