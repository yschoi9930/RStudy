#############################
## 2021-02-25 
#############################


# 데이터프레임 컬럼 접근 : with, within


print(iris$Sepal.Length)
print(iris$Petal.Length)

with(iris, 
     {
       print(Sepal.Length)
       print(Petal.Length)
     }
     )

within(iris, 
     {
       print(Sepal.Length)
       print(Petal.Length)
     }
)

x <- data.frame(val=c(1,2,3,4,NA,5,NA))

x <- within(x, 
            { val <- ifelse(is.na(val), median(val, na.rm=TRUE), val) })

x <- with(x, 
            { val <- ifelse(is.na(val), median(val, na.rm=TRUE), val) })
x

attach(iris)
print(Sepal.Length)
print(Petal.Length[2] * 100)

search()

detach(iris)
print(Sepal.Length)

search()


eng <- data.frame(name = c("Lee", "Choi", "Kim", "You"), 
                  eng = c(93,73,83,73)) 

math <- data.frame(name = c("You", "Kawk", "Lee", "Park"), 
                   math = c(89,79,69,99)) 
eng
math

score <- merge(eng, math, by = 'name', all = T) 
score

scoreTmp <- with(score, { 
  eng = ifelse(is.na(eng), median(eng, na.rm = T), eng)
}) 

scoreTmp
score

scoreTmp <- within(score, { 
  eng = ifelse(is.na(eng), median(eng, na.rm = T), eng)
}) 
scoreTmp

## 조건에 맞는 데이터의 인덱스 가져오기 : which(), which.min(), which.max()

id <- which(iris$Species == 'setosa')
iris[id,]

id.max <- which.max(iris$Sepal.Length)
iris[id.max,]

id.min <- which.min(iris$Petal.Length)
iris[id.min,]



## 집계 : aggregate(data, by, FUN)

#attach(mtcars)
str(mtcars)
aggregate(mtcars, by=list(Cyl=mtcars$cyl, Gear=mtcars$gear), FUN = mean)
aggregate(mpg ~ cyl + gear, mtcars, mean)

aggregate(iris, by=list(iris$Species), FUN=mean)
aggregate(Sepal.Length ~ Species, iris, mean)

library(ggplot2)
str(ggplot2::diamonds)
aggregate(price ~ cut, diamonds, FUN=mean)

aggregate(cbind(mpg, hp) ~ cyl+gear, mtcars, mean)


####################################
## dplyr 패키지 : select(), filter(), mutate(), slice(), summarize()

install.packages('dplyr')
library(dplyr)

diamonds[,c('carat','color','price')]
select(diamonds, carat)
diamonds %>% select(carat)
diamonds %>% select(c(carat, color))
diamonds %>% select(1,7)
diamonds %>% select(starts_with('c'))
diamonds %>% select(ends_with('e'))
diamonds %>% select(contains('l'))
diamonds %>% select(matches('r.+t'))
diamonds %>% select(-carat, -price)
diamonds %>% select(-c(carat, price))
diamonds %>% select(-c(1,7))
str(diamonds)

diamonds[diamonds$cut == 'Good',]
subset(diamonds, cut == 'Good')
diamonds %>% filter(cut == 'Good')
diamonds %>% filter(cut %in% c('Good', 'Ideal'))
diamonds %>% filter(price >= 1000, price <= 1500)
diamonds %>% filter(price >= 1000 | carat > 5)

diamonds %>% slice(1000:1500)
diamonds %>% slice(c(1:5, 10, 50:55))

diamonds %>% mutate(newPrice=price/carat)

diamonds %>% select(carat, price) %>% mutate(Ratio=price/carat)

diamonds2 <- diamonds
diamonds2 %<>% select(carat, price) %>% mutate(Ratio=price/carat)
diamonds2

# 
diamonds %>% summarize(mean(price), mean(carat))

diamonds %>% group_by(cut) %>% summarize(mean(price), mean(carat))

topN <- function(x, N=5){
  x %>% arrange(desc(price)) %>% head(N)
}

diamonds %>% group_by(cut) %>% do(topN(., 3))

##########################################################
## 실습

options(digits=2)
Students <- c("John Davis", "Angela Williams", "Bullwinkle Moose", 
              "David Jones", "Janice Markhammer", "Cheryl Cushing",
              "Reuven Ytzrhak", "Greg Knox", "Joel England",
              "Mary Rayburn")
Math <- c(502, 600, 412, 358, 495, 512, 410, 625, 573, 522)
Science <- c(95, 99, 80, 82, 75, 85, 80, 95, 89, 86)
English <- c(25, 22, 18, 15, 20, 28, 15, 30, 27, 18)
roster <- data.frame(Students, Math, Science, English, 
                     stringsAsFactors = FALSE)

roster

# 1. 학생들의 점수를 같은 기준에서 비교 가능하도록 데이터 표준화
#     scaling : Z-score

new <- scale(roster[,2:4])
score <- apply(new, 1, mean)
score

roster <- cbind(roster, score)
roster

y <- quantile(score, c(0.8, 0.6, 0.4, 0.2))

roster$grade[score >= y[1]] <- 'A'
roster$grade[score < y[1] & score >= y[2]] <- 'B'
roster$grade[score < y[2] & score >= y[3]] <- 'C'
roster$grade[score < y[3] & score >= y[4]] <- 'D'
roster$grade[score < y[4]] <- 'F'
roster

name <- strsplit(Students, " ")
Lastname <- sapply(name, "[", 2)
Firstname <- sapply(name, "[", 1)

roster <- cbind(Firstname, Lastname, roster[,-1])
roster[order(Lastname,Firstname),]


#############################################
#

weight.M <- c(72, 74, 77, 68, 66, 75)
weight.F <- c(45, 48, 52, 53, 46, 50)

mean(weight.M)
median(weight.M)
fivenum(weight.M)
summary(weight.M)

AStore <- c(20, 21, 23, 22, 26, 28, 35, 35, 41, 42,
           43, 45, 44, 45, 46, 47, 47, 46, 47, 58,
           59, 60, 56, 57, 57, 80)
BStore <- c( 5, 6, 11, 13, 15, 16, 20, 20, 21, 23,
             22, 27, 27, 30, 30, 32, 36, 37, 37, 40,
             40, 43, 44, 45, 51, 54, 70, 600)
CStore <- c(5, 5, 5, 12, 10, 11, 20, 20, 20, 20,
           20, 21, 20, 30 ,32, 31, 31, 31, 36, 40,
           40, 51, 61, 51, 61, 61, 70)

mean(AStore) ; mean(BStore)  ; mean(CStore)
median(AStore) ; median(BStore)  ; median(CStore)
sd(AStore) ; sd(BStore)  ; sd(CStore)

quantile(AStore) ; quantile(BStore) ; quantile(CStore) 

id.B <- which(BStore != max(BStore))
mean(BStore[which(BStore != max(BStore))])
new.BStore <- BStore[which(BStore != max(BStore))]

sd(AStore)/mean(AStore) ; sd(new.BStore)/mean(new.BStore)  ; sd(CStore)/mean(CStore)


mystat <- function(x, na.omit=FALSE) {
  if(na.omit)
    x <- x[!is.na(x)]
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^2)/n - 3
  cv <- s/m
  return(c(mean=m, SD=s, Skewness=skew, Kurtosis=kurt, CV=cv))
}

mystat(AStore) ; mystat(new.BStore) ; mystat(CStore)
options(digits = 2)

sapply(iris[,1:4], mystat)

###############
# 그룹별 집계함수를 이용한 기술통계 계산

aggregate(iris[,1:4], by=list(Species=iris$Species), mean)
by(iris[,1:4], iris$Species, mean)

library(doBy)
doBy::summaryBy(Petal.Length + Petal.Width ~ Species, data=iris, FUN=mean)

library(Hmisc)
Hmisc::describe(iris[,c('Petal.Length','Petal.Width')])

library(pastecs)
stat.desc(iris[,c('Petal.Length','Petal.Width')])

library(psych)
psych::describe(iris[,c('Petal.Length','Petal.Width')])
psych::describeBy(iris[,1:4], list(Species=iris$Species))



#########################
# 데이터 시각화

# boxplot

boxplot(BStore)
boxplot(new.BStore)
mystat(new.BStore)

boxplot(AStore, new.BStore, CStore, names=c("A","B","C"))
points(c(mean(AStore), mean(new.BStore), mean(CStore)),
       pch=3, col='red', cex=2 )

boxplot(mtcars$mpg, main="MtCars", ylab='Miles per Gallon')
boxplot(mtcars$mpg, main="MtCars", ylab='Miles per Gallon', horizontal = T)

boxplot(mpg ~ cyl, data=mtcars, main="Car Mileage Data",
        xlab='Cylinder', ylab='Miles per Gallon')

mtcars$cyl.f <- factor(mtcars$cyl, levels=c(4,6,8),
                      labels=c("4","6","8"))
mtcars$gear.f <- factor(mtcars$gear, levels=c(3,4,5),
                      labels=c("3","4","5"))
mtcars$am.f <- factor(mtcars$am, levels=c(0,1),
                       labels=c("auto","standard"))


head(mtcars)

boxplot(mpg ~ cyl.f * am.f, data=mtcars, main="MPG data by Cylinder Type",
        xlab='Cylinde Type  ', ylab='Miles per Gallon')

boxplot(mpg ~ am.f*cyl.f, data=mtcars, main="MPG data by Auto type",
        xlab='Auto Type  ', ylab='Miles per Gallon',
        col=c('gold','darkgreen'))
str(mtcars)

boxplot()



# 문제. iris 데이터에서 Species를 그룹으로하여 Petal.Length을 boxplot

boxplot(Petal.Length~Species, data=iris)



########################
## histogram

hist(AStore, breaks = 10, main='AStore의 배달시간 분포',
     xlab='배달시간 구간', ylab='빈도', col='gold')
# plot(AStore)

########################
## 빈도표(분할표)
## table(... , )

table(mtcars$cyl.f)

table(diamonds$color)
table(diamonds$cut)

class(diamonds$color)

cut.Freq <- table(diamonds$cut)
addmargins(cut.Freq)

table(mtcars[,c('cyl.f', 'am.f')])
addmargins(table(mtcars[,c('cyl.f', 'am.f')]))

head(mtcars)
# xtabs( ~ , data=)

df <- data.frame(x=c('A','B','A','B'), y=c('1', '2','2','1'),
                 num=c(3,5,7,10))
df

xtabs( num ~ x + y , data=df)
xtabs(~ x + y , data=df)


xtabs( ~ cyl.f, data=mtcars)
xtabs( ~ am.f, data=mtcars)

ft <- xtabs( ~ cyl.f + am.f, data=mtcars)
ft
margin.table(ft, 2)
prop.table(ft,2)

####################################
## Barplot

barplot(table(diamonds$cut), horiz = T)
cnt <- table(diamonds$cut, diamonds$color)
barplot(cnt, col = rainbow(5),
        legend=T, beside = T)

par(mfrow=c(1,2))
barplot(cnt, col = rainbow(5),
        legend=T, beside = T)

par(mfrow=c(1,1))

feq <- table(diamonds$cut)
 
#####################################
#### piechart

lbl <- paste(names(feq), feq, sep = '\n')
pie(feq, labels=lbl, clockwise = T, radius = 0.7,
    col=rainbow(5))


install.packages('plotrix')
library(plotrix)
pie3D(feq, labels=lbl, explode = 0.05)

fan.plot(feq, labels = lbl)

###################################
#### line 
nrow(mtcars)
head(mtcars)



late <- c(5, 7, 10, 3, 2, 1, 0, 0, 1, 0)
plot(1:10, late, type='l', lty=6, lwd=1)

plot(diamonds$cut)

##################################
#### scatterplot

plot(mtcars$mpg, mtcars$hp, pch=3)

#################################
### scatterplot matrix

plot(iris[,1:4])
pairs(iris[,1:4])

###############################
### parallel plot

install.packages('MASS')
library(MASS)
MASS::parcoord(iris[,1:4])


###############################
### Mosaic plot


str(Titanic)
mosaicplot(~Sex + Survived + Class, data=Titanic)

install.packages('psych')
library(psych)

data(mtcars)
str(mtcars)
