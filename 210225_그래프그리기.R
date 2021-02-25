str(mtcars)
aggregate(mtcars, by=list(mtcars$cyl,mtcars$gear), FUN=mean)
head(mtcars)
a<-as.factor(mtcars$cyl)
levels(a)

str(ggplot2::diamonds)
library(ggplot2)
aggregate(price ~ cut, diamonds, mean)
aggregate()
b<-airquality[,c('Month','Day','Solar.R')]
c<-b[order(b$Solar.R, decreasing = T),]
tail(c,n=10)

airquality[order(airquality$Solar.R, decreasing = T),c('Month','Day','Solar.R')]


head(airquality)

airquality[order(airquality$Solar.R, decreasing=T),]

?order


library(carData)
head(CES11)
data('CES11')
str(CES11)
CES11<-as.vector(CES11$absortion)
table(CES11$absortion)
table(CES11$absortion)/nrow(CES11$absortion)

head(cars)
dist<- cars[,2]
dist
boxplot(dist, main ='자동차 제동거리')
points(mean(dist), pch=3,col='red', cex=2)

boxplot(Petal.Length~Species, data=iris, main ="품종별 꽃잎의 길이", col=c("green",'red','blue'))

boxplot(mpg ~ , data=mtcars, notch=T)

head(mtcars)
mean(mtcars$wt);
wt>=mean(wt)

mtcars$cyl.f <- factor(mtcars$cyl,levels=c(4,6,8),
                labels=c('4','6','8'))
head(mtcars)

for(i in nrow(mtcars)){
  if(mtcars$wt[i] >= mean(mtcars$wt)){
  mtcars$wtlevel[i]<-'High'
  } else {
  mtcars$wtlevel[i]<-'Low'
  }
}

head(mtcars)
?rep
mtcars$wt[2]
str(mtcars)
mean(mtcars$wt)
mtcars$wtlevel[1]<-'Low'


mtcars$wtlevel2[mtcars$wt>= mean(mtcars$wt)]<- "High"
mtcars$wtlevel2[mtcars$wt< mean(mtcars$wt)]<- "Low"
mtcars$wt

mtcars<-subset(mtcars, select=-wtlevel2)
head(mtcars)

head(cars)
dist<-cars[,2]
dist
hist(dist, main ="Histogram for dist", xlab='dist', ylab='freq',
     border='blue', col='green',las=2, breaks=5)

table(diamonds$color)
class(diamonds$color)
cut.Freq<-table(diamonds$color)
addmargins(cut.Freq)

head(mtcars)


df<-data.frame(x=c('A','B','A','B'), y=c('1','2','2','1'), num =c(3,5,7,10))
df

xtabs(num ~ x+y, data=df)
xtabs( ~ x+y, data=df)

trees
hist(trees$Girth, border='red', col='orange', breaks=4)
bs<-read.csv('businessTrip.csv'); bs
str(bs)
bs$cost<-as.numeric(bs$cost)

barplot(table(diamonds$cut), horiz =T)
cnt<-table(diamonds$cut, diamonds$color)
barplot(cnt, col = rainbow(5),legend=T, beside=T)
par(mfrow=c(1,1))
barplot(cnt, col = rainbow(5),legend=T, beside=T)
feq<-table(diamonds$cut)
lbl<-paste(names(feq), feq, sep= '\n')
pie(table(diamonds$cut), labels=lbl, main = 'Quality', col=rainbow(5), radius = 1, clockwise=T,
    )
?pie

install.packages('plotrix')
library(plotrix)
pie3D(feq, labels=lbl, explode = 0.05)
fan.plot(feq, labels= lbl)

month=1:12
late=c(5,8,7,9,4,6,12,13,8,6,6,4)
plot(1:12, late, type='l',lty=2)
?plot
plot(iris[,1:4])
pairs(iris[,1:4])

mosaicplot(~ Sex + Survived + Class, data =Titanic)
data(mtcars)
str(mtcars)

str(bs)

table(bs$purpose)
table(bs$department)
table(bs$area)
table(bs$day)
table(bs$cost)
table(bs)

install.packages('formattable')
library(formattable)
HnA<-comma(c(54659,61028,53307,46161,54180), format='d')
HE<-comma(c(31215,29863,32098,39684,29707), format='d')
MC<-comma(c(15107,16133,15222,13208,9986), format='d')
VS<-comma(c(13470,14231,13401,13552,13193), format='d')
BS<-comma(c(16513,14947,15112,14392,17091), format='d')
LAB<-rbind(HnA,HE,MC,VS,BS)
colnames(LAB)<-c("19년 1Q","19년 2Q","19년 3Q","19년 4Q","20년 1Q")
head(LAB)
barplot(LAB, main="사업부문별 매출액", 
        col=c('#003f5c', '#58508d', '#bc5090', '#ff6361', '#ffa600'),
        horiz=T, las=1, beside=T, xlab="억 원", legend=T,
        args.legend=list(x='topright', bty='o', inset=c(-0.25,0)))
par(mfrow=c(1,1), mar =c(5,4,4,2)+.1)

?barplot



bs
bs$cost<-as.numeric(gsub(",","",bs$cost))
head(bs)
pie(table(bs$area), main = "지역")
barplot(table(bs$department), main ="부서명")
a<-table(bs$department,bs$area);a
barplot(a, main ="지역별 부서", legend=T)
boxplot(bs$cost)
barplot(table(bs$day), main="day")
b<-table(bs$department, bs$purpose); b
plot(b)
aggregate(bs[,6], by=list(bs$purpose), FUN=mean)

install.packages('Stat2Data')
library(Stat2Data)
data(Diamonds)
head(Diamonds)
ds<-Diamonds$PricePerCt
hist(ds, breaks=9, main="캐럿당 가격 분포", 
     las=2, xlab='캐럿당 가격($)', ylab='빈도',
     border = '#457b9d', col=color)
color<-rep('#1d3557',9)
color[3]<-'#a8dadc'; color
