###########################
## 2021-03-05
## 고급 그래픽 패키지
## lattice & ggplot2
###########################


## lattice package

install.packages("lattice")
library(lattice)

# xyplot

help(xyplot)

head(airquality)
View(airquality)
xyplot(Ozone ~ Wind, data = airquality)

# bwplot

attach(airquality)
plot(Wind,Ozone)
detach(airquality)

?singer
View(singer)
bwplot(voice.part ~ height,data=singer,
       xlab="Height (inches)")
head(singer)


# histogram

help("histogram")
histogram( ~ height | voice.part,
           data = singer, layout=c(4,2))
           
histogram( ~ height | voice.part,
           data = singer, 
           nint = 17,
           endpoints = c(59.5,76.5),
           layout = c(2,4),
           aspect = 1,
           xlab = "Height (inches)")

# stripplot

help("stripplot")
stripplot(voice.part ~ height, 
          data = singer)
stripplot(voice.part ~ jitter(height), 
          data = singer, aspect = 1,
          jitter.data = TRUE, 
          xlab = "Height (inches)")

# dotplot

head(barley)
View(barley)
dotplot(variety ~ yield | year * site, 
        data=barley)


dotplot(mpg ~ wt, data=mtcars)

# splom()

help(splom)
head(quakes)
splom(quakes[,1:4])

# levelplot()

parallel(iris)

help("levelplot")
x<-rep(seq(6,15,by=1),each=20)
y<-rep(seq(0,0.95,by=0.05),10)
z<-x*y
x
y
z
levelplot(z ~ x * y,
          scales=list(x=list(at=seq(6,15,1),cex=2), 
                      y=list(at=seq(0,0.9,0.1), cex=2)),
          xlab=list(label="x", cex=2),
          ylab=list(label="y", cex=2))

x1 <- seq(-3,3,length=50)
x2 <- seq(-4,4,length =60)
f <- function(x1,x2){x1^2+x2^2+x1*x2}
y <- outer(x1,x2,FUN=f)
contour(x1,x2,y)

help(outer)
y

################################################
##  ggplot2 패키지
################################################
install.packages("ggplot2")
library(ggplot2)

s <- ggplot(data=iris,aes(Sepal.Length, Petal.Length, colour=Species))
s + geom_point()
summary(s)


# aes( )함수로 x축 y축 색상 매핑 
p <- ggplot(mtcars, aes(wt, mpg, colour=factor(cyl)))
p

# Geometric object로 점 정의
p <- p + geom_point()

# ggplot 클래스 객체인 p 출력
p

p <- ggplot(mtcars, aes(factor(cyl), fill=factor(cyl)))
p <- p + geom_bar(width=.5)   # 막대정의 : 너비 0.5
p <- p + facet_grid(. ~gear)    # gear 변수에 따라 서로 다른 패싯에 출력(패널)
p

p <- ggplot(mtcars, aes(wt, mpg))
p <- p + geom_point( )  # 산점도 그림
p <- p + geom_smooth(method="loess")  # 평활(smooth) 정의
p  # p 출력


ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point()

qplot(mtcars$wt, mtcars$mpg, geom="point")
qplot(wt, mpg, data=mtcars, geom="point")

p <- ggplot(mtcars, aes(wt, mpg))
p + geom_point()

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(aes(x=wt, y=mpg))
print(p)   # 화면에 플롯으로 출력함

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(colour="orange", size=6)
summary(p)

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p + geom_point(aes(colour=cyl, size=gear))

p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + xlim(1, 5) + ylim(10, 35)
p + geom_abline(intercept = 37, slope = -5)

# 회귀추정식을 적용
mtcars_coefs <- coef(lm(mpg ~ wt, mtcars))
mtcars_coefs
p <- ggplot(data=mtcars, aes(x=wt, y=mpg))
p <- p + geom_point()
p + geom_abline(intercept = mtcars_coefs["(Intercept)"], 
                  slope = mtcars_coefs["wt"], colour="red")

p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar()
p + geom_bar(binwidth=1)

p <- ggplot(data=mtcars, aes(factor(cyl)))
p + geom_bar(binwidth=1)

qplot(Sepal.Length,Petal.Length,data=iris,
      color=Species,size=Petal.Width)

head(Orange)
qplot(age,circumference,data = Orange, geom = "line",
      colour = Tree, main="How does orange tree circumference vary with age?")


qplot(Sepal.Length,Petal.Length,data=iris,color=Species)

ggplot(data=iris, aes(x=Sepal.Length, y= Petal.Length))+geom_point(aes(colour = Species))

# MASS::Cars93

library(MASS)
(ggplot(data=Cars93, aes(x=Weight, y=MPG.highway, colour=Type)) 
+ geom_point(shape=19, size=3)
+ ggtitle("Scatter Plot by Type, using different Colours"))

(ggplot(data=Cars93, aes(x=Weight, y=MPG.highway, shape=Type))
+ geom_point(size=2)
+ ggtitle("Scatter Plot by Type, differenct Shapes"))


p <- ggplot(mtcars, aes(x=cyl))
p <- p+ geom_bar(fill="yellow")
p
help(scale_fill_brewer)      

# MASS::Tooth Growth
df2 <- data.frame(supp=rep(c("VC", "OJ"), each=3),
                  dose=rep(c("D0.5", "D1", "D2"),2),
                  len=c(6.8, 15, 33, 4.2, 10, 29.5))

p <- ggplot(data=df2, aes(x=dose, y=len, fill=supp)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  geom_text(aes(label=len), color="red",position = position_dodge(0.9), size=3.5)
p + scale_fill_brewer(palette="Blues")

df2


#### 집단화(grouping) & 측면화(faceting)

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  labs(title = "Automobile Data",
       x="Weight", y="Miles per Gallon")

mtcars$am <- factor(mtcars$am, levels = c(0,1),
                    labels=c("Automatic", "Manual"))
mtcars$vs <- factor(mtcars$vs, levels = c(0,1),
                    labels=c("V-Engine", "Straight Engine"))
mtcars$cyl <- factor(mtcars$cyl)

ggplot(data=mtcars, 
       aes(x=hp, y=mpg, shape=cyl, color=cyl)) +
  geom_point(size=3) +
  facet_grid(am~vs) +
  labs(title = "Automobile Data by Engine Type",
              x="Horsepower", y="Miles per Gallon")


install.packages("carData")
library(carData)
data(Salaries, package = "carData")
library(ggplot2)
ggplot(data=Salaries, aes(x=salary, fill=rank)) +
  geom_density(alpha=0.3)

ggplot(data=Salaries,
       aes(x=yrs.since.phd,
           y=salary,
           color=rank,
           shape=sex)) +
  geom_point()
ggplot(data=Salaries,
       aes(x=rank, fill=sex)) +
  geom_bar(position="stack") +
  labs(title='postion="stack"')

ggplot(data=Salaries,
       aes(x=rank, fill=sex)) +
  geom_bar(position="dodge") +
  labs(title='postion="dodge"')

ggplot(data=Salaries,
       aes(x=rank, fill=sex)) +
  geom_bar(position="fill") +
  labs(title='postion="fill"')

install.packages("lattice")
library(lattice)
data(singer, package = "lattice")
ggplot(data=singer, aes(x=height)) +
  geom_histogram() +
  facet_wrap(~voice.part, nrow=4)

ggplot(data=singer, 
       aes(x=height,
           fill=voice.part)) +
  geom_density() +
  facet_grid(voice.part~.)


ggplot(data=Salaries,
       aes(x=yrs.since.phd,
           y=salary,
           color=rank,
           shape=rank)) +
  geom_point() +
  facet_grid(.~sex)
