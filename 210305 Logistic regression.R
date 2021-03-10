install.packages("AER")
data("Affairs", package="AER")
View(Affairs)

summary(Affairs)
table(Affairs$affairs)
barplot(table(Affairs$affairs))
str(Affairs)

str(Affairs)

barplot(Affairs$affairs)

Affairs$ynaffairs[Affairs$affairs>0] <-1
Affairs$ynaffairs[Affairs$affairs==0] <- 0
table(Affairs$ynaffairs)
str(Affairs)

Affairs$ynaffairs <- factor(Affairs$ynaffairs,
                            level=c(0,1),
                            labels=c("No","yes"))

psych::pairs.panels(Affairs[,1:5])
Affairs <- Affairs[,c(1:10)]
table(Affairs$ynaffairs)
colnames(Affairs)

head(Affairs)

fit.full <- glm(ynaffairs ~ gender + age + yearsmarried
                + children + religiousness + education
                +occupation + rating,
                data = Affairs,
                family = binomial(link="logit"))
summary(fit.full)

fit.reduced <-glm(ynaffairs ~  age + yearsmarried
                  + religiousness + rating,
                  data = Affairs,
                  family = binomial(link="logit"))
summary(fit.reduced)

anova(fit.reduced, fit.full, test="Chisq")
coef(fit.reduced)
exp(coef(fit.reduced))

# rating effects

testdata <- data.frame(rating =c(1,2,3,4,5),
                 age=mean(Affairs$age),
                 yearsmarried=mean(Affairs$yearsmarried),
                 religiousness=mean(Affairs$religiousness))

testdata$prob <- predict(fit.reduced, newdata=testdata,
                         type="response")

# age effects
testdata <- data.frame(rating =mean(Affairs$rating),
                       age=seq(17,57,10),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))

testdata

testdata$prob <- predict(fit.reduced, newdata=testdata,
                         type="response")

#yearsmarried effects
testdata <- data.frame(rating =mean(Affairs$rating),
                       age=mean(Affairs$age),
                       yearsmarried=seq(1,15,3),
                       religiousness=mean(Affairs$religiousness))
testdata

testdata$prob <- predict(fit.reduced, newdata=testdata,
                         type="response")

#religiousness effects
testdata <- data.frame(rating =mean(Affairs$rating),
                       age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=seq(1,5))
testdata

testdata$prob <- predict(fit.reduced, newdata=testdata,
                         type="response")


# 과대산포 검정
# 방법 1

deviance(fit.reduced) / df.residual(fit.reduced)

# 실습예제

mydata <- read.csv("http://stats.idre.ucla.edu/stat/data/binary.csv")
head(mydata)
summary(mydata)
str(mydata)

mydata$admit_f <- factor(mydata$admit, labels=c("no","yes"))
head(mydata)
table(mydata$admit_f)
psych::pairs.panels(mydata[,1:4])

fit.full <- glm(admit_f ~ gre + gpa + rank, data = mydata,
                family = binomial(link="logit"))
summary(fit.full)

coef(fit.full)

exp(coef(fit.full))

# rank effects

summary(mydata)
test.data <- data.frame(rank=c(1,2,3,4),
                        gre = mean(mydata$gre),
                        gpa=mean(mydata$gpa))
test.data

test.data$prob <- predict(fit.full, newdata=test.data,
                         type="response")

test.data

# gpa effects

test.data <- data.frame(rank=mean(mydata$rank),
                        gre =mean(mydata$gre) ,
                        gpa=c(2,3,4))
test.data

test.data$prob <- predict(fit.full, newdata=test.data,
                          type="response")

test.data

# gre effects

test.data <- data.frame(rank=mean(mydata$rank),
                        gre =seq(250,750,100) ,
                        gpa=mean(mydata$gpa))
test.data

test.data$prob <- predict(fit.full, newdata=test.data,
                          type="response")

test.data

# 과대산포
# 방법1 : 1에 가까우면 과대산포 아님
deviance(fit.full)/df.residual(fit.full)
# 1에 가까움

install.packages("lattice")
library(lattice)

# xyplot
head(airquality)
xyplot(Ozone ~ Wind, data=airquality)

# bwplot
attach(airquality)
plot(Wind, Ozone)
detach(airquality)
bwplot(voice.part ~ height, data=singer,
       xlab="Height(inches)") # 점은 평균점

# histogram
histogram(~ height | voice.part,
          data= singer)

histogram(~ height | voice.part,
          data= singer,
          layout=c(4,2), endpoints = c(59.5,76.5), nint=17)

# strip plot
stripplot(voice.part ~ height, data=singer, jitter.data=T)
stripplot(voice.part ~ jitter(height), data=singer, jitter.data=T) 
# jitter :중복 데이터를 따로 보이게

# dotplot
dotplot(variety ~ yield | year*site, data=barley)


########## ggplot ############

# 객체 지정 방법
s <- ggplot(data=iris, aes(Sepal.Length, Petal.Length, col=Species)) ;s
s + geom_point()

p <- ggplot(mtcars, aes(wt, mpg, col=cyl))
p <- p +geom_point()
p

p <- ggplot(mtcars, aes(factor(cyl), fill=factor(cyl))) ;p
p <- p +geom_bar(width=5)
p <- p + facet_grid(gear~.) # .에따라 열단위 행단위 분리
p



