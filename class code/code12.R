##################################
## Generalized linear model(GLM)
##################################

# 머플러 판매 데이터
# 순이익이 5만원 미만이면 0, 넘으면 1로 기록

muffler <- data.frame(discount=c(2, 4, 6, 8, 10),
                      profit=c(0, 0, 1, 1, 1))

Mfit <- lm(profit ~ discount, data=muffler)
plot(muffler, pch=20, cex=1, xlim=c(0,12))
abline(Mfit)

summary(Mfit)

newd <- data.frame(discount=c(1, 5, 12, 20, 30))

p <- predict(Mfit, newd)
print(p)
             
plot(muffler, xlim=c(0,32), ylim=c(-0.5, 4.2))        
abline(Mfit)
res <- data.frame(discount=newd, profit=p)
points(res, pch=20, cex=1, col='red')
legend('bottomright', pch=c(20,20),
       legend=c('train data', 'new data'), 
       col=c('black', 'red'))

### glm()적용
gFit <- glm(profit ~ discount, data=muffler, family=binomial)
summary(gFit)
plot(muffler, pch=20, cex=1)
abline(gFit, col='blue', lwd=2)
newd <- data.frame(discount=c(1, 5, 12, 20, 30))
p <- predict(gFit, newd, type = 'response')
print(p)

plot(muffler, xlim=c(0,32), ylim=c(-0.5, 1.5))        
abline(gFit, col='blue', lwd=2)
res <- data.frame(discount=newd, profit=p)
points(res, pch=20, cex=1, col='red')
legend('bottomright', pch=c(20,20),
       legend=c('train data', 'new data'), 
       col=c('black', 'red'))


#######################################################
# Harberman Survival Datasets

haberman <- read.csv('http://archive.ics.uci.edu/ml/machine-learning-databases/haberman/haberman.data', 
                     header = FALSE)
View(haberman)
names(haberman) <- c('age', 'op_year', 'no_nodes', 'survival')
str(haberman)
# survival 변수 범주형으로 변환
haberman$survival <- factor(haberman$survival)

h <- glm(survival ~ age + op_year + no_nodes, data=haberman,
         family = binomial)
coef(h)
deviance(h)

new.patients <- data.frame(age=c(37,66), op_year=c(58, 60),
                           no_nodes=c(5,32))
predict(h, newdata = new.patients, type='response')


h2 <- glm(survival ~ age + no_nodes, data=haberman,family = binomial)
coef(h2)
deviance(h2)

new.patients <- data.frame(age=c(37,66), no_nodes=c(5,32))
predict(h2, newdata = new.patients, type='response')


########################################################
install.packages("AER")
library(AER)

data("Affairs", package = "AER")
View(Affairs)
summary(Affairs)

table(Affairs$affairs)
prop.table(table(Affairs$affairs))
barplot(table(Affairs$affairs))
barplot(prop.table(table(Affairs$affairs)))

install.packages("psych")
library(psych)
pairs.panels(Affairs[,1:5])

Affairs$ynaffairs[Affairs$affairs > 0] <- 1
Affairs$ynaffairs[Affairs$affairs == 0] <- 0
table(Affairs$ynaffairs)

Affairs$ynaffairs <- factor(Affairs$ynaffairs, 
                            level=c(0, 1),
                            labels=c("No","Yes"))
table(Affairs$ynaffairs)
colnames(Affairs)

fit.full <- glm(ynaffairs ~ gender + age + yearsmarried
                + children + religiousness + education
                + occupation + rating, 
                data = Affairs,
                family = binomial(link="logit"))

summary(fit.full)

fit.reduced <- glm(ynaffairs ~ age + yearsmarried +
                     religiousness + rating,
                   data = Affairs,
                   family = binomial(link="logit"))
summary(fit.reduced)

anova(fit.reduced, fit.full, test="Chisq")

coef(fit.reduced)

exp(coef(fit.reduced))

# rating effects
testdata <- data.frame(rating=c(1,2,3,4,5),
                       age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata

testdata$prob <- predict(fit.reduced, newdata = testdata,
                         type = "response")
testdata

# age effects
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=seq(17, 57, 10),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=mean(Affairs$religiousness))
testdata

testdata$prob <- predict(fit.reduced, newdata = testdata,
                         type = "response")
testdata

# yearsmarried effects
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=mean(Affairs$age),
                       yearsmarried=seq(1, 15, 3),
                       religiousness=mean(Affairs$religiousness))
testdata

testdata$prob <- predict(fit.reduced,newdata = testdata,
                         type = "response")
testdata


# religiousness effects
testdata <- data.frame(rating=mean(Affairs$rating),
                       age=mean(Affairs$age),
                       yearsmarried=mean(Affairs$yearsmarried),
                       religiousness=seq(1, 5))

testdata$prob <- predict(fit.reduced, newdata = testdata,
                         type = "response")
testdata


## 과대산포 검정

#방법1
deviance(fit.reduced) / df.residual(fit.reduced)

#방법2

fit <- glm(ynaffairs ~ age + yearsmarried +
             religiousness + rating,
           data = Affairs, family = binomial()) 

fit.od <- glm(ynaffairs ~ age + yearsmarried
              + religiousness + rating,
           data = Affairs, family = quasibinomial()) 

fit$df.residual
summary(fit.od)$dispersion
q <- summary(fit.od)$dispersion * fit$df.residual
q

pchisq(summary(fit.od)$dispersion * fit$df.residual, 
       fit$df.residual, lower=F)

help(glm)


#######################################################
# 다중 로지스틱 회귀분석(mulitinomial Logistic Regression)
# dataset 출처: http://3months.tistory.com/236


install.packages("foreign")
library(foreign)
help(read.dta)
ml <- read.dta("https://stats.idre.ucla.edu/stat/data/hsbdemo.dta")

head(ml)
str(ml)
View(ml)
with(ml, table(ses, prog)) 

# write변수 범주에 따른 prog변수의 평균과 표준편차 계산
with(ml, do.call(rbind, 
                 tapply(write, prog, 
                        function(x) c(M = mean(x), SD = sd(x)))))

install.packages("nnet")
library(nnet)

ml$prog2 <- relevel(ml$prog, ref="academic")
ml$prog2

mfit <- multinom(prog2 ~ ses + write, data = ml)
summary(mfit)


##################################################
## 포아송 회귀분석(Poisson Regression)

install.packages("robust")
library(robust)
data(breslow.dat, package = "robust" )
View(breslow.dat)


summary(breslow.dat[, c(6,7,8,10)])

opar <- par(no.readonly=TRUE)
par(mfrow=c(1,2))
attach(breslow.dat)
hist(sumY, breaks = 20, xlab = "Seizure Count",
     main="Distribution of Seizures")
boxplot(sumY ~ Trt, xlab = "Treatment", 
        main ="Group Comparisons")
par(opar)
detach(breslow.dat)

fit <- glm(sumY ~ Base + Age + Trt, data = breslow.dat,
           family = poisson())
summary(fit)

coef(fit)

exp(coef(fit))

deviance(fit)/ df.residual(fit)

install.packages("qcc")
library(qcc)
help(qcc.overdispersion.test)
qcc.overdispersion.test(breslow.dat$sumY, type = "poisson")

fit.od <- glm(sumY ~ Base + Age + Trt, data = breslow.dat,
              family = quasipoisson())
summary(fit.od)

fit <- glm(sumY ~ Base + Age + Trt, data = breslow.dat,
           family = poisson(), offset = log(time))
