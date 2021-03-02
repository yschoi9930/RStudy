right <- c(41.11, 38.35, 38.57, 42.54, 37.46, 40.13, 43.05,36.00,40.45,
           36.69,45.38,38.42,37.39,38.30,33.33)
left <- c(30.37,25.90,29.60,34.22,33.46,41.91,30.80,31.74,
          34.28,31.13,31.24,30.63,29.99,33.20,33.32)
di <- right - left

boxplot(di, horizontal = T)

shapiro.test(di)

# 대응 표본  t test 방법
t.test(right, left, paired =T, alternative = "two.sided")

# 일표본 t test 방법
t.test(di, mu=0, paired = F, alternative = "two.sided")

data <- matrix(c(59,16,6,80), nrow=2, dimnames=list(before=c("Positive","Negative"),
                                                    after=c("Positive","Negative")))
data

mcnemar.test(data, correct =F)

# 두 모비율 비교
beans <- matrix(c(160,40,175,125), nrow =2, byrow =T)
rownames(beans)<- c("region I", "region II")
colnames(beans)<-c("coffee_beans A", "coffee_beans B")
beans
bean_prop <- prop.table(beans, margin =1)
bean_prop
barplot(bean_prop, col=c("red", "blue"), beside=T)
legend(locator(1), c("region I", "region II"), fill=c("red","blue"))

prop_test <- prop.test(beans, alternative = "two.sided", correct = F)
prop_test

prop_test <- prop.test(beans, alternative = "two.sided", correct = T)
prop_test

prop_test$estimate
prop_test$p.value
prop_test

x <- c(46.6, 48.9, 48.3, 49.2, 51.6, 52.4, 56.7, 49.7, 
       48.7, 55.8, 56.8, 50.1, 48.6, 42.7, 55.6)
y <- c(54.9, 42.5, 51.0, 54.0, 49.8, 48.8, 45.3, 62.3,
       54.5, 60.9, 50.4, 46.2, 40.9, 46.1, 51.3)

boxplot(x, y, names = c("process I", "process II"))

shapiro.test(x)
shapiro.test(y)
var.test(x,y,alternative = "two.sided", ratio=1)

x1 <- c(x,y) ; x1
group <- rep(1:2,c(15,15)) ; group
var.test(x,group,alternative = "two.sided", ratio=1)
boxplot(x, group, names = c("process I", "process II"))

x<-rnorm(1000,mean=10, sd=1)
qqnorm(x)

x<-runif(1000)
qqnorm(x)
qqline(x)

qqplot(runif(1000, min=1, max=10), 1:10)

library(MASS)
head(UScrime)
with(UScrime, by(Prob, So, median)
with(UScrime)

install.packages("UsingR")
library(UsingR)
data(father.son)
head(father.son)
boxplot(father.son)
shapiro.test(father.son$fheight)
shapiro.test(father.son$sheight)
var.test(father.son$fheight, father.son$sheight)
ansari.test(father.son$fheight, father.son$sheight)
t.test(father.son$fheight, father.son$sheight, var.equal = T)

install.packages("lawstat")
library(lawstat)


x<- rnorm(1000,0,1)
hist(x, freq=F)
lines(density(x), col='red', lew=3)

x1 <- c(22,19,26,25,17,11)
p1 <- rep(1/6, 6)
chisq.test(x1, p=p1)

x<-matrix(c(26,10,20,15,11,18), nrow=3, ncol=2, byrow=T)
dimnames(x)<-list(c("만족","보통","불만족"), c("남","여"))
x
chisq.test(x, correct=F)

credit <- read.csv("credit.csv", header=T)
head(credit)
str(credit)
credit$Age <- as.factor(credit$Age)

freq_Age <-xtabs(~Age, data=credit)
freq_Age

cross.Income_Credit <- xtabs(~Income+Credit, data=credit)
cross.Income_Credit

chi_test_inde <-chisq.test(credit$Income, credit$Credit)
chi_test_inde

chi_test_inde2 <- chisq.test(cross.Income_Credit) 
chi_test_inde2

install.packages("vcd")
library(vcd)

head(Arthritis)
mytable <- xtabs(~Treatment+Improved, data=Arthritis)
mytable
prop.tabel(mytable)
ch <- chisq.test(mytable,correct = F)
ch
ch$expected

# Fisher Test
fisher.test(mytable)

library(MASS)
View(survey)
summary(survey)
mytable <- xtabs( ~W.Hnd +Clap, data=survey)
mytable
ch<-chisq.test(mytable);ch
ch$expected
head(ch)

fisher.test(mytable)

install.packages('gmodels')
library(gmodels)
CrossTable(survey$W.Hnd, survey$Clap, fisher=T)

# CHM Test
mytable <- xtabs(~Treatment+Improved, data=Arthritis) ;mytable
assocstats(mytable)

?addmargin
options(digits=3)
# 교차분석 - 실습문제 1
line<-matrix(c(62,40,37,61,35,72,37,76,28,71,57,24), ncol=4, byrow=T)
dimnames(line) <- list(c("I","II","III"),c("A","B","C","D"))
addmargins(line)
line
prop_line <- prop.table(line, margin=2); prop_line
prop_line2 <- prop.table(line, margin=1); prop_line2
str(line)
barplot(line)
barplot(prop_line,beside=T, legend=c("I","II","III"))
summary(line)
ch<-chisq.test(line); ch
ch$expected
# 가설 : 공정라인에 따라 결함유형의 차이가 없다.(동일하다) - 기각

# 교차분석 - 실습문제 2
N2<-matrix(c(11000,9400,700,130,30,40,30,5,1,1), ncol=2)
dimnames(N2) <- list(c("1","2","3","4","5"),c("I","II"))
N2
ch<-chisq.test(N2)
ch$expected
fisher.test(N2)

# t test - 실습문제 1
A<-c(91,115,96,90,120,108,82,118,105,97,113,119,
     90,106,116,92,108,115,114,101,96,96,96,97,89,99,90,85,
     91,124,93,90,100,100,91,96,120,78,96,114)
B<-c(102,117,82,104,77,110,93,115,75,103,126,79,81,118,93,106,
     104,97,115,80,78,109,116,104,102,137,99,100,113,112,96,106,
     76,102,111,105,85,125,77,111)

summary(A)
summary(B)
boxplot(A,B)

shapiro.test(A)
shapiro.test(B)
# A는 정규분포를 따르지 않고, B는 정규분포를 따름
# -> 등분산성 검정이 필요

var.test(A,B)
# 귀무가설 채택 : 등분산성 성립


n.A<-length(A)
n.B<-length(B)
m.A<-mean(A)
m.B<-mean(B)
sd_A<-sd(A)
sd_B<-sd(B)
se<-sqrt(((sd_A)^2/n.A)+((sd_B)^2/n.B))
alpha<-0.05
z<-qnorm(1-(alpha/2)); z

diff.mean<- m.A - m.B

upper<-diff.mean + z*se
lower<-diff.mean - z*se
cat(lower, "< m <", upper)

t<-t.test(A,B, alternative = "greater")
# 귀무가설 채택, 모평균이 같다고 볼 수 있다.

mean_z.test_two <- function(x, y, sigma1, sigma2, conf.level, alternative){
  n <- length(x)
  m <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  diff <- mean_x - mean_y
  z_alpha_half <- qnorm((1-conf.level)/2, lower.tail=FALSE)
  var_mean <- (sigma1^2/n)+(sigma2^2/m)
  lower_limit <- diff - z_alpha_half * sqrt(var_mean)
  upper_limit <- diff + z_alpha_half * sqrt(var_mean)
  z_statistic <- diff /sqrt(var_mean)
  p_value_R <- pnorm(z_statistic, lower.tail = FALSE)
  p_value_L <- pnorm(z_statistic, lower.tail = TRUE)
  p_value_two <- pnorm(abs(z_statistic), lower.tail = FALSE)
  
  cat("Two sample Z-test", "\n")
  if(alternative == "two_sided"){
    p_value <- 2*p_value_two
  }else if(alternative =="greater"){
    p_value <- p_value_R
  } else {
    p_value <- p_value_L
  }
  result <- ifelse(p_value<(1-conf.level), "Reject H0", "Accept H0")
  cat("Z_statistics=", z_statistic, "\n")
  cat("p_value=", p_value, "\n")
  
  cat("alternative hyphothesis: ")
  
  if(alternative=="two_sided"){
    cat("true difference in mean is not equal to 0", "\n")
  } else if(alternative =="greater"){
    cat("true difference in mean is greater than 0", "\n")
  } else{
    cat("true difference in mean is less than 0", "\n")
  }
  cat(conf.level *100, "% confidence interval: (", lower_limit, ", ", 
      upper_limit, "\n")
  cat("sample mean of x= ", mean_x, ", sample mean of y=", mean_y, "\n")
  cat("sample estimate difference of mean=", mean_x - mean_y, "\n")
  cat("result = ", result)
}

# t test 실습문제 2
before<-c(71,72,66,69,69,69,70,67,72,67,71,72,69,69,70)
after<-c(69,67,68,68,70,67,67,64,65,64,66,70,70,67,66)
data<-data.frame(before, after); data
str(data)

boxplot(data, col=c("blue","red"))

shapiro.test(data$before)
shapiro.test()
