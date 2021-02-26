# Binominal Distribution
Binom<- data.frame(Successes=rbinom(n=10000, size=5,prob=0.3), Size=5)
library(ggplot2)
ggplot(Binom, aes(x=Successes)) + geom_histogram(bandwidth=1)
head(Binom)

Binom10<- data.frame(Successes=rbinom(n=10000, size=10,prob=0.3), Size=5)
Binom100<- data.frame(Successes=rbinom(n=10000, size=100,prob=0.3), Size=5)
Binom1000<- data.frame(Successes=rbinom(n=10000, size=1000,prob=0.3), Size=5)

hist(Binom$Successes)
hist(Binom10$Successes)
hist(Binom100$Successes)
hist(Binom1000$Successes)

par(mfrow=c(2,2))

Successes=rbinom(n=100, size=5,prob=0.7); Successes

?rbinom

Binom7<- data.frame(Successes=rbinom(n=10000, size=5,prob=0.7), Size=5); Binom7
Binom7.1<- data.frame(Successes=rbinom(n=10000, size=100,prob=0.3), Size=100)
Binom7.2<- data.frame(Successes=rbinom(n=10000, size=1000,prob=0.7), Size=1000)
Binom7.3<- data.frame(Successes=rbinom(n=10000, size=10000,prob=0.7), Size=10000)


mean(Binom7$Successes)
mean(Binom7.1$Successes)
mean(Binom7.2$Successes)
(Binom7.3$Successes)

head(Binom7.1$Successes)

Binom7
hist(Binom7$Successes)
hist(Binom7.1$Successes)
hist(Binom7.2$Successes)
hist(Binom7.3$Successes)
par(mfrow=c(2,2))

Binom8<- data.frame(Successes=rbinom(n=10000, size=100,prob=0.5), Size=100)
hist(Binom8$Successes)

rbinom(n=10, p=0.4, size=5)


pois1<-rpois(n=1000,lambda=1)
pois2<-rpois(n=1000,lambda=3)
pois3<-rpois(n=1000,lambda=5)
pois4<-rpois(n=1000,lambda=10)
pois5<-rpois(n=1000,lambda=20)
pois6<-rpois(n=1000,lambda=30)
pois7<-rpois(n=1000,lambda=40)
pois8<-rpois(n=1000,lambda=50)



hist(pois1)
hist(pois2)
hist(pois3)
hist(pois4)
hist(pois5)
hist(pois6)
hist(pois7)
hist(pois8)

####Normal Distribution
rnorm(n=10)
rnorm(n=1000, mean=10, sd=5)
randNorm10<-rnorm(10)
dnorm(randNorm10)
pnorm(randNorm10)

no1<-rnorm(n=1000, mean=10, sd=5)
no2<-rnorm(n=1000, mean=10, sd=10)
no3<-rnorm(n=1000, mean=5, sd=5)
no4<-rnorm(n=1000, mean=5, sd=10)
hist(no1)
hist(no2)
hist(no3)
hist(no4)



n <- 10 # 표본의 크기
sid<- 1:100 # 표본 100개
d<- seq(-3,3,by=0.01); d

data<-matrix(rnorm(n=100), ncol=n)
head(data)
data

xbar<-apply(data,1,mean)
xbar

se<-1/sqrt(n); se
alp<-0.05
z<-qnorm(1-alp/2); z



lowB <- xbar -z*se ; lowB
uppB <- xbar +z*se ; uppB

CL <- cbind(lowB,uppB) ;CL
sum(lowB*uppB>0)
CL[lowB*uppB>0]

xtick<-seq(-3,3,by=0.01) ; xtick
par(mfrow=c(1,1))
plot(xtick, type='n', xlab='표본추출', ylab='z',
     xlim=c(1,100), ylim=c(-1.5,1.5), cex.lab=1.8)

abline(h=0, col='red', lwd=2, lty=2)

l.c <- rep(NA, length(sid)); l.c
l.c <- ifelse(lowB*uppB >0, 'red', 'black')
l.c
arrows(1:length(sid), lowB, 1:length(sid), uppB, code =3,
       angle=90, length=0.02, col=l.c, lwd =1.5)

### 모평균의 구간추정 함수 작성 : t분포의 값을 이용 ###

CI.t <-function(x, alpha=0.05) {
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  t <- qt(1-alpha/2, n-1)   # t분포의 확률 t(n-1, alpha/2) -> 양수값 t 값만
  se <-s/sqrt(n)
  low <- m - t*se
  upper <- m +t*se
  ci <- c(1-alpha, low, m, upper)
  names(ci) <- c("Confidence.level", "lower", "mean", "upper" )
  return(ci)
}

CI.t(rt(100,9))


# 1단계 데이터 탐색 -> 분포, 이상치, 결측치 확인
A<-c(12.60,15.15,17.62,16.81,15.51,15.12,14.39,15.20,13.70,14.75,15.13,15.66,
     13.69, 15.74, 14.96,15.20,16.45,13.66,16.16,14.47)
B<-c(13.77,13.63,12.63,14.13,13.50,13.09,13.96,13.41,14.03,14.25,13.47,13.43,
     13.24,14.61,13.82,14.07,15.96,13.69,14.25,14.50)

box<-boxplot(A,B, names=c("car A","car B"))
summary(A)
summary(B)

box
box$stats
box$out

A.cor <- A[A!=box$out[1]]
B.cor <- B[B!=box$out[2]]

boxplot(A.cor, B.cor, names=c("car A","car B"))

# 2단계 정규성 검정 - 샤피로 윌크 테스트
# 귀무가설 : 정규분포를 이룬다

shapiro.test(A.cor) 
shapiro.test(B.cor)

# 둘다 p값이 0.05보다 커서 귀무가설 채택

# 3단계 t 검정 수행
# 귀무가설 : mu(A) = mu(B)
# 대립가설 : mu(A) > mu(B)
# t.test : 두 집단의 평균 비교
t.test(A.cor, B.cor, alternative = 'greater')

# 모분산을 모르는 경우

# 등분산성 검정 : var.test()
# 귀무가설 : var(A)=var(B)

var.test(A.cor, B.cor)

# p-value < 0.05 :귀무가설 기각, 둘의 분산이 다름


# 결론
# 1. 데이터 분포 확인 -> 시각화, 기술통계를 통해 이상치 결측치 확인
# 2. 정규성 검정 : shpiro.test()
# 3. 등분산성 검정(두 모집단의 분산이 동일): var.test()
# 4. t-test 진행

t.test(x, y = NULL,
       alternative = c("two.sided", "less", "greater"),
       var.equal = T, conf.level = 0.95, ...)

# 1)
x <- c(68.86, 74.64, 75.80, 76.41, 67.37, 79.98, 74.95, 76.36,
       72.39, 76.73, 75.56, 73.59, 78.49, 69.49, 72.38, 73.38, 
       77.29, 80.43, 73.94, 73.35)
y <- c(65.92, 64.49, 68.24, 74.38, 70.82, 61.30, 73.09, 71.20,
       74.31, 73.14, 68.76, 71.12, 65.62, 72.11, 72.20, 76.57, 
       65.26, 73.82, 71.42, 71.93)

boxplot(x,y)
summary(x)
summary(y)

shapiro.test(x)
shapiro.test(y)

var.test(x,y)


t.test(x,y, alternative = "greater",var.equal = T, conf.level = 0.95)
# p-value < 0.05 : 귀무가설 기각 
# x data의 평균이 y data의 평균보다 크다

temp.A <- c(75.23, 71.74, 79.86, 87.70, 85.13, 80.98, 79.75, 77.42,
            80.00, 75.23, 78.59, 79.78, 81.49, 75.20, 81.73, 79.26,
            80.00, 83.98, 75.12, 83.07)
temp.B <- c(75.43, 70.02, 90.93, 76.95, 79.44, 77.49, 61.68, 84.93,
            67.89, 68.17, 87.55, 68.05, 68.21, 91.73, 85.00, 73.84,
            84.93, 52.33, 73.64, 59.69)

