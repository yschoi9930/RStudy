# R Studio 사용

5+8
3+(4*5)
a <- 10
print(a)
a

help(q)

# 산술연산자
3 + 2
3 - 2
3 * 2
3 / 2
3 ^ 2
3 ** 2
10 %% 2
10 %/% 2

# 논리연산자
5 < 5
5 <= 5
5 > 5
5 >= 5
5 == 5
5 != 5
!TRUE  # T F
TRUE | FALSE
TRUE & FALSE

10 >3 & 10 <3
10 >3 | 10 <3
!(10 >3)
!(10 <3)

# 변수 만들고 대입
x <- 3
x <- c(1,2,3)

# 변수 x의 값 사용
x

# 변수의 값 바꾸기
x <- 5
x

x <- NULL
x

x <-7

# 변수 사용하기
y <- 3
temp <- y
temp; y

y <- x
y
x
x <- temp
x

x; y


v <- ls()
v
rm(temp)
temp

ls()
rm(a,x,y)
rm(list=ls())
help(rm)
?rm


getwd()
setwd('c:\\RStudy')


cat("I love you!")
sink("output.txt")
cat("I love you\n")
sink()


help(head)
?tail

# 변수의 초기화
x
x <- NULL
x <- 'fruit'
x <- 125
x <- TRUE
y <- c(1,3,5)
z <- 111

head(iris)
tail(iris)
summary(iris)

# 기본자료형

x <- 3..
y <- 2
x/y
xi <- 1+2i
yi <- 1-2i
xi + yi
st <- "string"
typeof(x)
typeof(xi)
typeof(st)
typeof(TRUE)

#######################################
# 데이터구조 : 벡터(vector)
#######################################

1:100
100:1

c(1:100, 200:250)
c(1,2,10, c(2,3,5))
seq(20, 30, 2)
help(seq)
seq(from=20, to=100, by=30)
seq(to=300, from=20, by=30)
seq(20, 100, by=5)
seq(20, 25, length.out = 50)


x <- seq(from=20, to=100, by=30)
x
typeof(x)
f <- c('apple', 'banana', 'melon')
f
typeof(f)
rep(x, 3)
rep(x, c(1,2,3))
rep(x, times=2)

x1 <- rep(x, c(3,2,1))

x1
length(x1)
str(x)
str(x1)

str(f)
mode(f)
mode(x)
f2 <- c(TRUE, TRUE, FALSE)
f2
typeof(f2)
mode(f2)
str(f2)

# 벡터 내 원소 접근하기

x <- 5:1
x
x[1]
x[1,2,3]
x[c(1,3)]
x[-c(1,3)]


c(1,2,3) 
1:3

length(x)
x[2:length(x)]

y<-c(1,3,7, NA, 12)
y<10
y[y<10]
y[y%%2==0]
y[is.na(y)]
y[y %% 2 == !is.na(y)]

# 벡터 이름 반환 및 부여
x <- c(1,3,4)
x
names(x) <- NULL
names(x) <- c("kim", "seo", "lee")
x
x['seo']
names(x)[2]


# 벡터 연산

identical(c(1,2,3), c(1,2,4))  # 두 벡터가 동일한지 판단

"a" %in% c("a", "b")    # 벡터에 포함되어 있는 값 판단 : %in%

x <- c(2, -10, 100, 6, 35)                  # x의 값을 확인 
mean(x)            # 평균 
order(x)           # 오름차순으로 정렬될 자료의 인덱스를 나열 
rev(x)             # 역순으로 나열
range(x)           # 범위
sd(x)              # 표준편차 
sort(x)            # 오름차순으로 정렬 
sort(x, decreasing=TRUE) # 내림차순으로 정렬 
length(x)          


# 벡터 자료 일부 변경
x <- c(1,4,6,8,9)
x
y <- replace(x, c(2,4), c(32,24))
y 
w <- append(x,y) 
w 
z <- append(x,y, after=2)
z 

# 벡터형 자료 연산 기능
c(1,2)+c(4,5)      
c(1,2,3)*10
x <-c(1,2,3) 
y <-c(4,2,8) 
x==y


# 벡터 집합연산
x <-c(sort(sample(1:99, 9)))  # 1~99까지 9개 추출하고 정렬 
x
y <-c(sample(3:60, 7))        # 3에서 60까지 7개 추출  
y
c(x, y)
union(x,y)            # x와 y를 합집합 
intersect(x,y)        # x와 y의 교집합
setdiff(x,y)          


# 문자형 자료를 갖는 벡터의 연산
xx <- c('a','b','c','d','e')
x <- paste(xx[1], xx[2]) ; x

paste("I love ", "you and ", "you love ", "me!", sep="")

substring("abcdefghijklmn", 2)

substring("abcdefghijklmn", c(2,3,4,5))
# 2:5 -> 2:, 3:, 4:, 5:

name <- c("Jungwom","University","Computer",
          "Science","Major", "Communication")
grep('Co', name)
grep('om',name)

grep("[a-z]", letters)



# 논리형 자료를 갖는 벡터의 연산
x <- runif(5)                  # 0~1 사이의 값을 5개 생성 
x
(0.4 <= x) & (x <=0.7)        # x가 0.4~ 0.7 사이에 있는가? 
any(x > 0.9)                   # x 중에 0.9 이상이 있는가? 
all(x < 0.9)                 # x의 값이 모두 0.9 이하인가? 
is.vector(x)         


###########################################
#  데이터구조 : 팩터(factor)
###########################################

# 1차원 데이터 구조 : 질적자료

help(factor)


# data type

f <- append(f, c("melon", "apple", 
                 "banana", "berry"))
# unique(f)
str(f)
mode(f)

x <- 1:5

x <- c(1,2,3,1,1,4)
x <- factor(x, levels=c(1,2,3,4), 
       labels = c('apple','banana',
                  'melon','berry'))
x <- ordered(x)
x
str(x)

std.name <- c('kim', 'lee', 'kimsw', 'choi')
sex <- c('M', 'F', 'F', 'M')
age <- c(15, 17, 21, 18)

sex <- factor(sex, levels = c('M', 'F'))



###########################################
#  데이터구조 : 행렬(matrix)
###########################################

v1 <- c(1,2,3)
v2 <- c(4,5,6)
v3 <- c(7,8,9)

mat1 <- rbind(v1, v2, v3)
mat2 <- cbind(v1, v2, v3)
mat2

d <- 1:9
mat3 <- matrix(d, nrow=3, byrow = T)
mat3   

mat3*3			
mat3*c(10, 20, 30)

mat4 <- matrix(1:12, nrow=3,
               dimnames=list(c("R1","R2","R3"),
                             c("C1","C2","C3", "C4")))
mat4
colnames(mat4) <- c("A","B","C","D")
rownames(mat4)

# 행렬에서 원소 접근
mat4[7]       
mat4[1, ]      
mat4[,3]
mat4[3,4]
mat4[, 2:4]
mat4[, -c(2,3,4)]
mat4[,-2]

#
mat1
apply(mat1, 1, min)
apply(mat1, 2, max)

colnames(mat1) <-c("A", "B", "C")
rownames(mat1) <-c("Kim", "Lee", "Seo")
mat1


matrix(1:10, ncol=2)

A <- matrix(1:4, nrow=2)
A

A %*% A
A %*% solve(A)

nrow(A)
ncol(A)
dim(A)

mat4[1,3]
mat4['R1','A']

sex <- c('F','M','M','M','F')
age <- c(21, 20, 25, 30, 26)
name <- c('철수', '길동', '관순', '감찬', '영희')

student <- data.frame(sex, age, name)
student
str(student)
student$sex <- factor(student$sex)
student[,1]
student$name[2]
View(student)
student <- edit(student)
student

colnames(student)
rownames(student)

mat4
colSums(mat4)
rowSums(mat4)
colMeans(mat4)
rowMeans(mat4)

subset()

ls()
rm(list=ls())

#############################
## 데이터구조 : 리스트(list)
#############################

h.list <- c('balling','tennis','ski')
person <- list(name='Kim', age=25, student=TRUE, hobby=h.list)

person
person[1:3]
person[[1]]
person$hobby[2]

person$birth <- "1995-10-23"
person


## 카페 매출액 분석 예제
cafe <- list(espresso = c(4, 5, 3, 6, 5, 4, 7),
             americano = c(63, 68, 64, 68, 72, 89, 94),
             latte = c(61, 70, 59, 71, 71, 92, 88),
             price = c(2.0, 2.5, 3.0),
             menu = c('espresso','americano','latte'))
cafe$price

cafe$menu <- factor(cafe$menu)
cafe$menu
str(cafe)
cafe$price
names(cafe$price) <- cafe$menu

sale.espresso <- cafe$price['espresso'] * cafe$espresso
sale.americano <- cafe$price['americano'] * cafe$americano
sale.latte <- cafe$price['latte'] * cafe$latte

# 요일별 매출액

sale.day <- sale.espresso + sale.americano + sale.latte 
sale.day
names(sale.day) <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')

# 총 매출액
sum(sale.day)

# 평균 매출액
sale.mean <- mean(sale.day)
sale.mean

# 평균 매출액 이상인 요일 추출
sale.day[sale.day >= sale.mean]

names(sale.day[sale.day >= sale.mean] ) 	


