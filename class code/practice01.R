" 
1. 다음의 내용을 수행하는 코드를 작성하시오.
  1) 1부터 100까지 정수로 구성된 벡터 d를 생성

  2) 벡터 d에서 홀수만 추출하여 벡터 odd에 저장
  3) 벡터 odd에서 끝에 10개를 제외하고 출력
  4) odd 에서 3번째, 7번째, 32번째 값을 추출하여 pick 벡터에 저장
  5) pick 벡터에 이름 속성을 '3rd', '7th', '32th'로 설정
  6) pick 벡터에서 이름이 '7th'인 값을 제외하고 출력
  7) pick 벡터에서 이름이 '3rd'인 값을 3으로 변경
"
d <- 1:100
odd <- d[d %% 2 == 1]
odd[-c(41:50)]
pick <- odd[c(3,7,32)]
names(pick) <- c('3rd', '7th', '32th')
pick[c('3rd', '32th')]
pick['3rd'] <- 3


# 2. 1~5000까지의 자연수 합을 구하는 명령문 작성하시오.
sum(1:5000)


# 3. 1~100 사이의 짝수로 구성된 벡터 even을 생성하고, 
#    even의 합계를 출력하는 코드를 작성하시오.

x <- 1:100
even <- x[x %% 2== 0]
sum(even)


# 4. v1<-c('T','T','F','T')에서 sum(v1)의 실행 결과는?
v1<-c('T','T','F','T')
sum(v1)
  

#
# 교재 148쪽 연습문제
#

# 9. 다음은 지나가는 행인 10명으로부터 선호하는 계절을 조사한 결과이다.
#    이 결과를 팩터 타입의 favorite에 저장하고 저장값의 종류를 나타내시오.

favor1 <- c('여름', '봄', '여름', '겨울', '봄',
            '겨울', '여름', '여름', '봄', '가을')
favor1 <- factor(favor1)
favor1
levels(favor1)

favor2 <- factor(c('여름', '봄', '여름', '겨울', '봄',
                   '겨울', '여름', '여름', '봄', '가을'),
                 levels=c('봄','여름','가을','겨울'))
levels(favor2)

country <- c('호주','독일','영국','일본','미국','중국','호주','영국','중국','일본',
             '터키','미국','중국','중국','호주','터키','독일','일본','중국','독일')
country <- factor(country)
levels(country)
as.integer(country)


#
# 교재 194쪽~ 연습문제
# 

# 1번 데이터 프레임
# 2번 관측값(observation), 변수(variable)
# 3번 A=4, B=T, C=3, D=F

x <- c(2,4,6,8)
y <- c(10,12,14, 16)
z <- c(18,20,22,24)
m <- cbind(x,y,z)
m

matrix(c(2,10,18,4,12,20,6,14,22,8,16,24), nrow=4, byrow=T)
matrix(c(2,4,6,8,10,12,14,16,18,20,22,24), ncol=3, byrow=F)

# 4번 
  colnames(m)	<- c('a',	'b',	'c',	'd')
  rownames(m)	<- c('x',	'y')
  
# 5번
  #(1)
   m <- t(m)
  #(2)  
   df <- data.frame(m)
  #(3)
  df.new <- data.frame(m, info)
  #(4)
  str(df.new)
  class(df.new)
  
# 6번
  blood	<-	c('A',	'O',	'AB',	'B',	'B')
  rh	<-	c('+',	'+',	'-',	'+',	'+')
  age	<-	c(21,	30,	43,	17,	26)
  
  blood != 'B'
  #(1)
  df	<-	data.frame(blood,	rh,	age)
  df
  #(2)
  colnames(df)
  #(3)
  df.nb	<- subset(df,	blood	!=	'B')
  df.nb  
  
# 7번
  cars
  str(cars)  
  class(cars)  
  dim(cars)  

# 8번 :  (1)	T	(2)	T	(3)	F	(4)	T	(5)	F
matrix(1:12,	nrow=3)	%%	3
matrix(1:12,	nrow=3)	+	3
matrix(1:12,	nrow=3)	+	matrix(1:12,	nrow	=	4)
matrix(1:12,	nrow=3)	+	matrix(1:12,	nrow	=	3)
rbind(matrix(1:9,	nrow	=	3),	c('1',	'2',	'3'))
rbind(matrix(1:9,	nrow	=	3),	c('1',	'2',	'3')) + 3

#9번
 #(1)
번호	<-	1:7
제목	<-	c('그대랑',	'다툼',	'빨래',	'두통',	'보조개',	'매듭',	'이상해')
좋아요	<-	c(16075,	8218,	12119,	738,	3200,	16144,	5110)
love	<-	data.frame(번호, 제목,좋아요)
love 

#(2)
best	<-	subset(love,	좋아요	==	max(좋아요))
best


# 10번
kor <- c(97, 88, 100)
his <- c(100, 82, 96)
math <- c(83, 90, 76)
sci <- c(95, 91, 89)
eng <- c(92, 87, 95)
mid <- data.frame(kor, his, math, sci, eng)
mid

kor <- c(94, 98, 100)
his <- c(100, 82, 96)
math <- c(83, 90, 76)
sci <- c(95, 85, 89)
eng <- c(92, 87, 95)
final <- data.frame(kor, his, math, sci, eng)
final

(mid	+	final)	/	2



