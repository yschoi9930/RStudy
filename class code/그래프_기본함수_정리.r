# R그래프 관련 기본 함수 - 수업했던 내용이어도 다시 한번 리뷰함

# R 스튜디오 Plots 창
plot.new()

# 단독 PLOT 창
dev.new()

# 옵션 없이 x, y축만 있는 기본 그래프
# plot() : 산점도 (기본)
plot(1:15)
plot(1:5, c(5,3,2,6,10))

# plot() 함수의 주요 옵션
# xlab : x축 이름
# ylab : y축 이름
# xlim : x축 값의 범위
# ylim : y축 값의 범위
# main : 그래프 제목
# pch : 점의 종류
# cex : 점의 크기
# col : 색상
# type : 그래프 유형


x <- c(1, 2, 3)
y <- c(4, 5, 6)
# 기본 옵션
plot(x, y,
     xlab="x축 이름",
     ylab="y축 이름",
     main="제목")

# 점의 크기 / 색상
plot(x, y, cex=10, col='red')

# 축 생략
v1 <- c(100, 130, 120, 160, 150)

# x, y축 생략 가능 : axes : FALSE
plot(v1, 
     type = 'o',  # 점과 선을 중첩해서 그림
     col='red',
     ylim=c(0,200),
     axes=FALSE,  # x, y 축 없음
     ann=FALSE)   # 축 제목 없음

# 축추가 : axis() 함수
# 1 : x축
# 2 : y축
axis(1, at=c(1:5),
     lab=c('MON','TUE','WED','THU','FRI'))
axis(2,ylim=c(0,200))

#그래프 제목 : main 옵션
title(main='FRUIT',
      col.main='red',
      font.main=4)

#축제목 : xlab, ylab
title(xlab="DAY",col.lab="red")
title(ylab="PRICE",col.lab="blue")

# plot 분할
# plot 화면 분할
# par(mfrow=c(행, 열)) 사용
# 1행 3열로 화면 분할
par(mfrow=c(1,3))
plot(v1, type='o') # 점과 선을 중첩
plot(v1, type='s') # 왼쪽값을 기초로 계단모양으로 연결
plot(v1, type="l") # 선 모양 : 꺽은선 그래프

# 2행 3열로 화면 분할
par(mfrow=c(2,3))
plot(v1, type='o') # 점과 선을 중첩
plot(v1, type='s') # 왼쪽값을 기초로 계단모양으로 연결
plot(v1, type='l')# 선 모양 : 꺽은선 그래프
pie(v1)
plot(v1, type='o')
barplot(v1)

# 화면이 2행 3열로 분할된 상태
plot(v1, type='o')  # 1행 1열에 그려짐

# plot 분할 없음
par(mfrow=c(1,1))

plot(v1, type='o') # 화면에 1개만 그려짐

### 그래프 작성 코드 함수로 생성

p<-function(v){
plot(v,type='o',col='red',ylim=c(0,8),axes=FALSE,ann=FALSE)

axis(1,at=1:5 ,lab=c("A","B","C","D","F"))
axis(2,ylim=c(0,8))

title(main="학점별학생수",col.main="red",font.main=4)
title(xlab="학점(점)", col.lab="black")
title(ylab="학생수(명)",col.lab="blue") }

y <-c(5,7,7,6,0)
p(y)

p2 <- function(v){
  plot(v,type='o',col='red',ylim=c(10,24),axes=FALSE,ann=FALSE)
  
  axis(1,at=1:4 ,lab=c("나리","구슬","송이","난초"))
  axis(2,ylim=c(10,24))
  
  title(main="반별어린이수",col.main="blue")
  title(xlab="반이름", col.lab="black")
  title(ylab="인원수(명)",col.lab="black")
}

y <-c(12,13,20,23)
y <-c(5,7,7,6,0)
p2(y)

#화면 분할
par(mfrow=c(1,2))
p(y)
p2(y)

#-------------------------------------------------------------------
par(mfrow=c(1,1))
# 축제목 위치, 지표값 위치, 지표선 위치
# par() 함수의 mgp 옵션 설정
# mgp = c(제목위치, 지표값위치, 지표선위치)
par(mgp = c(1, 1, 1))  # 각 값을 하나씩 0부터 대입해본다
a <- c(1, 2, 3, 4, 5)
plot(a, xlab="aaa")
# 제목, 지표, 지표선 다 겹쳐서 나옴 (모두 1)

par(mgp=c(3, 2, 1))
plot(a, xlab="aaa")
# 지하 1층에 지표선
# 지하 2층에 지표값
# 지하 3층에 축제목

# 지표선 생략 : 0
par(mgp=c(2, 1, 0))
plot(a, xlab="aaa")

# 그래프의 여백 조정하기
a <- c(1, 2, 3, 4, 5)
plot(a, xlab="aaa")

# par() 함수의 oma 옵션 사용
# par(oma =c(아래, 왼쪽, 위, 오른쪽))
par(oma=c(2,1,0,0))
plot(a, xlab="aaa")

# 기본으로 설정
par(oma=c(0,0,0,0))
plot(a, xlab="aaa")

# par() 함수 : 그래픽 파라미터를 지정
# 그래픽 장치의 설정을 정의하는데 이용할 수 있는 함수
# 여러 가지 그래픽 인수를 이용해서
# 화면의 분할 방법, 글자 크기, 색상 등 
# 여러 가지 그래픽 환경을 다양하게 조정
# par() 함수에서 사용하는 인수의 가지 수가 매우 다양

#----------------------------------------------------------------------
# 여러 개의 그래프 중복으로 겹쳐서 그리기
# par(new=T) : 중복 허용 옵션
# par(new=T) 다음에 있는 그래프가 겹쳐서 출력됨

par(mfrow=c(1,1)) # plot 창 분할 없음

v1 <- c(1,2,3,4,5)
v2 <- c(5,4,3,2,1)
v3 <- c(3,4,5,6,7)

# 일반적으로 plot() 연속 실행시킨 경우
# 그래프는 순서대로 하나씩 출력
plot(v1, type='s', col='red', ylim=c(1,5))
plot(v1, type='o', col='blue', ylim=c(1,5))

# 앞의 plot() 그래프 위에 겹쳐서 출력하려면
# par(new=T) 옵션 사용

plot(v1, type='s', col='red', ylim=c(1,5))
par(new=T) # 중복 허용
plot(v1, type='o', col='blue', ylim=c(1,5))
par(new=T)
plot(v3, type='l', col='green', ylim=c(1,5))

# plot()과 lines() 함수를 사용하여 겹쳐서 출력
plot(v1, type='s', col='red', ylim=c(1, 10))
lines(v2, type='o', col='blue', ylim=c(1,5))
lines(v3, type='l', col='green', ylim=c(1,15))

# lines() 함수 : plot()으로 출력된 그래프 위에
# 꺽은 선을 추가하는 함수

# 대각선 그리기
# abline() 함수 사용
# a : 절편
# 기울기 : 1
# 검정색, 점선
# lty=6 : 2개의 대쉬선
abline(a=0, b=1, col='black', lty=6)

# 수평선 추가 : h= y축값
# lty=3 : 점선
abline(h=3, col='red',lty=3)

# 수직선 추가 : v= y축값
# lty=2 : 대시선
abline(v=5, col='blue',lty=2)

# 범례 추가하기
# legend(x축 위치, y축 위치, 내용,
# cex=문자크기, col=색상,pch=점의모양,lty= 선모양)

v1 <- c(1,2,3,4,5)
v2 <- c(5,4,3,2,1)
v3 <- c(3,4,5,6,7)

# plot()과 lines() 함수를 사용하여 겹쳐서 출력
plot(v1, type='s', col='red', ylim=c(1, 10))
lines(v2, type='o', col='blue', ylim=c(1,5))
lines(v3, type='l', col='green', ylim=c(1,15))

legend(4,10,
       c("v1","v2","v3"),
       cex=0.5,
       col=c("red","blue","green"),
       lty=1) #실선

legend(3,8,
       c("v1","v2","v3"),
       cex=0.6,
       col=c("red","blue","green"),
       lty=1) #실선


legend(4,5,
       c("v1","v2","v3"),
       cex=0.2,
       col=c("red","blue","green"),
       lty=1) #실선

legend(1,8,
       c("v1","v2","v3"),
       cex=10,
       col=c("red","blue","green"),
       lty=1) #실선

## legend 위치 : left, bottom, center, topleft

legend('topleft',
       c("v1","v2","v3"),
       cex=0.5,
       col=c("red","blue","green"),
       lty=1) #실선

legend('bottomright',
       c("v1","v2","v3"),
       cex=0.5,
       col=c("red","blue","green"),
       lty=1) #실선

legend('center',
       c("v1","v2","v3"),
       cex=0.5,
       col=c("red","blue","green"),
       lty=1) #실선

##----------------------------------------------------
# 그래프 연습문제
v1 <- c(75,74,80,75,78,94)
v2 <- c(74,71,77,71,70,76)

par(mfrow=c(1,1))

plot(v1, type="o", col="red", ylim=c(70,95))
lines(v2, type="o", col="blue", ylim=c(70,95))

legend(2,92, c("2003","2013"), cex=0.9, col=c("red","blue"),lty=1)

#---------------------------------------------------------------

plot(v1, type="o", col="red", 
     ylim=c(70,95),axes=FALSE, ann=FALSE)
lines(v2, type="o", col="blue", ylim=c(70,95))

axis(1,at=1:6 ,lab=c("20대","30대","40대","50대","60대","70대"))
axis(2,ylim=c(70,95))

title(main="연령대별평균소비성향",col.main="blue")
title(xlab="연령", col.lab="black")
title(ylab="가계소득대비소비율(%)",col.lab="black")

legend(2,92, c("2003","2013"), cex=0.9, col=c("red","blue"),lty=1)

#-----------------------------------------------------------------
# 화면에 화살표, 사각형, 텍스트 표시
x<-c(10,20,30,40)
plot(x)

#arrows() : 화살표 표시
#(2,30)에서 (2,20)
arrows(2,30,2,20,col="red")

#rect() : 사각형 영역 표시
rect(2,30,3,20,density=20,col="red")

#text() : 텍스트 표시
#srt : 각도
text(3,35,"키포인트",srt=20)

#mtext() : 
# side : 1-하 2-좌 3-상 4-우
mtext("문자열출력",side=3)

#-----------------------------------------------------------
#barplot()
#범주형 자료의 빈도수를 기둥의 높이로 표현하는 그래프
#barplot(height,인수,...)
# height
# 각 기둥의 높이에 해당하는 값 (벡터 또는 행렬)
# 벡터일 경우 각 기둥의 높이가 x 원소들로 결정
# 행렬일 경우에는 열의 개수만큼의 기둥이 만들어지고, 각 기중의 높이는 행의 값의 누적

# 막대그래프 그리기
x<-c(1,2,3,4,5)
barplot(x)
barplot(x,names=c(1:5))

#그래프를 가로로 출력
barplot(x,horiz=T)

#행렬 생성
x<-matrix(c(5,4,3,2),2,2)
x

#
barplot(x,
        names=c(5,3),
        col=c("green","yellow"),
        ylim=c(0,12))

#세로 묶음 막대 그래프
barplot(x,
        names=c(5,3),
        beside=T,
        col=c("green","yellow"))

#가로 묶음 막대 그래프
barplot(x,
        names=c(5,3),
        beside=T,
        col=c("green","yellow"),
        horiz=T)

#가로 누적 막대 그래프
barplot(x,
        names=c(5,3),
        col=c("green","yellow"),
        horiz=T)


# 여러 막대 그래프를 그룹으로 묶어서 한꺼번에 출력하기
v1 <- c(100, 120, 140, 160, 180)
v2 <- c(120, 130, 150, 140, 170)
v3 <- c(140, 170, 120, 110, 160)

qty <- data.frame(BANANA=v1, CHERRY=v2, ORANGE=v3)
qty

# height는 반드시 벡터 또는 행렬 이어야 함
# 데이터 프레임은 행렬로 변환하고 사용
barplot(as.matrix(qty),
        main="Fruit'Sales QTY",
        col=rainbow(nrow(qty)),
        ylim=c(0,900))

barplot(as.matrix(qty),
        main="Fruit'Sales QTY",
        beside=T,
        col=rainbow(nrow(qty)),
        ylim=c(0,300))

legend(14,300,
       c('MON','TUE','WED','THU','FRI'),
       cex=0.8,
       fill=rainbow(nrow(qty)))

# 색상값 생성 함수(16진수 RGB 값)
# 연속된 색상의 벡터 생성
a <- rainbow(7)
a #FF0000

b<-rainbow(7)
b

a<-rainbow(4)
a
b<-rainbow(100)
b

qty

# 행과열을 변환하여 누적 막대 그래프 그리기
t(qty)

barplot(t(qty),
        main="Fruit's Sales QTY",
        ylim=c(0, 900),
        col=rainbow(length(qty)),
        space=1,   # bar 간격
        cex.axis = 0.8, # y축 값 크기
        las=2, # x축 값 기울기 (1: 가로방향/ 2: 세로방향)
        names.arg = c("MON", "TUE", "WED", "THU", "FRI"),
        cex=0.8) # x축 제목 크기

legend(1, 800,
       names(qty),
       cex = 0.7, 
       fill=rainbow(length(qty)))


#------------------------------------------------
#데이터 조건에 따라 색상값을 다르게 설정

# 색상값 설정 코드
col_r <- c(180, 200, 250, 198, 170)
col_r

colors <- c()
colors

for(i in 1:length(col_r)){
  if(col_r[i] >= 200){
    colors <- c(colors, "red")
  } else if (col_r[i] >= 180) {
    colors <- c(colors, "yellow")
  } else {
    colors <- c(colors, "green")
  }
}

colors

barplot(col_r,
        main="Sales QTY",
        names.arg =c("MON", "TUE", "WED", "THU", "FRI"),
        col=colors)

#-------------------------------------------------------------------
# pie 차트
p1<-c(10,20,30,38)
pie(p1, radius=1)

# 시작각도 : init.angle = 90
pie(p1, radius=1,init.angle = 90)

# 색상값과 라벨명 설정
pie(p1,
    radius =1,  # 원형의 크기
    init.angle = 90,  # 시작 각도
    col=rainbow(length(p1)), # 색상 설정
    label=c("Week 1", "Week2", "Week3" , "Week 4")) # label

#수치값과 함께 출력하기
# 소수점 1자리 수로 퍼센트 출력
pct <-round(p1/sum(p1)*100,1)
pct
lab <- paste(pct," %")
lab

pie(p1,
    radius =1,  # 원형의 크기
    init.angle = 90,  # 시작 각도
    col=rainbow(length(p1)), # 색상 설정
    label=lab) # label

# legend() : 범례
legend('topright',
       c("Week 1", "Week2", "Week3" , "Week 4"),
       cex=0.5,
       fill = rainbow(length(p1)))

# 범례 생략하고 그래프에서 레이블로 출력하기
pct <-round(p1/sum(p1)*100,1)
pct
lab1 <-c("Week 1", "Week2", "Week3" , "Week 4")
lab2 <- paste(lab1,'\n',pct," %")
lab2

pie(p1,
    radius =1,  # 원형의 크기
    init.angle = 90,  # 시작 각도
    col=rainbow(length(p1)), # 색상 설정
    label=lab2) # label


# 3D로 출력
# plotrix 패키지의 pie3D() 함수 사용

##---- 그래프 연습
# barplot 연습문제
x <- matrix(c(40, 52, 33, 51), 2, 2)
plot.new()
dev.new()

barplot(x,
        names=c('가족도 중요하지만\n 나를먼저생각한다.',
                '물건을 충동적으로\n 구매하는 경우가 많다.'),#\n 줄바꿈 문자
        main='나를 중시하는 경향',
        beside=T,
        col=c('Magenta1','gray'),
        ylim=c(0,60),
        ylab='(%)')
legend(3.5,60,c("2009","2014"),cex=0.7,fill=c('Magenta1','gray'))

barplot(x,
        names=c('가족도 중요하지만\n 나를먼저생각한다.',
                '물건을 충동적으로\n 구매하는 경우가 많다.'),#\n 줄바꿈 문자
        main='나를 중시하는 경향',
        col=c('Magenta1','gray'),
        ylim=c(0,120),
        ylab='(%)')






