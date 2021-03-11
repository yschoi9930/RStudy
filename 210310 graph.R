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
barplot(col_r,
        main="Sales QTY",
        names.arg =c("MON", "TUE", "WED", "THU", "FRI"),
        col=colors)
pie(p1,
    radius = 1,  # 원형의 크기
    init.angle = 90,  # 시작 각도
    col=rainbow(length(p1)), # 색상 설정
    label=c("Week 1", "Week2", "Week3" , "Week 4")) # label

# legend() : 범례
legend('topright',
       c("Week 1", "Week2", "Week3" , "Week 4"),
       cex=0.5,
       fill = rainbow(length(p1)))
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
        ylim=c(0,60),
        ylab='(%)')

# geom_bar() 그래프의 stat 옵션과 position 옵션

# stat 옵션 : y축 값 사용 방법

# stat='count' : 빈도수 계산 (디폴트: 생략 가능)
#                - x축 값만 지정
#                - y축 값 : x축 값의 빈도수
#                - ggplot(data = diamonds, aes(x=cut))

# stat='identity' : y축 값의 높이를 데이터를 기반으로 정해줄 때 사용
#                  - ggplot(data = sleep, aes(x=ID, y=extra))

# position='dodge' : 막대의 위치를 개별적인 막대로 나란히 표현
# position='fill' : 데이터의 종류를 비율로
# position='dodge' : 막대의 위치를 개별적인 막대로 나란히 표현
ggplot(sleep, aes(ID, extra, fill=group)) +
  geom_bar(stat='identity', position = 'dodge')

# fill = 변수 : 데이터의 종류를 비율로 표시
ggplot(diamonds, aes(color, fill=cut)) +
  geom_bar(position='fill')


# 그래프 함수의 주요 옵션은 그래프에 대한 색상/모양/크기/넓이 등에 초점

# colour = "색상"
# shape(pch) = 모양
# size = 크기
ggplot(data = mpg, aes(x = displ, y = hwy)) + 
  geom_point(color = "red",
             shape = 1,
             size = 2)

# Orange 데이터 세트 사용
str(Orange)

# fill : 도형에 색을 채워줄 때 사용
# 오렌지 나무 종류별 둘레의 합을 도식화


Orange %>%
  group_by(Tree) %>% 
  summarize(Sum.circumference = sum(circumference)) %>%
  ggplot(aes(Tree, Sum.circumference)) + 
  geom_bar(stat='identity', fill='red') 

Orange %>%
  group_by(Tree) %>%
  summarize(Sum.circumference = sum(circumference)) %>%
  ggplot(aes(Tree, Sum.circumference, fill=Tree)) + 
  geom_bar(stat='identity')


Orange %>% 
  group_by(Tree) %>%
  summarize(Sum.circumference = sum(circumference)) %>%
  ggplot(aes(Tree, Sum.circumference))+
  geom_bar(stat='identity', color='red', fill='white') 

Orange %>% 
  group_by(Tree) %>%
  summarize(Sum.circumference = sum(circumference)) %>%
  ggplot(aes(Tree, Sum.circumference, color=Tree))+
  geom_bar(stat='identity',  fill='white') 

Orange %>%
  group_by(Tree) %>%
  summarize(Sum.circumference = sum(circumference)) %>%
  ggplot(aes(Tree, Sum.circumference, color=Tree))+
  geom_bar(stat='identity', color=Tree, fill='white')

# fill 변수가 색상을 채우는(fill) 방법을 지정
# values : 색상
# name : 범례 제목
# breaks : 범레에 나타나는 변수값
# labels : 범례 설명 
# limits : 실제 시각화되는 범주
# mpg 데이터 세트에서 class 종류 확인
distinct(mpg, class) # 한 번씩만 출력 (중복 제거)

g <- ggplot(data=mpg, aes(x=class, fill=class)) + geom_bar()
g

# values : 색상
# name : 범례 제목
g + scale_fill_manual(
  values = c("navy", "blue", "royalblue", 
             "skyblue", "orange", "gold", "yellow"),
  name = "자동차 종류")

# breaks : 범례에 나타나는 변수값
g + scale_fill_manual(
  values = c("navy", "blue", "royalblue", 
             "skyblue", "orange", "gold", "yellow"),
  name = "자동차 종류",
  breaks = c("compact", "suv"))

# labels : 범례 설명 
g + scale_fill_manual(
  values = c("navy", "blue", "royalblue", 
             "skyblue", "orange", "gold", "yellow"),
  name = "자동차 종류",
  breaks = c("compact", "suv"),
  labels = c("경차", "SUV"))

# limits : 실제 시각화되는 범주
g + scale_fill_manual(
  values = c("navy", "blue", "royalblue", 
             "skyblue", "orange", "gold", "yellow"),
  name = "자동차 종류",
  breaks = c("compact", "suv"),
  labels = c("경차", "SUV"),
  limits = c("compact", "suv"))

# scales 패키지의 scale_fill_manual() 함수
# 기존의 색상 위에 다른 색상을 입혀주는 함수
# scale_fill_manual을 적용하기 전에 
# fill, 혹은 color함수를 먼저 적용해준 후 사용해야 함
# 그래프의 종류에 따라 fill과 color를 다르게 적용해줌
# line 그래프 : scale_color_manual()
# bar 그래프 : scale_fill_manua()

library(scales)

p <- Orange %>%
  group_by(Tree) %>%
  summarize(Sum.circumference=sum(circumference)) %>%
  ggplot(aes(Tree, Sum.circumference, fill=Tree)) +
  geom_bar(stat='identity')

p

# scales 패키지의 scale_fill_manual() 함수를 통해 
# 각각의 그래프에 색상을 지정
p + scale_fill_manual(values = c( "#FFFFFF", 
                                  "#FFCC00", 
                                  "#FF9900", 
                                  "#FF6600", 
                                  "#FF3300"))

# 그래프에서 범주형 변수

# mtcars 데이터 세트의 cyl 변수
# cyl 변수 : numeric 타입 / 4, 6, 8의 값을 가짐
# geom_bar()에서 x 축의 값을 cyl로 지정하면
# y축 값은 4, 6, 8 각각의 빈도수로 설정됨

# factor() 함수를 사용하지 않고 그래프를 그린 경우
ggplot(mtcars, aes(x = cyl)) + geom_bar()

# 막대 굵기 지정 : width = 0.5
ggplot(mtcars, aes(x = cyl)) + geom_bar(width = 0.5)
# 굵기를 줄이니까 x값 4, 6, 8에만 막대 표시 (종류별 빈도 수)
# 문제 : 빈 범주가 표시됨


# factor() 함수 사용
# 값이 있는 범주만 인식되고 나머지는 생략
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar(width = 0.5)

# factor()에서 level을 지정하여 순서 변경 가능
ggplot(mtcars, aes(x = factor(cyl, level=c('8','6','4')))) +
  geom_bar(width = 0.5)

# 'fill = 범주값'인 경우
# (1) position = "dodge"인 경우
# position='dodge' : 막대의 위치를 개별적인 막대로 나란히 표현
# fill에서 am을 factor()으로 한 경우
ggplot(data=mtcars, aes(x=cyl, fill=as.factor(am))) +
  geom_bar(position = "dodge") 

# (2) position = "fill"인 경우
ggplot(data=mtcars, aes(x=cyl, fill=as.factor(am))) +
  geom_bar(position = "fill")

# 범례 제목 변경
ggplot(data=mtcars, aes(x=cyl, fill=as.factor(am))) +
  geom_bar(position = "dodge") +
  labs(fill ="am")


# 범례 표시 - 위치 변경
# 범례 위치 변경 : theme(legend.position = "위치")
# top / bottom / right / left / none
# 디폴트 : right

d1 <- ggplot(iris, aes(Petal.Length, Petal.Width)) +
  geom_point(aes(color=Species))
d1

d1 + theme(legend.position = "top")
d1 + theme(legend.position = "left")
d1 + theme(legend.position = "right")
d1 + theme(legend.position = "none")

d1 + theme(legend.position = "bottom",
           legend.direction = 'vertical')

# title : 범례 제목 설정
# face=4 : 굵게 기울이기
# face	Font face ("plain", "italic", "bold", "bold.italic")
d1 + theme(legend.title = element_text(face = 4,
                                       color = 'red',
                                       size = 15))

# 그래프 제목 설정
ggplot(data=mtcars, aes(x=cyl, fill=as.factor(am))) +
  geom_bar(position = "dodge") + 
  labs(x = "cyl (실린더)",
       y = "am count (빈도)",
       title = "cyl vs am count 분석",
       subtitle = "막대그래프 이용하기",
       caption = "출처 = 본인작성",
       fill ="AM") + # 범례 제목 변경
  theme(plot.title = element_text(face="bold",
                                  size=20,
                                  hjust=0.5),
        legend.position = "bottom")

# 막대에 레이블 추가
# geom_text(stat="count) 변수 ..count를 사용해서 레이블 출력
ggplot(mtcars, aes(x=factor(cyl), y=..count.., fill=as.factor(am))) +
  geom_bar(position='dodge') +
  geom_text(stat="count",
              aes(label=..count..),
            position=position_dodge(width=0.3),
            vjust=10)
# position=position_dodge(width=0.3) : 
# geom_bar() position인수와 같은 함수를 사용 

