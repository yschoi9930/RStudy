print("Hello World")

xx<- c("a","b","c","d","e")
x <- paste(xx[1], xx[2])
x
paste("I love", "you and", "you love", "me!", sep="&")

substring("bacdefgkfkdds",2,5)
substring("bacdefgkfkdds",2:5)
name<-("")

play<-c(421,298,254,232,239,368,465)
names(play)<- c('sun','mon','tue','wed','thu','fri','sat')
play
play[c(3,5)]<-c(267,241)
play


v1<-c('T','F','T','F','T','F')
sum(v1)

v2<-c(T,F,T,F,T,F)
sum(v2)


a<-c('red','white','brown','green','white','red','brown','white')

b<-factor(a, levels=c(1,2,3,4), labels=c('red','white','brown','green'))

levels(b)

d<-1:100
d

odd<-seq(1,100,2)
odd

pick<-c(odd[3],odd[7],odd[32])

pick2<-odd[c(3,7,32)]

names(pick)<-c('3rd','7th','32th')
pick

pick[c('3rd','32th')]

pick['3rd']<-3
pick

sum(1:5000)

even<-seq(2,101,2); even
sum(even)

v1<-c('T','T','F','T')
sum(v1)

sex <- c('F','M','M','M','F')
age<- c(21,20,25,30,26)
name<-c('철수','길동','관순','감찬','영희')

student<-data.frame(sex,age,name); student
str(student)
student$sex<-factor(student$sex)
str(student)

student[1,2]
view(student)
View(student)
student<-edit(student)
colnames(student)
colSums(student$age)

d<-c('mon',2)
d

matrix(c(2,10,18,4,12,20,6,14,22,8,16,24),nrow=4, byrow=T)
matrix(c(2,4,6,8,10,12,14,16,18,20,22,24), ncol=3)

a<-matrix(c(9,7,5,3,8,11,2,9), ncol=4, byrow=T)

colnames(a)<-c('a','b','c','d'); a
rownames(a)<-c('x','y'); a

b<-t(a);b

df<-data.frame(a); df
str(df)
info<-matrix(c(3,4), byrow=T); info

df.new<-cbind(df,info); df.new

blood<-c('A','O','AB','B','B')
rh<-c('+','+','-','+','+')
age<-c(21,30,43,17,26)

df2<-data.frame(blood,rh,age); df2

colnames(df2)

df.nb<-subset(df2, blood!='B'); df.nb

str(cars)
dim(cars)
cars
class(cars)

a<-matrix(1:12, nrow=3); a

Q8.1<- matrix(1:12, nrow=3) %% 3; Q8.1
Q8.2<- matrix(1:12, nrow=3) + 3; Q8.2
Q8.3<- matrix(1:12, nrow=3) + matrix(1:12, nrow=4); Q8.3
Q8.4<- matrix(1:12, nrow=3) + matrix(1:12, nrow=3); Q8.4
Q8.5<- rbind(matrix(1:9, nrow=3), c('1','2','3')) +3 ; Q8.5

번호<-1:7
제목<-c('그대랑','다툼','빨래','두통','보조개','매듭','이상해')
좋아요<-c(16075,8218,12119,738,3200,16144,5110)

love<-data.frame(번호,제목,좋아요); love
best<-subset(love, 좋아요==max(좋아요)); best

스티브<-c(97,100,83,95,92)
엔더맨<-c(88,82,90,91,87)
크리퍼<-c(100,96,76,89,95)

mid<-t(data.frame(스티브,엔더맨,크리퍼 )); mid

colnames(mid)<-c('국어','역사','수학','과학','영어'); mid

스티브_f<-c(94,95,90,92,89)
엔더맨_f<-c(92,95,87,95,94)
크리퍼_f<-c(100,100,85,84,96)

final<-t(data.frame(스티브_f,엔더맨_f,크리퍼_f )); final

colnames(final)<-c('국어','역사','수학','과학','영어'); final

avg<-(mid+final)/2; avg

fav<-c('여름','봄','여름','겨울','봄','겨울','여름','여름','봄','가을')
favorite<-factor(fav)
levels(favorite)

country<-c('호주','독일','영국','일본','미국','중국','호주','영국','중국','일본',
           '터키','미국','중국','중국','호주','터키','독일','일본','중국','독일')
country_factor<-factor(country)
levels(country_factor)
as.integer(country_factor)


