install.packages("miltilinguer")
library(multilinguer)
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")

# github 버전 설치
install.packages("remotes")
# 64bit 에서만 동작
library(remotes)
install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))

library(KoNLP)
library(stringr)

# ----------------------
# wordcloud2 패키지 설치
install.packages('wordcloud2')

# 기초 사용 정리

## 파일 읽어오기(readLines)
getwd()
test_data<-readLines('test.txt')

## 형태소 분할 : extractNoun(data)
extract_result <- extractNoun(test_data)
class(extract_result)


# sapply 함수 사용
s_result <- sapply(test_data, extractNoun,USE.NAMES = T)
s_result

# case1
V1<- '봄이 지나면 여름이고 여름이 지나면 가을입니다. 그리고 겨울이죠'
extractNoun(v1)

# case2
v2 <- '봄이지나 면여름이고 여름이 지나면가을 입니다'
extractNoun(v2)

# 불용어 제거 : gsub 함수 사용
word_list <- c('창업','학생','운영','회사','친구','학교')
length(word_list)

# gusb 단어 제거
word_list <- gsub('운영',"",word_list)
length(word_list)

write(word_list,"word_list.txt")

# 읽어오기
word_list2 <- read.table("word_list.txt")
word_list2 # 빈라인은 제거하고 가져옴

# grep(패턴, 문자열) : 특정 텍스트를 검색한 결과 반환

st <- c('word list', 123, 'my list', 567, 'word')
grep('my',st)
grep('abs',st) # 없으면 0
grep('\\d', st, value = T) # value = T -> 찾은 값 / '\\d' -> 숫자 / \\D -> 숫자가 아닌 것

# seoul_new.txt 파일 형태소 분석 후 워드크라우드 작성

# 사전 설정
# 세종 사전 설정
useSejongDic()

# NIADIC 설정
useNIADic()

data1 <- readLines('seoul_new.txt')
data1

data2 <- sapply(data1, extractNoun, USE.NAMES = FALSE)
data2

# 리스트를 벡터로 변환
data3 <- unlist(data2)
head(data3)

# 불필요한 단어 제거
data3 <- gsub('\\d+',"",data3)
data3 <- gsub('-',"",data3)
data3 <- gsub('[[:punct:]]',"",data3)
data3 <- gsub(' ',"",data3)
data3 <- gsub('서울시|서울|요청|제안|서울시장|시장',"",data3)
head(data3) # 기본 gsub결과 변수 : ""가 너무 많음

write(data3,'seoul_2.txt')

data4 <- read.table('seoul_2.txt') # 데이터 프레임

# 단어별 빈도수 출력 : table() 함수

wordcount <- table(data4)
View(wordcount)

# 빈도수 기준 내림차순 정렬
head(sort(wordcount,decreasing = T),30)

# 불필요한 단어 제거 data3 벡터형, data4 df -> data3이용
data3 <- gsub("OO|문제|민원|관련","",data3)

# 한글자 단어제거
data3 <- Filter(function(x) {nchar(x)>2}, data3)

# 재저장 및 재확인
write(data3,'seoul_2.txt')
data4 <- read.table('seoul_2.txt') # 데이터 프레임

wordcount <- table(data4)
head(sort(wordcount,decreasing = T),30)

# wordcloud 그리기

# 색상 표현 패키지
install.packages("RColorBrewer")
library(RColorBrewer)

# 패키지에 내재된 모든 색상의 파레트 출력
display.brewer.all(n=10, exact.n = FALSE) 
display.brewer.all(n=10, exact.n = TRUE) 

# 종류와 총 색상 수 및 카테고리 출력
brewer.pal.info

# 색상 세트를 불러오는 함수 : brewer.pal(n,name)
# n : 사용하려는 색상 수
# name : 팔레트 이름

palete <- brewer.pal(9,"Set3")
palete

wordcount[1]
names(wordcount)

install.packages('wordcloud')
library(wordcloud)
wordcloud(names(wordcount), # 출력할 단어들
          freq=wordcount, # 언급된 빈도수
          scale=c(5,3), # 글자크기
          rot.per=0.25, # 회전단어 비율
          min.freq=1, # 최소단어빈도
          random.order=FALSE, # 출력되는 순서를 임의로 지정
          random.color=TRUE, # 색상 랜덤하게
          colors=palete) # 색상

library(wordcloud2)

# 기본 출력
wordcloud2(wordcount)

# 배경색 등 색상 변경하기
wordcloud2(wordcount, color="random-light", 
           backgroundColor = 'black',
           fontFamily = '맑은 고딕',
           size =1.2,
           shape ='star')
