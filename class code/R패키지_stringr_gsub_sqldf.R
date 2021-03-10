#R패키지_stringr_gsub_sqldf.r

#stringr : 문자열처리
#sqldf : 데이터를 sql 쿼리로 다루기
#gsub() : 불필요한 문자 제거

#--------------------------------------------------------------
# stringr 패키지
# install.packages("stringr")
library(stringr)

# 문자열은 데이터의 70% 정도 차지 함 - 문자열을 가능한 쉽게 처리하도록
# 기능을 제공하는 패키지

# string_detect() 함수 : 특정 문자의 포함여부 확인
# True/FALSE 로 반환
fruits <- c('apple','Apple','banana','pineapple')
fruits

# 대문자 'A'가 포함된 요소 
str_detect(fruits,'A')
# [1] FALSE  TRUE FALSE FALSE

# 첫문자가 소문자  a 인 단어 찾기
# ^ : 첫문자를 표시
# ^a : a로 시작하는
str_detect(fruits,'^a')
# [1]  TRUE FALSE FALSE FALSE

# 찾는문자가 들어 있는 단어 출력
fruits[str_detect(fruits,'A')] # 크롤링에서 자주 사용하는 문법

#  결과가 TRUE인 데이터가 여러개 있는 경우
fruits[str_detect(fruits,'a')]
# [1] "apple"     "banana"    "pineapple"

#~로 끝나는 : $ 사용 => e로 끝나는 단어 e$
fruits[str_detect(fruits,'e$')]
# [1] "apple"     "Apple"     "pineapple"

# which() 함수 : 요소의 위치 찾기

# 단어에 소문자 a나 대문자 A가 들어 있는 단어 포함 여부 확인
# [aA]
str_detect(fruits,'[aA]')

# 패턴을 설정하고 설정된 패턴에 일치하는 문자열 포함 여부
pattern <- "a.b" #a가 출연하고뒤에 한글자가 나오고 b가 나오는 : . 은 한글자
string <- c("ABB","aaB","aa.b")
str_detect(string,pattern)

pattern <- "a..b"
str<-c("ABB","aab","aa.b")
str_detect(str,pattern)

fruits
str_detect(fruits,'[banana]') #[]의 문자 하나 하나를 매칭시킴

#--------------------------------------------------------
#str_count() : 특정문자의 출현 횟수를 세는 함수
fruits
str_count(fruits,'a')

#--------------------------
#대소문자 변환하기
# 소문자로 변환 : str_to_lower()
# 대문자로 변환 : str_to_upper()
mystr <- 'hi everybody'

up <-str_to_upper(mystr)
str_to_lower(up)

# str_to_title
str_to_title(up) #단어의 첫 글자를 대문자로 변환
# 원래 모든 글자가 대문자면 첫글자 제외한 나머지 글자는 소문자로 변환

# str_c() :  문자열 합치기
str_c('apple','banana')

# 여러 요소의 데이터를 합치면 각 요소별로 대입 됨
fruits
str_c(fruits,': Fruits')

str_c(fruits,' name is ', fruits)

# 모든 요소들을 하나로 합치기
# collapse = 옵션 사용(구분문자를정의)
fruits
str_c(fruits) #fruits형태 그대로 출력 됨
str_c(fruits,collapse="")

#str_dup() :  반목출력
# 각 요소마다 반복 반환
str_dup(fruits,3)

# str_length()
length # 구조의 요소 개 수 반환
str_length(fruits) #각 요소의 문자열 길이 반환

# str_locate() : 특정 문자의 위치 값 반환
str_locate('apple','a')
    # start end
# [1,]     1   1

str_locate(fruits,'a') # 요소 각각에 대해서 확인

# 문자 변경(대체) 함수
# str_replace(전체문자열, 대상, 교체될 문자열)
str_a <- 'apple'

str_replace(str_a,'p','*')
# 처음 만나는 p만 변경하고 종료

str_replace_all(str_a,'p','*')
# 모든 p를 변경

#str_split() : 문자 분리 - 리스트로 반환

fruits2 <- str_c(fruits,collapse = "/")
fruits2

# /를 기준으로 분할
str_split(fruits2,'/')
# [[1]]
# [1] "apple"     "Apple"     "banana"    "pineapple"

#리스트 접근 방법
str_split(fruits2,'/')[[1]][2]

# 리스트를 세부 데이터 구조로 변환
unlist(str_split(fruits2,'/'))


#stringr 패키지는 아니지만 문자열에서 자주 쓰는 함수(base) : paste(,collapse=) ->문자열 결합함수
paste(c('a','b','c'),collapse='/')
paste('가','/','나','/')

# paste는 두 가지 형태가 있음
# paste vs paste0
# paste (..., sep = " ", collapse = NULL)
# paste0(..., collapse = NULL)
# paste는 나열된 원소 사이에 공백을 두고 결과값을 출력합니다.
# paste0은 나열된 원소 사이에 공백없이 출력합니다.

paste(1,2,3,4)
# [1] "1 2 3 4"
paste(1,2,3,4,sep="")
# [1] "1234"
paste0(1,2,3,4)
# [1] "1234"

# str_sub() : 부분 문자열 추출
# str_sub(문자열,start=, end=)
str_sub('apple',start=1, end=3)

# 여러 요소를 입력으로 전달하면
# 각 요소마다 순환적용
str_sub(fruits,start=1, end=3)

# - : 뒤에서부터 시작
str_sub('banana',start=-5)


# str_trim() : 공백제거
str_trim('    applebanana     ')
str_trim('    applebanana     ',side='right')
str_trim('    applebanana     ',side='left')

#-----------------------------------------------------------------------------------------------
# gsub() : 문자 정제 하는 함수
# 불필요한 문자를 제거 할 때 많이 사용 : 태그, 특수문자등을 제거 할 때 사용

# 형식
# gsub(변경전문자, 변경후문자, 데이터, 옵션)

#ABC를 ***로 변환
gsub("ABC","***","ABCabcABC")

#ignore.case=T : 대소문자 구분하지 말고 진행
gsub("ABC","***","ABCabcABC",ignore.case=T)

# 패턴문자 사용
# b와 n 사이에 1개의 문자가 있는 패턴을 *** 로 변경
gsub("b.n","***","i love banana")

#---------------------------------------------------------------------------
# sqldf 패키지 활용
install.packages('sqldf')
library(sqldf)

install.packages('googleVis')
library(googleVis)

Fruits

# 데이터세트에서 모든 데이터 가져오기
sqldf("select Year, Fruit from Fruits")
# 와일드문자 사용
sqldf("select * from Fruits")
# 조건문 사용
sqldf("select Year, Fruit from Fruits where Fruit='Apples'")
# 조건에 연산 사용
sqldf("select * from Fruits 
       where Fruit='Apples' and sales >= 100 ")
# 출력되는 행 수 제어
sqldf("select Year, Fruit from Fruits limit 5")

# 정렬 출력 : order by
sqldf("select Year, Fruit from Fruits order by Year")
sqldf("select Year, Fruit from Fruits order by Year,Fruit")

sqldf("select Year, Fruit from Fruits order by Year desc")

# 정렬 옵션을 추가하고 기준을 확장
sqldf("select Year, Fruit from Fruits order by Year desc, Fruit")

# 그룹 함수 사용 가능
# sum()/avg()/max()/min()
# select 그룹함수(컬럼) from 데이터프레임

# 총판매량을 구하시오
sqldf("select sum(Sales) from Fruits")

Fruits
sqldf("select sum(Sales), Profit from Fruits") # 논리적으로 잘못된 출력
#   sum(Sales) Profit
# 1        845     20

sqldf("select sum(Sales), avg(Profit) from Fruits")
sqldf("select max(Sales), min(Sales) from Fruits")

# Group By 절 사용
# select ~ from ~ group by ~
sqldf("select Fruit, avg(Sales) from Fruits group by Fruit")

# Fruits df 의 관측치 개수를 추출하시오(sqldf 함수 사용)
sqldf('select count(*) from Fruits')

library(ggplot2)
View(mpg)


# mpg 의 모든 데이터를 출력 하시오.
head(sqldf("select * from mpg"),10)
# cyl이 4개인 자동차만 추출 하시오.
sqldf("select * from mpg where cyl=4")
# mpg의 처음 3행만 추출 하시오.
sqldf("select * from mpg limit 3")
# mpg의 제조사를 기준으로 오름차순 정렬 하시오.
sqldf("select * from mpg order by manufacturer")
# mpg의 cyl을 기준으로 내림차순 정렬 하시오.
sqldf("select * from mpg order by cyl desc")
# mpg의 hwy 총 합을 구 하시오.
sqldf("select sum(hwy) from mpg")
# 고속도로 연비(hwy)가 가장 높은 자동차의 정보를 출력 하시오.
sqldf("select * from mpg 
       where hwy = (select max(hwy) from mpg)")
# 제조사별로 고속도로 연비의 최대값을 구하시오.
sqldf("select manufacturer, sum(hwy) from mpg group by manufacturer")




















