# 네이버검색 API 사용

## 개발자 등록 필요
## 요청변수 확인 필요 - get 방식 요청
## header 에 첨부해서 보낼 변수 확인

# https://openapi.naver.com/v1/search/blog.xml
# ?query=검색어&display=검색결과수&start=검색시작위치&sort=sim(유사도순)

#query는 필수 변수, 나머지 변수는 기본값이 있음(생략가능)

#네이버 API document 활용

## httr 패키지 - 요청, 응답시 사용 패키지
library(httr)

# 기본 URL
urlstr <- 'https://openapi.naver.com/v1/search/blog.xml?'

# 검색어 설정
keyword <- '자동차'
searchstr <- paste0('query=',keyword)

# 한글 인코딩
searchstr <- iconv(searchstr, to='UTF-8')

# url인코딩
searchstr <- URLencode(searchstr)
searchstr

# 나머지 요청변수 설정
req_str <- '&display=50&start=1&sort=sim'

# 요청변수 조립
req_URL <- paste0(urlstr,searchstr,req_str)
req_URL

# headers 변수 설정
clientID <- "dylL4vaALt3jr39dZW4c"
clientpw <- '6IcFbl92ar'

# 호출 : GET(url) 함수 이용 - 헤더데이터는 add_headers(변수,변수)
apiRest <- GET(req_URL,
               add_headers('X-Naver-Client-Id' = clientID,
                           'X-Naver-Client-Secret' = clientpw))

# 호출결과 조회
apiRest # Status : 200 -> 정상 응답

apiRest$content

result <- rawToChar(apiRest$content) # binary data -> 문자로
result

# 한글 인코딩
Encoding(result) <- 'UTF-8'
result

length(result)

# 데이터 파싱
# xml 데이터 -> xml2 패키지 사용
library(xml2)

# 데이터 읽어오기
content <- read_xml(apiRest)
content

con_child <- xml_child(content)
con_child

# <total>18354265</total> : 총 18354265 데이터가 추출되었지만 1100까지만 가져올 수 잇음

items <- xml_find_all(con_child, 'item')
items # 50개 들어와있음

View(items)

items[1]

test <- xml_find_all(items[1],'./*')
class(test)
test

xml_name(test)
xml_text(test)

# 데이터 추출 후 df로 정제

library(dplyr)
srch <- function(x) {
  temp <- xml_find_all(x,'./*')
  td_raw <- tibble(key = xml_name(temp),
                   value = xml_text(temp))
  return(td_raw)
} 

td <- srch(items[1])
td

ld <- lapply(1:length(items),
             function(n) {
               temp <- xml_find_all(items[n],'./*')
               td_raw <- tibble(
                 id = n,
                 key = xml_name(temp),
                 value = xml_text(temp)
               )
               return(td_raw)
             })

ld

bind_td <- bind_rows(ld) # 리스트 요소들을 행결합
View(bind_td)

library(tidyr)
spread(bind_td,key,value)
