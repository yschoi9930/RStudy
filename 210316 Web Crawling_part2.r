library(rvest)
library(stringr)
library(RJSONIO)
url <- "https://sports.news.naver.com/kbaseball/news/list.nhn?date=20210316&isphoto=N&page=2"
news_raw<-readLines(url, encoding="UTF-8")

news_json <-fromJSON(news_raw)
length(news_json$list)
news_json$list[[2]]$oid #언론사 코드
news_json$list[[2]]$aid #기사 고유번호
news_json$list[[2]]$title #기사 제목

#---- list 키의 모든 item의 요소 가져오기
sapply(news_json$list,function(x){x$oid})
sapply(news_json$list,function(x){x$aid})
sapply(news_json$list,function(x){x$title})

#-- 2021년3월16일 야구 최신 뉴스의 전체페이지 기사 정보 가져오기

# url 분석

url <- "https://sports.news.naver.com/kbaseball/news/list.nhn?date=20210316&isphoto=N&page=3"
oid <- c()
aid <- c()
title <- c()
# url 생성
for (i in 1:100){
base_url <- 'https://sports.news.naver.com/kbaseball/news/list.nhn?date=20210316&isphoto=N&page='
url <- paste0(base_url,i)
news_raw<-readLines(url, encoding="UTF-8")
news_json <-fromJSON(news_raw)
oid <- c(oid,sapply(news_json$list,function(x){x$oid}))
aid <- c(aid,sapply(news_json$list,function(x){x$aid}))
title <- c(title,sapply(news_json$list,function(x){x$title}))
print(i)
if (i== as.numeric(news_json$totalPages)){
  break
}
}

View(cbind(oid,aid,title))
#---과제 : 오늘부터 5일전까지 날짜에 대하여 데이터 수집
oid <- c()
aid <- c()
title <- c()
for (j in 0:5) {
for (i in 1:100){
  base_url <- 'https://sports.news.naver.com/kbaseball/news/list.nhn?date='
  base_url2 <- '&isphoto=N&page='
  date <- Sys.Date()-j
  date_fin <- gsub('-','',date)
  url <- paste0(base_url,date_fin,base_url2,i)
  # print(url)
  news_raw<-readLines(url, encoding="UTF-8")
  news_json <-fromJSON(news_raw)
  oid <- c(oid,sapply(news_json$list,function(x){x$oid}))
  aid <- c(aid,sapply(news_json$list,function(x){x$aid}))
  title <- c(title,sapply(news_json$list,function(x){x$title}))
  cat('\n data -',i,'수집중')
  if (i== as.numeric(news_json$totalPages)){
    break
  }
}
}

base_ball <- data.frame(oid,aid,title)
View(base_ball)
getwd()
write.csv(base_ball,'네이버_야구_최신뉴스_수집자료.csv')

#----------------------------------------

# 수집한 데이터로 각 기사 세부 내용 크롤링하기
# revest의 read_html로 추출
# readLines로 읽어와서 (char 형태) 기존 함수 이용해서 파싱

link_csv <- read.csv('네이버_야구_최신뉴스_수집자료.csv')
View(head(link_csv))

## url 구성
# https://sports.news.naver.com/news.nhn?oid=144&aid=0000723996

url <- paste0('https://sports.news.naver.com/news.nhn?oid=',
              link_csv$oid[1],'&aid=',link_csv$aid[1])

url
cont_raw <- read_html(url)

# Error in open.connection(x, "rb") : HTTP error 404. ->  페이지가 없음
# oid와 aid의 번호가 숫자 처리되면서 0을 삭제함
# 500번은 서버 관련


# -------------------------------
# 문자처리해서 csv파일 읽어오기

link_csv <- read.csv('네이버_야구_최신뉴스_수집자료.csv', colClasses='character') # colClasses 추가
View(head(link_csv))

url <- paste0('https://sports.news.naver.com/news.nhn?oid=',
              link_csv$oid[1],'&aid=',link_csv$aid[1])

url
cont_raw <- read_html(url)

# 기사 읽어오기 (rvest 이용)
cont_raw <- read_html(url)

cont_node <- html_node(cont_raw, '#newsEndContents')
cont_node

cont_text <- html_text(cont_node)
cont_text <- gsub('\n|\t','', cont_text) %>% 
             str_replace_all('[[:punct:]]','') %>% 
             str_trim() # 공백제거
cont_text

# 다른 방법으로 데이터 읽어오기 ------------------------------------------

url

#  일반 텍스트로 읽어오기
cont_raw_txt <- readLines(url,encoding ='UTF-8')

head(cont_raw_txt)

which(str_detect(cont_raw_txt,'id=\"newsEndContents\"')) # which로 라인 위치 알 수 있음
which(str_detect(cont_raw_txt,'\"news_end_btn\"'))

# 443 ~ 458라인까지

cont_data <- cont_raw_txt[which(str_detect(cont_raw_txt,'id=\"newsEndContents\"'))
                            :which(str_detect(cont_raw_txt,'\"news_end_btn\"'))]
cont_data[4]

# 한덩어리로 결함
cont_data <-paste(cont_data,collapse=" ")
cont_data <- gsub('<.*?>','',cont_data) # <글자들> 형식을 제거 - 태그 제거
cont_data
cont_data <- gsub("\t|&gt;|&lt;|&#160|\"",'',cont_data)
cont_data <-str_trim(cont_data)
cont_data

# ---------------------------------------------
# 네이버 야구 최신뉴스 수집자료.csv 파일을 읽어들여서 oid 열과 aid열을 사용하여
# 각 기사에 접근한 후 기사 내용을 수집한 후 네이버_야구_최신뉴스_수집자료.csv 파일에 수집한
# 컨텐츠를 추가하시오

View(link_csv)

# oid, aid, title, contents
# 작업 후 저장하기
nrow(link_csv)
contents <-c()
for (i in 1:nrow(link_csv)){
  url <- paste0('https://sports.news.naver.com/news.nhn?oid=',
                  link_csv$oid[i],'&aid=',link_csv$aid[i])
  raw_html <- read_html(url)
  raw_node <- html_nodes(raw_html,'#newsEndContents')
  raw_text <- html_text(raw_node)
  cnt <- raw_text %>% str_replace_all('\n|\t|<|>','') %>% 
                  str_replace_all('[[:punct:]]','') %>% 
                  str_trim()
  contents <- c(contents,cnt)
}

nrow(contents)
final <- cbind(link_csv,contents)

write.csv(final,'final.csv')
