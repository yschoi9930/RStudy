
# 네이버 TV 연예 랭킹페이지 크롤링하기

# url 확인
# 브라우저 - naver 뉴스 - TV연예홈 - 랭킹 - 화면 하단의 날짜 클릭

# https://entertain.naver.com/ranking#type=hit_total&date=2021-03-14
# url - https://entertain.naver.com/ranking
# q_str - #type=hit_total&date=2021-03-14
# data = 쿼리의 값을 변경하면 다른 날짜의 데이터 추출도 가능함

url  <- 'https://entertain.naver.com/ranking#type=hit_total&date=2021-03-14'
rank_raw <- read_html(url)

# selector 이용해서 node 추출
selector <- '#ranking_list > li:nth-child(1) > div.tit_area > a'
rank_node <- html_nodes(rank_raw,selector)
length(rank_node) # 0 크롤링 되지 않음

# 페이지 소스 보기에서 확인해야 함
# 찾기 이용해서 id에 해당하는 ranking list를 찾아봄
# ul 태그만 있고 내부소스가 없음 - 이 경우 대부분 javascript에 의해서 코드가 생성되는 경우
# 보통은 data를 같이 넘겨주게 된다(data url를 알려줌)

# selenium을 이용해서 동적으로 요청해야 하는 경우도 있음
# 개발자 도구로 확인해야 함
# 개발자 도구 - 네트워크 도구로 확인
# data가 넘어오는 url 확인 : 
# 다시 데이터 요청작업을 해서 네트워크 탭에서 
# page.json?&type=hit_total&date=20210314 확인
# 더블 클릭해서 해당 링크로 들어가서 확인하면 json 형태의 데이터가 보이게 됨
# so, json 페이지를 크롤링 한다

# 크롤링 할 url 결정
url <- 'https://entertain.naver.com/ranking/page.json?&type=hit_total&date=20210314'

# 위 페이지의 소스는 형태가 XML이나 HTML 형태가 아님 - JSON 형태

# JSON 형태로 파싱을 진행해야 함

# readlines 함수를 이용해서 문자열로 읽어옴
# RJSONIO 패키지를 이용해서 json 형태로 변환
# 구조 확인 후 필요한 html 형태로 변환 후 사용

## ------ 네이버 엔터 랭킹 페이지 크롤링(javascript에 의해 화면 결정되는 페이지)

url <- 'https://entertain.naver.com/ranking/page.json?&type=hit_total&date=20210314'
news_raw<-readLines(url,encoding='UTF-8')
head(news_raw,1)
class(news_raw)

# json 패키지 사용해서 json 데이터로 변환
install.packages('RJSONIO')
library(RJSONIO)

# raw data를 json으로 변환
news_json <-fromJSON(news_raw)
View(news_json)
news_html <- news_json$newsListHtml

# html 형태로 다시 로딩하기
# html로 처리하게 
news_html <- gsub('[\",\n,\t,\r]','',news_html)
# 외부에 html 파일로 저장 - html 파일을 text로 처리 (여기부터 엔코딩을 해서 내보내는 걸로)
write.table(news_html,'test_h.html', fileEncoding = 'utf-8') 

# html 형태로 읽어옴
news_raw_data <- read_html('test_h.html',encoding = 'utf-8')
news_raw_data
html_text(html_nodes(news_raw_data,'li > div.tit_area > a'))
html_attr(html_nodes(news_raw_data,'li > div.tit_area > a'),'href')


# --------------------------- 실습예제

url <- 'https://entertain.naver.com/ranking/page.json?&type=hit_total&date=20210315'
raw <- readLines(url, encoding='UTF-8')
head(raw,1) # 문자열

raw_json <- fromJSON(raw)
View(raw_json)

raw_html<-raw_json$newsListHtml
raw_html <- gsub('[\",\n,\t,\r]','',raw_html)
write.table(raw_html,'raw.html', fileEncoding = 'utf-8') 
raw_data <- read_html('raw.html', encoding='utf-8')
text <- html_text(html_nodes(raw_data, 'li > div.tit_area > a'))
text
link <- html_attr(html_nodes(raw_data, 'li > div.tit_area > a'),'href')
link <- paste0('https://entertain.naver.com',link)
link


df <- data.frame(text,link)
write.csv(df,'last.csv')

# ---------반복으로 시행할 시 ------------------

selector <- 'li > div.tit_area > a'
html_node <- html_nodes(raw_data,selector)
base <- 'https://entertain.naver.com'
link <- c()
title <- c()
for (i in 1:length(html_node)) {
  link <- c(link,paste0(base,html_attr(html_node[i],'href')))
  title <- c(title, html_text(html_node[i]))
}

df2 <- data.frame(title,link)
df2


# --------------여러 날짜 데이터 크롤링하기
# url 구조 확인
# https://entertain.naver.com/ranking#type=hit_total&date=2021-03-09
# https://entertain.naver.com/ranking/page.json?&type=hit_total&date=20210309
url <- 'https://entertain.naver.com/ranking/page.json?&type=hit_total&date='

# 날짜 구성
Sys.Date()

dt <- Sys.Date()
dt <- gsub('-','',dt)
url_fin <- paste0(url,dt)
url_fin

# 오늘 날짜를 기준으로 이전 5일의 데이터(title, link) 추출하기
raw_json<-c()
raw_html<-c()
for(j in 0:5) {
  rank_date <- gsub("-","",Sys.Date()-j)
  url_fin <-paste0(url,rank_date)
  raw <- readLines(url_fin,encoding='utf-8')
  raw_json1 <- fromJSON(raw)
  raw_json <-c(raw_json,raw_json1)
  raw_html1 <- raw_json$newsListHtml
  raw_html <- c(raw_html, raw_html1)
  raw_html <- gsub('[\",\n,\t,\r]','',raw_html)

}

write.csv(raw_html,'date.html',fileEncoding = 'utf-8')
raw_data <- read_html('date.html', encoding='utf-8')
head(raw_data,1)

selector <- 'li > div.tit_area > a'
html_node <- html_nodes(raw_data,selector)
base <- 'https://entertain.naver.com'
link <- c()
title <- c()
length(html_node)
for (i in 1:length(html_node)) {
  link <- c(link,paste0(base,html_attr(html_node[i],'href')))
  title <- c(title, html_text(html_node[i]))
}

date.df <- data.frame(title,link)
View(date.df)

write.csv(date.df[1:30],'210315.csv')
