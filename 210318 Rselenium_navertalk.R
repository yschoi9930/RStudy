install.packages('seleniumPipes')

library(rvest)
library(httr)
library(RSelenium)
library(seleniumPipes)
# 네이버 검색 창에 검색어 전달 후 해당 페이지 데이터 크롤링

# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.141.59.jar -port 4445

url <- 'https://m.naver.com'

remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')
remDr$open()

remDr$navigate(url)

webElem <- remDr$findElement(using="id",value="MM_SEARCH_FAKE")
webElem$getElementAttribute('name')
webElem$getElementAttribute('class')
webElem$getElementAttribute('id')

webElem$sendKeysToElement(list('어쩌다 사장'))
                          
# 검색버튼을 찾아서 클릭 #sch_w > div > form > button.sch_submit.MM_SEARCH_SUBMIT
# 검색버튼 영역 - 코드
# <button type="submit" class="sch_submit MM_SEARCH_SUBMIT" style=""><span class="ico_sch_submit">검색</span></button>

button <- remDr$findElement(using="css selector", value='.sch_submit.MM_SEARCH_SUBMIT') 
# selector 경로로 value 설정
button$sendKeysToElement(list(key='enter'))
 
 # 검색페이지로 진입
 # 현재 페이지 확인
 remDr$getCurrentUrl()
 
 selector <- '#ct > section.sc.cs_nbroadcast > div > div.group_wrap > div > div.api_more_wrap > a'
 
 # css = css selector
 talkMore <- remDr$findElement(using='css selector',selector)
 talkMore$clickElement()
 
 # talk 더보기로 진입 - 정적크롤링 가능한지 확인
 test_url <- 'https://program.naver.com/p/17767146/talk'
test_Raw <- read_html('https://program.naver.com/p/17767146/talk') 
test_node <- html_node(test_Raw, '#cbox_module_wai_u_cbox_content_wrap_tabpanel > ul > li.u_cbox_comment.cbox_module__comment_142282372._user_id_no_6zrSB > div.u_cbox_comment_box > div > div.u_cbox_text_wrap > span') 
test_node # {xml_missing} <NA>
# ------------- 정적 크롤링 안됨

## talk 추가 버튼 클릭 코드
##cbox_module > div > div.u_cbox_paginate > a > span > span > span.u_cbox_ico_more
talkMore_ic <- remDr$findElement(using ='css', 'span.u_cbox_ico_more')
talkMore_ic$clickElement()


## 페이지 데이터 가져오기
a <- remDr$getPageSource()[[1]]
a
raw_data <- read_html(a)
raw_node <- html_nodes(raw_data,'.u_cbox_contents')
length(raw_node)
html_text <- html_text(raw_node)
str(html_text)

library(KoNLP)
library(stringr)
library(wordcloud2)
data2 <- sapply(html_text, extractNoun, USE.NAMES = FALSE)
data2

df_raw <- unlist(data2)


df_raw <- Filter(function(x) {nchar(x)>2}, df_raw)
df <- gsub("\\d|[[:punct:]]| ","",df_raw)
count <- table(df)
count <- sort(count,decreasing = T)
write(df,'navertalk.txt')

df_f <- read.table('navertalk.txt')
count_f <- table(df_f)
sort(count_f,decreasing = TRUE) %>% View()
wordcloud2(count_f,size =1.2)
count_f
