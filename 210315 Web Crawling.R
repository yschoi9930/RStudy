library(rvest)
library(stringr)

# 주용 사용 함수
# - read_html(url): url로부터 html을 읽어옵니다.
# - html_nodes(data, find_class): data에서 find_class에 해당하는 부분(노드)을 추출합니다. 
# - html_text(): 노드에서 text 부분만 추출합니다.
# - html_arrt('속성명') : 노드내에의 속성명에 대한 속성값을 가져오기
# - paste0(): 문자열을 붙여줍니다.
# - strsplit(): 문자열을 특정 패턴으로 잘라줍니다. 
# - write.csv(): matrix나 data frame을 csv 파일 형태로 저장해줍니다.


# 요청 : read_html(url) : url 

url <- 'https://entertain.naver.com/ranking/read?oid=609&aid=0000413124'
nv <- read_html(url)

# 정적인 페이지 : selector
# 동적인 페이지 : Xpath

# 읽어온 페이지에서 필요한 node(tag)만 추출 : 
# html_nodes(html 객체, '찾을 패턴(selector or Xpath')
nv_node <- html_nodes(nv,'#content > div.end_ct > div > h2')
nv_node

# 필요한 데이터만 추출 
# html_text(node 객체) : 태그 내 text 추출
# html_attr(node 객체, 속성명) # node 객체의 해당 속성의 값을 추출

nv_text <- html_text(nv_node)
nv_text

# 데이터 정제 (불용어 제거)
# stringr 패키지의 str_replace_all
# gsub()

# 특수기호 제거
# 모든 특수기호 정규식 : '[[:punct]]'
text <- str_replace_all(nv_text, '[[:punct:]]',"")
text <- gsub("\\n|\\t","",text)
text <- gsub("\\n|\\t|\u97d3|→","",text)
text


# 첫번째 크롤링 : 네이버 tv 연예 뉴스 홈 웹페이지 크롤링하기

# 네이버뉴스에서 제공하는 랭킹 뉴스 헤드라인(많이 본 뉴스)
url <- 'https://entertain.naver.com/home'

# 페이지 태그 구성 확인
# 랭킹 1위 뉴스의 텍스트 수집
rank_row <- read_html(url)
class(rank_row)

# 관련 노드 찾아오기
selector <- '#ranking_news > div > div.rank_lst > ul > li:nth-child(1) > a'
rank_node <- html_nodes(rank_row,selector)
rank_node

# text 찾아오기
text <- html_text(rank_node)

# 정제 (내용을 보고 지울 내역을 결정)
text <- str_replace_all(text, '[[:punct:]]',"")
text

# ------------------------------------
# 반복 작업을 통해 해당 일자의 많이 본 뉴스 타이틀 가져오기
# 네이버뉴스에서 제공하는 랭킹 뉴스 헤드라인(많이 본 뉴스)
url <- 'https://entertain.naver.com/home'

# 페이지 태그 구성 확인
# 랭킹 1위 뉴스의 텍스트 수집
rank_row <- read_html(url)
class(rank_row)

# 관련 노드 찾아오기
selector <- '#ranking_news > div > div.rank_lst > ul > li > a'
rank_node <- html_nodes(rank_row,selector)
rank_node

# a태그의 href 속성 수집 - 기사의 세부내용을 크롤링
# href="/ranking/read?oid=076&amp;aid=00037033" 
# -> oid(신문사 고유번호) 값과 aid(기사 고유번호) 값만 달라짐

# text 찾아오기
text <- html_text(rank_node[1])
text

# 정제 (내용을 보고 지울 내역을 결정)
text <- str_replace_all(text, '[[:punct:]]',"")
txt <- str_replace_all(text,"\\t|\\n","")
text

# 정리
title <-c()
for (i in 1:length(rank_node)){
 tit <- html_text(rank_node[i]) %>% 
        str_replace_all('[[:punct:]]',"") %>% 
        str_replace_all("\\t|\\n","")
 title <- c(title,tit)
}

title


# --------------------------------------------------------------
# 세부 기사 링크 가져오기

# 관련 노드 찾아오기
selector <- '#ranking_news > div > div.rank_lst > ul > li > a'
rank_node <- html_nodes(rank_row,selector)
rank_node

# a태그의 href 속성 수집 - 기사의 세부내용을 크롤링
# href="/ranking/read?oid=076&amp;aid=00037033" 
# -> oid(신문사 고유번호) 값과 aid(기사 고유번호) 값만 달라짐

# 속성 찾아오기
lik <- html_attr(rank_node[1],'href')
lik


# 정리
link <- c()
for (i in 1:length(rank_node)){
  lik <- html_attr(rank_node[i],'href')
  link <- c(link,lik)
}

link

rank_news <- data.frame(title, link)
View(rank_news)

# 최종 코드 : url과 타이틀 모두 수집하기
# 반복 작업을 통해서 오늘 tv 연예 랭킹 기사 타이틀과 세부 링크 수집하기
url <- 'https://entertain.naver.com/home'

# 페이지 태그 구성 확인
# 랭킹 1위 뉴스의 텍스트 수집
rank_row <- read_html(url)
class(rank_row)

# 관련 노드 찾아오기
selector <- '#ranking_news > div > div.rank_lst > ul > li > a'
rank_node <- html_nodes(rank_row,selector)
rank_node

# title과 link 를 저장할 벡터 생성
title <- c()
link <- c()
base_link <- 'https://entertain.naver.com'

for (i in 1:length(rank_node)){
  lik <- html_attr(rank_node[i],'href')
  lik <- paste0(base_link,lik)
  link <- c(link,lik)
  
  tit <- html_text(rank_node[i]) %>% 
    str_replace_all('[[:punct:]]',"") %>% 
    str_replace_all("\\t|\\n|♥|\u4e2d|\u5bb6|\ufe0e","")
  title <- c(title, tit)
}

link
title
top_rank_df <-data.frame(title,link)

write.csv(top_rank_df, "네이버_연예뉴스_top10.csv")
