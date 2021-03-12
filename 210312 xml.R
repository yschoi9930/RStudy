library(xml2)
library(dplyr)
library(stringr)
install.packages('tidyverse')
library(tidyverse)

xpath01 <- 'data/전통시장상점현황.xml'
xdata01 <- read_xml(xpath01)

# raw xml 구조 확인
class(xdata01)
typeof(xdata01) # = mode(xdata01)
str(xdata01)

View(xdata01)
xdata01 # {xml_document} : 묶음을 가져온 것

# raw xml의 자식노드 구조 파악
# xml_children(xmlDATA)
child_node<-xml_children(xdata01) # {xml_nodeset (396)} : 396개 모두 가져옴
mode(child_node)

child_node[1]

# xml_find_all(x,y) : x번째 노드의 y번째 항목을 모두 찾기
# './*' : y항목 모두를 의미

xml_find_all(child_node[2],'./*')

# 태그 사이의 문자
child_node[2] %>% xml_find_all('./*') %>% xml_text()

# 태그 사이의 이름
child_node[2] %>% xml_find_all('./*') %>% xml_name()

# 인덱스를 변수화하여 한번에 가져올 수 있음

xml_extract <- function(x) {
  # t_row : x번째 행 데이터의 child nodeset
  t_row = xml_find_all(child_node[x],'./*')
  tibble(
    idx=x,
    # tag
    # xml_name() : tag name 가져오기
    key=t_row %>% xml_name(),
    # text = value
    value=t_row %>% xml_text()
    ) %>% return()
}

seq(child_node) %>% str() # child_node만큼 벡터 생성 : 396개

m_T <- lapply(seq(child_node),xml_extract)
head(m_T)

# bind_row() : table로 묶어주는 함수
# spread(key, value) : 키 값을 colnames으로 value 값을 내용으로 펼쳐주는 함수

library(tidyr)
data1 <- m_T %>% bind_rows() %>% spread(key,value); data1


# Example
install.packages('rvest')
library(rvest)
url<-"https://entertain.naver.com/read?oid=408&aid=0000116226"

nv<-read_html(url)
head(nv)
class(nv)
View(nv)
mode(nv)

nv_node <- html_nodes(nv, '#content > div.end_ct > div > h2')
nv_node

nv_text <- html_text(nv_node)
nv_text

View(nv_node)
