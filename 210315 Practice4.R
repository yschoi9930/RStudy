# 과제 :  네이버 헤드라인 뉴스 크롤링
# url 얻어오기
# 헤드라인의 첫번째 뉴스 크롤링
# 헤드라인의 뉴스 타이틀 5개 크롤링
# link 5개 크롤링
library(rvest)
library(stringr)

url <- 'https://news.naver.com/'

headline <- read_html(url)
selector <- '#today_main_news > div.hdline_news > ul > li > div.hdline_article_tit > a'

head_node <- html_nodes(headline, selector)
head_node
title <- html_text(head_node)
title
base_link <-  'https://news.naver.com'

title <- c()
link <- c()
for(i in 1:length(head_node)) {
  lik <- html_attr(head_node[i],'href')
  lik <- paste0(base_link, lik)
  link <- c(link,lik)
  tit <- html_text(head_node[i]) %>% 
    str_replace_all('[[:punct:]]',"") %>% 
    str_replace_all('\n| ',"")
  title <- c(title,tit)
}

link
title

head_df <- data.frame(link, title)
head_df

write.csv(head_df, 'headline.csv')



# 각 섹션에 대한 링크만 따로 구성을 해서 csv 파일로 저장
# naver_news_section
# 행(섹션) lk1 lk2 lk3 lk4 lk5 
# 정치
# 사회
# 문화
# 세계
# it

url <- 'https://news.naver.com'

sec_html <- read_html(url)
p_selector <-'#section_politics > div.com_list > div > ul > li > a'
e_selector <- '#section_economy > div.com_list > div > ul > li > a'
s_selector <- '#section_society > div.com_list > div > ul > li > a'
l_selector <- '#section_life > div.com_list > div > ul > li > a'
w_selector <- '#section_world > div.com_list > div > ul > li > a'
i_selector <- '#section_it > div.com_list > div > ul > li > a'

p_nodes <- html_nodes(sec_html, p_selector)
e_nodes <- html_nodes(sec_html,e_selector)
s_nodes <- html_nodes(sec_html,s_selector)
l_nodes <- html_nodes(sec_html,l_selector)
w_nodes <- html_nodes(sec_html,w_selector)
i_nodes <- html_nodes(sec_html,i_selector)

link <-c()

for (i in 1:length(p_nodes)){
  lik <- html_attr(p_nodes[i],'href')
  lik <- paste0(url,lik)
  link <- c(link,lik)
}

link

# ----------------------------------------------------------------------
url <- 'https://news.naver.com'

sec_html <- read_html(url)

section_name <- c('politics','economy','society','life','world','it')

title <- c()
link <- c()
news_head <- data.frame(head_1=c(1:6), head_2=NA, head_3=NA, head_4=NA, head_5=NA)
news_head
rownames(news_head) <- section_name
news_head

for (i in 1:length(section_name)) {
  selector <- paste0('#section_',section_name[i],' > div.com_list > div > ul > li > a')
  news_node <- html_nodes(sec_html,selector)
  for (j in 1:length(news_node)) {
    lik <- html_attr(news_node[j],'href')
    news_head[i,j] <- lik
  }
}

View(news_head)

