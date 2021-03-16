# 네이버 영화 사이트 크롤링
# movie.naver.com
# 1.1 왼쪽 사이드 메뉴에서 영화랭킹 선택 후 조회순 랭킹 1~50위에 대하여 제목과 세부링크 url을 크롤링
## 최종 데이터 데이터 프레임으로 구성

# 1.2 각 영화 링크로 접근하여 네티즌 관람객 평점과 기자 평론가 평점을 크롤링하여
# 1번에서 작업한 내용과 모두 결합하여 영화랭킹.csv 파일로 저장

# 수업과제2 
# 2.1 과제 1에서 크롤링한 각 영화의 url를 사용하여 영화의 리뷰를 수집한다
# 2.2 과제 1에서 크롤링한 영화의 url를 사용하여 모든 영화의 리뷰를 수집한다
# 예 ) 리뷰.csv

# 1.1

url <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn'
selector <-'#old_content > table > tbody > tr > td.title > div > a'
base_url <- 'https://movie.naver.com/'

rank_html <- read_html(url)
rank_node <- html_nodes(rank_html,selector)
rank_text <- html_text(rank_node)
rank_text <- gsub('\r|\n|\t','',rank_text) %>% str_trim()
rank_text

length(rank_node)

rank_node[50]

rank_link <- html_attr(rank_node,'href') ; 
rank_link <-paste0(base_url,rank_link)
rank_link

rank_df <- data.frame(rank_text,rank_link)
View(rank_df)




people_selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > div.score_area > div.netizen_score > div > div > em'
reporter_selector <-'#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > div.score_area > div.special_score > div > div > em'

review_url <- rank_link[49] ; review_url
review_html <- read_html(review_url) ;review_html
review_node <- html_nodes(review_html,people_selector)
review_text <- html_text(review_node) ;review_text

people_review <-c()
reporter_review <- c()
for(i in 1:50) {
  review_url <- rank_link[i]
  review_html <- read_html(review_url)
  people_review_node <- html_nodes(review_html,people_selector)
  people_review_text <- html_text(people_review_node)
  people_review <- c(people_review, people_review_text)
  reporter_review_node <-  html_nodes(review_html,reporter_selector)
  reporter_review_text <- html_text(reporter_review_node)
  reporter_review <- c(reporter_review, reporter_review_text)
}

length(people_review)
length(reporter_review)

review_df <- data.frame(people_review,reporter_review)
View(review_df)
