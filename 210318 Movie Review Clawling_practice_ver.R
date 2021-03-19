# 해당 영화 페이지 이동
parasite <- subset(df$rank_link,df$rank_text=='기생충')
remDr$navigate(parasite)
# -------------------

# 더보기 클릭
# <a href="./point.nhn?code=161967#tab" class="link_more">더보기<span class="ico_more"></span></a>

selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > a'
more <- remDr$findElement(using ='css', selector )
more$clickElement()

# 한줄평 크롤링 해오기
frame <- remDr$findElements('css',"#pointAfterListIframe") # Elements
print(frame) 
remDr$switchToFrame(frame[[1]])


# -------------------------------------------------
url <- 'https://movie.naver.com/movie/sdb/rank/rmovie.nhn'
remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')
remDr$open()
remDr$navigate(url)



line_text_final <- c()
movie <- c('기생충','극장판 귀멸의 칼날: 무한열차편','반지의 제왕: 두 개의 탑')
for (x in 1:length(movie)) {
  movie_link <- subset(df$rank_link,df$rank_text==movie[x])
  remDr$navigate(movie_link)
  selector <- '#content > div.article > div.section_group.section_group_frst > div:nth-child(5) > div:nth-child(2) > a'
  more <- remDr$findElement(using ='css', selector )
  more$clickElement()
  frame <- remDr$findElements('css',"#pointAfterListIframe")
  print(frame) 
  remDr$switchToFrame(frame[[1]])
  for (i in 1:10) {
    button_s <- paste0('#pagerTagAnchor',i)
    button <- remDr$findElement('css',button_s)
    button$clickElement()
    raw <- remDr$getPageSource()[[1]]
    line_html <- read_html(raw)
    for(n in 1:9) {
      s <- paste0('#_filtered_ment_',n)
      line_node <- html_node(line_html, s)
      line_text <- html_text(line_node)
      line_text <- gsub('\t|\n|\u314b','',line_text);line_text %>%   
        str_replace_all('[[:punct:]]','') %>% 
        str_trim()
      line_text_final <- c(line_text,line_text_final)
    }
  }
  name <- paste0('Final_',movie[2],'.csv')
  write.csv(line_text_final,name)
}

write.csv(line_text_final,'final.csv')

str_squish(movie[2])
str_squish ()