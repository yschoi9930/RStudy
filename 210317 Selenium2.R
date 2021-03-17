# 네이버 금융 기업실적분석 table 가져오기
library(stringr)
library(rvest)

url <- 'https://finance.naver.com/item/coinfo.nhn?code=005930&target=finsum_more'
test_data <- read_html(url, encoding = 'UTF-8')

remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')

remDr$open()

remDr$navigate(url)

# ifrme 형태이고 ifrme의 id가 #coinfo_cp

frames <-  remDr$findElements(using='id',
                              value='coinfo_cp')

print(frames)

# iframe이므로  frme 안으로 접근 : switchToFrame
remDr$switchToFrame(frames[[1]])

# 현재페이지 소스 접근
page_parse <- remDr$getPageSource()[[1]] ; page_parse

page_html <- page_parse %>% read_html()
table = page_html %>%  html_table(fill=TRUE)

Sys.setlocale('LC_ALL', 'Korean')
View(table[[14]])
