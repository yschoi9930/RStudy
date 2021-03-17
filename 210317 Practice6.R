# 검색어를 설정해서 네이버 검색 API를 이용해서 이미지 검색해오기
# 검색한 이미지를 다운로드해서 폴더생성 후 저장하시오
# 이미지 검색은 200개

base <- 'https://openapi.naver.com/v1/search/image.xml?'

keyword <- '브레이브걸스'

search <- paste0('query=',keyword)
search <- iconv(search,to='UTF-8')
search

search <- URLencode(search)

req <- '&display=100&start=1&sort=sim'

URL <- paste0(base,search,req)
URL

clientID <- "dylL4vaALt3jr39dZW4c"
clientpw <- '6IcFbl92ar'

api <- GET(URL,
           add_headers('X-Naver-Client-Id' = clientID,
                       'X-Naver-Client-Secret' = clientpw))
api

api$content

result <- rawToChar(api$content)
result

Encoding(result) <- 'UTF-8'
result

library(xml2)
xml_r <- read_xml(result)
child <- xml_child(xml_r)
child
img_items <- xml_find_all(child,'item')
View(img_items)

library(tidyr)
search <- function(x) {
          temp <- xml_find_all(x,'./*')
          td_raw <- tibble(key = xml_name(temp),
                           value = xml_text(temp))
          return(td_raw)
}

length(img_items)
search(img_items[1])

td <- lapply(1:100, function(n) {
                      temp <- xml_find_all(img_items[n],'./*')
                      td_raw <- tibble(
                      id = n,
                      key = xml_name(temp),
                      value = xml_text(temp)
                    )
                    return(td_raw)
})

td

td <- bind_rows(td)
td <- spread(td,key,value)
td <- td[,2:6]
View(td)

img_link <- td[,4]
img_link

for (i in 1:100){
  
  
}

paste0

download.file(,'a.jpg',mode='wb')

img_link <- img_link[[1]]
img_link[1]

download.file(img_link[i], file_name, mode='wb')

for (i in 1:length(img_url)){
  img_url <- paste0(",img_link[i],")
  file_name <- paste0(keyword,i,'.jpg')
  download.file(img_url[i], file_name, mode='wb') 
}
