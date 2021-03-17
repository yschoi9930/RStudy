# 소스 확인 결과 데이터가 소스의 일부분으로 json 형태로 있음
# 문자열로 가지고 와서 해당 json 부분만 추출하고 -> json 변환 후 -> img url 찾기
# img url 이용해서 해당 img 다운받아 저장 

library(rvest)
library(stringr)
library(RJSONIO)

url <- 'https://search.daum.net/search?w=img&nil_search=btn&DA=NTB&enc=utf8&q=%EC%9E%90%EB%8F%99%EC%B0%A8'
raw_html <- readLines(url, encoding='UTF-8')
length(raw_html)
raw_text <- raw_html[str_detect(raw_html,'collection.meta')]
length(raw_text)

raw_text_split <- str_split(raw_text,"collection.items = ")
raw_text_fin <- str_split(raw_text_split[[1]][2],"; </script>")
raw_text_json <- raw_text_fin[[1]][1]

json_data <- fromJSON(raw_text_json)
test <- json_data[[1]]

search_url <- function(x) {
  data <- x$nf
  data[names(data) %in% "imgur"]
}

img_url <- sapply(json_data,search_url)
img_url

download.file(img_url[1],'car_text.jpg',mode='wb')

# 폴더생성
keyword <- 'car'
dir.create(paste0('C:/Rstudy/',keyword))
setwd(paste0('C:/Rstudy/',keyword))

for (i in 1:length(img_url)){
  file_name <- paste0(keyword,i,'.jpg')
  download.file(img_url[i], file_name, mode='wb') 
}
