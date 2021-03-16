# 네이버 지도를 이용하여 원하는 지역의 위경도 얻어오기
# url 찾기
# json 형태의 data가 전달됨

url <- 'https://map.naver.com/v5/api/instantSearch?lang=ko&caller=pcweb&types=place,address,bus&coords=37.
55517588296082,126.93685054779054&query=%EC%8B%A0%EC%B4%8C%EC%97%AD'

## 신촌역 2호선 위경도 얻어오기
coord_raw <- readLines(url, encoding = 'UTF-8')
coord_json <- fromJSON(coord_raw)
head(coord_json)
coord_json$place[[2]]$title
coord_json$place[[2]]$x
coord_json$place[[2]]$y


# ----검색어에 따른 네이버 지도 정보 추출(함수 구성)
keyword <- "신촌역"
url1 <- 'https://map.naver.com/v5/api/instantSearch?lang=ko&caller=pcweb&types=place,address,bus&coords=37.
55517588296082,126.93685054779054&query='
url <- paste0(url1, keyword)
url


# url의 키워드는 퍼센트 인코딩 처리 되어야 함
keyword <- '삼성역' # cp949
Encoding(keyword) # 엔코딩 형식을 확인하는 함수 : unknown, 에러가 남, cp949 형식은 읽지 못함

## cp949 형식을 utf형식으로 변경
keyword <- iconv(keyword,from='CP949',to='UTF-8')
Encoding(keyword) # "UTF-8" 형식 확인
URLencode(keyword) # "%EC%8B%A0%EC%B4%8C%EC%97%AD" 퍼센트엔코딩 함수 적용 완료

# --------------------------
key <- "신촌역"
key <- iconv(key,from='CP949',to='UTF-8')
Encoding(key) # "UTF-8" 형식 확인
key <- URLencode(key)
url1 <- 'https://map.naver.com/v5/api/instantSearch?lang=ko&caller=pcweb&types=place,address,bus&coords=37.
55517588296082,126.93685054779054&query='
url <- paste0(url1, key)
url

# ---------------- 함수생성

search_loc <- function(x) {
  key <- iconv(x,from='CP949',to='UTF-8')
  key <- URLencode(key) 
  url1 <- 'https://map.naver.com/v5/api/instantSearch?lang=ko&caller=pcweb&types=place,address,bus&coords=37.
55517588296082,126.93685054779054&query='
  url <- paste0(url1, key)
  coord_raw <- readLines(url, encoding = 'UTF-8')
  coord_json <- fromJSON(coord_raw)
  tit <- coord_json$place[[1]]$title
  p_x <- coord_json$place[[1]]$x
  p_y <- coord_json$place[[1]]$y
  return(c(tit,p_x,p_y))
}

rst <- search_loc('사당역')
rst

search_loc <- function(x) {
  key <- iconv(x,from='CP949',to='UTF-8')
  key <- URLencode(key) 
  url1 <- 'https://map.naver.com/v5/api/instantSearch?lang=ko&caller=pcweb&types=place,address,bus&coords=37.
55517588296082,126.93685054779054&query='
  url <- paste0(url1, key)
  coord_raw <- readLines(url, encoding = 'UTF-8')
  title <- c()
  x_loc <- c()
  y_loc <- c()
  coord_json <- fromJSON(coord_raw)
  for(i in 1:length(coord_json$title)){
  tit <- coord_json$place[[i]]$title
  p_x <- coord_json$place[[i]]$x
  p_y <- coord_json$place[[i]]$y
  title <- c(title,tit)
  x_loc <- c(x_loc,p_x)
  y_loc <- c(y_loc,p_y)
  }
  return(data.frame(title,x_loc,y_loc))
}

rs <- search_loc('사당역 카페')
View(rs)
