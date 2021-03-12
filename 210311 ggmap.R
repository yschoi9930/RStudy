# 설치 방법 1
install.packages('ggmap')
library(ggmap)

# 설치 방법 2 : github 사용
install.packages('devtools')
library(devtools)
install_github("dkahle/ggmap")
# 설치 중간에 update 관련은 enter로 pass

# 설치가 안될 시
# ggmap, tibble, dplyr 패키지를 삭제 후 ggmap 다시 설치

# 구글맵 사용 방법
# 1. google API 키 등록
register_google(key='')

# 구글 지도 가져오기 : 구글 서버에 요청을 보낸다
# get_googlemap ('기준위치', zoom 정보(생략가능), 지도종류(생략가능))
# 지도 종류 : terrain, satellite, roadmap, hybrid
ggseoul <- get_googlemap('대전',maptype = 'terrain')

# gg_seoul의 위치 값에 따른 구글 지도 호출
ggmap(ggseoul)

# 위 경도 얻어오기
# getgeocode('주소 혹은 지역명')를 이용함
# 한글의 경우에는 utf-8로 변환한 후 얻어옴
# enc2utf8() 함수를 이용해서 작업 

# 대전역 위 경도 얻어오기
geo_code <- enc2utf8("대전역") %>% geocode()
geo_code

# 자동변환 기능이 생겨 따로 변환안해도 되긴 함
geocode("대전역")

class(geo_code) # df
typeof(geo_code) # list

# 대전역 위치를 지도에 표시
get_googlemap("대전역", maptype = "roadmap", zoom =13) %>% ggmap() +
  geom_point(data=geo_code,
             aes(x=lon,y=lat),
             size=5)


# 경복궁의 위치를 지도에 표식하시오
get_googlemap("seoul", maptype = "roadmap", zoom =13) %>% ggmap() +
  geom_point(data=geocode('경복궁'),
             aes(x=lon,y=lat),
             size=5) +

  



