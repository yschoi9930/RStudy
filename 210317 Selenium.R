# 셀레니움 문법 정리
install.packages("RSelenium")
library(RSelenium)

# 드라이버 정보 전달해서 open
# 드라이버 객체 생성 : 

remDr <- remoteDriver(remoteServerAddr='localhost', port =4445L, browserName='chrome')

remDr$open() # 브라우저 열기

remDr$getStatus() # 브라우저(서버) 상태 확인

#--페이지 요청

remDr$navigate("http://www.google.com/ncr")

# 현재 페이지 url
remDr$getCurrentUrl()

# 사이트 이동
remDr$navigate("http://www.naver.com")

# 사이트 히스토리 이용해 이동
remDr$goBack() # 뒤로가기
remDr$goForward() # 앞으로 가기

# 현재 페이지 새로고침
remDr$refresh()

# 페이지 내에서 element 찾기
webElem <- remDr$findElement(using = "name", value='q')
webElem$getElementAtrribute('name')
webElem$getElementAtrribute('class')
webElem$getElementAtrribute('id')

# css selector 사용
webElem <- remDr$findElement(using = "css","input[name='q']")
webElem <- remDr$findElement(using = "css",name='q')

# Xpath 사용
webElem <- remDr$findElement(using='xpath', 'Xpath copy해서 붙여넣기')

# Element로 txt 보내기
webElem <- remDr$findElement(using = "name", value='q')
webElem$sendKeysToElement(list("R Cran"))

## keypress to element
webElem <- remDr$findElement(using = "name", value='q')
webElem$sendKeysToElement(list("R Cran","\uE007")) # \uE007 : 엔터키
webElem$sendKeysToElement(list("R Cran", key = 'enter'))
webElems <- remDr$findElements(using ='css selector',"h3.r")
webElems

# getElementText() : 객체의 text 찾아오기
resHeaders <- unlist(lapply(webElems, function(x) {x$getElementText()}))
resHeaders

# selector와 부합하는 여러 객체 중 값이 Ava~인 객체의 위치
webElem <- webElems[[which(resHeaders=='Available CRAN Packages for')]]

# 해당 하이퍼텍스트 클릭
webElem$clickElement()
remDr$getCurrentUrl()
remDr$getTitle()

# 브라우저 닫기
remDr$close()
