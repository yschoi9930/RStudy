# 행렬 
# 기본적으로 열로 생성
mat1 <- matrix(c(1,2,3,4))
mat1

# 2 행으로 구성(위에서 아래로)
mat1 <- matrix(c(1,2,3,4),nrow=2)
mat1

# byrow=T 옵션 : 행우선 배치
mat1 <- matrix(c(1,2,3,4),nrow=2,byrow=T)


# 서대문구치킨집이가장많은지역찾기

#http://www.localdata.kr/

# 01. 엑셀 파일 읽어오기
library('readxl') # 패키지로드

ck <- read_excel('data/치킨집_가공.xlsx')
head(ck)

# 02. [소재지전체주소]열에서 'xxx동'만 남기고
# 이후 상세 주소 삭제

# substr() 함수 사용
addr <- substr(ck$소재지전체주소,11,16)
head(addr)

# 동 이름이 4글자 또는 3글자와 숫자로 되어 있음
# 공백과 숫자제거
# gusub() : 텍스트의 문자를 다른 문자로 대체해주는 함수

# 숫자제거 : 숫자를 ""로 대체
# 모든 숫자 표현 : [0-9] 
addr_num <- gsub("[0-9]","",addr)
head(addr_num)

# 공백제거 : 
addr_trim <- gsub(" ","",addr_num)
head(addr_trim)

# 동별 업소 개수 확인 : 도수분포표 생성 - table() 함수
# 도수분포표를 데이터프레임으로 변환

# 파이프 연산자 사용
library(dplyr)

#table() 함수 이용하여 도수 분포표 작성
addr_count <- addr_trim %>% table()
head(addr_count)
class(addr_count)

# 데이터프레임까지 생성

addr_count <- addr_trim %>% table() %>% data.frame()
head(addr_count)
class(addr_count)
#     .     Freq
# 1 남가좌동 2456
# 2   냉천동  269
# 3   대신동  150
# 4   대현동 1589
# 5   미근동  207
# 6   봉원동    5

# 변수명 변경
# . ->상호명
# Freq ->빈도

# 변수명 변경하는 함수 : rename(데이터, '변경후이름'='변경전이름')
addr_count <- rename(addr_count,'상호명'='.','빈도'='Freq')
head(addr_count)

View(addr_count)

# 트리맵(Tree Map)
# 데이터가 갖는 계층구조를 타일 모양으로 표현한 것
# 타일은 계층적 속성을 가지며
# 계층은 색상으로 표현됨
# treemap 패키지 
# treemap() 함수 사용하여 표현
# treemap(데이터 세트, 
#            index=구분 열, 	# 계층을 선언하는 매개변수
#            vSize= 분포 열, 	# 타일 크기       
#            vColor=색상, 	# 타일 색상
#            # title=제목)
#03. treemap 패키지 설치 및 로드
install.packages("treemap")
library(treemap)

#04. 실제 데이터를 내림차순으로 정렬하여
# 트리맵이 제대로 표현되었는지 확인
arrange(addr_count,desc(빈도)) %>% head()





















