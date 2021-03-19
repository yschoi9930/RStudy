library(KoNLP)
library(wordcloud)
library(wordcloud2)

load('comments.Rdata')
load('score.Rdata')
head(comments,2)
head(score,2)

# 형태소 분리
s_token <- SimplePos09(comments[1:10])

# simplepos09(comments) # java 메모리 에러가 날 수 있음

sample_com <- comments[1:1000]

# 빈 list를 하나 만들고 각 방에 결과 하나씩 저장

com_list<- list()


# 문장 덩어리가 너무 크거나, 기호 문자가 들어가 있는 여러 이유로
# 형태소 분석이 진행 안됨(에러 처리를 해줘야함)

for(i in 1:length(sample_com)) {
  s_token <- simplePos09(sample_com[i])
  com_list[[i]] <- s_token
  cat('\n',i)
}

# 예외처리
for (i in 1:length(sample_com)) {
  if(class(try(s_token <- SimplePos09(sample_com[i])))=='try error'){
    com_list[[i]] <- NA
  } else {
    com_list[[i]] <- s_token
  }
  
}

length(com_list)

com_list2 <- unlist(com_list)
head(com_list2)

# ---------- 형태소 분석 ----------------
#  나눠진 형태소에서 명사만 추출 : N
# 오니기리/N+와/J : 오니기리만 추출
library(stringr)
str_split(com_list2[1],"/")[[1]][1]

words <- sapply(str_split(com_list2,"/"),function(x){x[1]})
head(words,30) # 명사뿐 아니라 [1] 위치는 전부 들어옴

# 빈도수 계산
count<- table(words)
count <- count[nchar(names(count))>1] # 한글자 단어 지움
sort(count, decreasing = TRUE) %>%  head()

sam_test <- sort(count, decreasing = TRUE)[1:100]

# -------- 나눠진 형태소에서 명사만 추출 : N
test <- str_split(com_list2[1:10],"/")
test

## 명사 추출 함수
sel_N <- function(x){
x[2]=='N'
}

 # N외에 N+인 것도 추출이 필요함
sel_N <- function(x){
  if(str_detect(x[2],"N")){
    x[1]
  } else{
  }
}

sapply(test,sel_N)

# 전체 데이터에 대해서 진행
spl_data<-str_split(com_list2,"/")
words <- sapply(spl_data, sel_N) 
# TRUE/FALSE가 필요한 곳에 값이 없습니다 -> 빈칸이 있다

sum(spl_data==" ") # 1개 존재

# 데이터가 없는 위치의 인덱스 검색
which(spl_data==" ") # 8245번
spl_data[8245] # 글자가 1개 이상인 것만 진행을 하고 나머지는 null이 반환되도록


# --- simplePos09 함수가 명사라고 인식한 단어 추출
spl_data <- str_split(com_list2,"/")
sel_N <- function(x){
  if(length(x)>1) {
    if(str_detect(x[2],"N")){
      x[1]
    } else{
    
      }
  } else{
    
  }  
}

spl_data<-str_split(com_list2,"/")
words <- sapply(spl_data, sel_N) 
words <- words %>% unlist()

count_n <- table(words)

sort(count_n, decreasing = TRUE)[1:20] # 명사
sort(count, decreasing = TRUE)[1:20] # 모든 품사

# 동사 명사 분류
sel_P <- function(x){
  if(length(x)>1) {
    if(str_detect(x[2],"P")){
      x[1]
    } else{
      
    }
  } else{
    
  }  
}

words <- sapply(spl_data, sel_P) 
words <- words %>% unlist()

count_p <- table(words)
count_p <- count_p[nchar(names(count_p))>1]

all_noun <- sort(count, decreasing = TRUE)[1:20]
noun <- sort(count_n, decreasing = TRUE)[1:20]
p <- sort(count_p, decreasing = TRUE)[1:20]

head(all_noun,20)
head(noun,20)
head(p,20)

#
