# 사전을 이용한 감성 분석
library(KoNLP)
library(wordcloud)
library(wordcloud2)

load('comments.Rdata')
load('score.Rdata')
head(comments,2)
head(score,10)

posi <- c('합리적인','호강','맛있','정말강추','좋았','최고','맛있습니',
          '친절하네','친절하','친절합니','진짜맛','정말강','매력있어요',
          '강추','합리적')
nega <- c("맛없",'낚였네요','밥맛뚝뚝','맛은별로','접으세요','싫','시켜먹',
          '없으신가봐요','절대','더럽','미숙함','썰렁','맛도없다','불친절',
          '망','꺼지라','꽝','못하네요', '가관','비추이','느려터짐', '비위생',
          '싫은','접으세요','')

comments[2]
# 형태소를 보고 계속 추가
nega_txt <- comments[score==1][1:10]
nega_txt
SimplePos09(nega_txt)
posi_txt <- comments[score==5][1:10]
posi_txt
SimplePos09(posi_txt)

# --sample을 추출해서 확인
sample_index <- sample(1: length(comments),1000)
sample_index


# 데이터 추출
sample_com <- comments[sample_index]
head(sample_com)

# SimplePos09('꺼지라그래 서비스 진짜 꽝임')
sample_score <- score[sample_index]
head(sample_score)


# ----------- 형태소 분석기 돌리는 코드
com_list <- list()
for (i in 1:length(sample_com)) {
  if(class(try(s_token <- SimplePos09(sample_com[i])))=='try-error'){
    com_list[[i]] <- NA
  } else {
    com_list[[i]] <- s_token
  }
  cat('\n',i)
}

a <- com_list[[2]]

# 함수로 구현
sel <- function(x){
  sapply(str_split(x,"/"),function(x){x[1]})
}

com_list2 <- sapply(com_list,sel)
head(com_list2)

nega[(nega %in% com_list2[[4]])] # [1] '절대','더럽' : -2점
sum(nega %in% com_list2[[4]])
posi[(posi %in% com_list2[[4]])] # [1] '절대','더럽' : -2점
sum(posi %in% com_list2[[4]])


# 모든 리뷰에 대하여 형태소를 확인
posi_list <- list()
nega_list <- list()
score_list <- c()

for(j in 1: length(com_list2)) {
  posi_list[[j]] <- posi[(posi %in% com_list2[[j]])]
  nega_list[[j]] <- nega[(nega %in% com_list2[[j]])]
  posi_score <- sum(posi %in% com_list2[[j]]) # 긍정점수
  nega_score <- -sum(nega %in% com_list2[[j]]) # 부정점수
  text_score <- posi_score + nega_score # 긍/부정 합산
  score_list[j] <- text_score
  cat("\n",j)
}

hist(score_list)
final_score <- ifelse((score_list) > 0, "긍정", '부정')
table(final_score)

# 예측결과 확인
# sample_score 파일에 평점이 있고 추출은 sample_com과 같은 indext에서 추출
# 3점보다 높으면 긍정, 아니면 부정
true_score <- ifelse(sample_score>3,'긍정','부정')
table(true_score)

# 예측율 계산
sum(final_score==true_score)/1000 
# 기존 평점이 긍정이었는데, 형태소 분석을 한 점수도 긍정인지 체크

# 예측이 틀린 데이터를 확인해서 역추적
which(final_score != true_score)

index <- 823
sample_score[823] # 수집
sample_com[823] # 수집
true_score[823] # 수집
score_list[823] # 예측 -> 긍정어가 없어 0점이 나와 부정으로 간주됨
final_score[823] # 예측

# ------------ 사전 추가해서 다시 해보기 -------------- 
posi_dic <- readLines('posi_dict.txt', encoding = 'UTF-8')
head(posi_dic,50)
nega_dic <- readLines('nega_dict.txt',encoding='UTF-8')
head(nega_dic,50)

posi_dic_list <- list()
nega_dic_list <- list()
score_dic_list <- c()

for(j in 1: length(com_list2)) {
  posi_dic_list[[j]] <- posi_dic[(posi_dic %in% com_list2[[j]])]
  nega_dic_list[[j]] <- nega_dic[(nega_dic %in% com_list2[[j]])]
  posi_dic_score <- sum(posi_dic %in% com_list2[[j]]) # 긍정점수
  nega_dic_score <- -sum(nega_dic %in% com_list2[[j]]) # 부정점수
  text_dic_score <- posi_dic_score + nega_dic_score # 긍/부정 합산
  score_dic_list[j] <- text_dic_score
  cat("\n",j)
}

hist(score_dic_list)
final_dic_score <- ifelse((score_dic_list) > 0, "긍정", '부정')
table(final_dic_score)

# 예측결과 확인
# sample_score 파일에 평점이 있고 추출은 sample_com과 같은 indext에서 추출
# 3점보다 높으면 긍정, 아니면 부정
true_score <- ifelse(sample_score>3,'긍정','부정')
table(true_score)

# 예측율 계산
sum(final_score==true_score)/1000 
# 기존 평점이 긍정이었는데, 형태소 분석을 한 점수도 긍정인지 체크

# 예측이 틀린 데이터를 확인해서 역추적
which(final_score != true_score)
