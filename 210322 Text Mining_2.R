### 형태소 분석 후 감성 분석 -----
library(KoNLP)
library(stringr)
library(wordcloud2)

load('comments.Rdata')
load('score.Rdata')

# sampledata 추출(1000개 sample 추출)

sam_index <- sample(1:length(comments),1000)
head(sam_index)

sam_com <- comments[sam_index]
sam_score <- comments[sam_index]

data_list <- list() # 리뷰 별 형태소를 분리한 후에 저장할 변수

for (i in 1:length(sam_com)){
  if(class(try(ss <- SimplePos09(sam_com[i])))=='try-error') {
    data_list[[i]] <- NA
  } else {
    data_list[[i]] <- ss
  }
  cat("\n",i)
}

## DTM 구성하기 위해 빈도수가 높은 단어를 추출

# terms 추출 (빈도수 높은 단어)
# 빈 matrix 구성 - terms 여부를 표시
#         terms1  terms2  terms3
# 리뷰1     0       1       0
# 리뷰2     1       0       0
# 리뷰3     0       1       1 

# terms가 없는 리뷰는 제외시킨다
# 나누어진 형태소에서 첫번째 단어만 추출
str_split(data_list[[1]],"/")[[1]][1]

sapply(str_split(data_list[[1]],"/"),function(x){x[1]})

a <- function(x){
  sapply(str_split(x,"/"),function(x){x[1]})
} 

a2 <- sapply(data_list,a)

# 리스트 형태 벡터 형태로 변환

words <- unlist(a2)
head(words)

# 빈도수 높은 단어 추출
tbl<- table(words)
top_words <- names(sort(tbl, decreasing = TRUE)[1:300])


# 빈 메트릭스 생성
dtm <- matrix(0,ncol=length(top_words), nrow=length(sam_com))
head(dtm)
colnames(dtm) <- top_words
head(dtm,1)

# 발생유무 기록
# terms 단어 : top_words
# 리뷰(형태소로 분리된) : a2

top_words %in% a2[[1000]]

dtm[1,top_words %in% a2[[1]]]<-1
dtm[1,]

for(i in 1:length(a2)){
  dtm[i,top_words %in% a2[[i]]]<-1
  cat('\n',i)
}
dtm[1,]

# 수집한 평점을 긍정(1), 부정(0)으로 변환, 3점초과 -> 긍정
score2 <- ifelse(sam_score>3,1,0)

# terms가 있는 행만 dtm에서 추출
d_index <- apply(dtm,1,sum) != 0
dtm_fin <-dtm[d_index,] # 학습 리뷰 데이터
head(dtm_fin)
dim(dtm_fin)
score_fin <-score2[d_index] # 리뷰에 대한 라벨
length(score_fin)

# --------------- train 데이터와 test 데이터를 분리
ss <- sample(1:length(score_fin),length(score_fin)*0.7)
head(dtm_fin[ss,]) # 학습데이터
head(dtm_fin[-ss,]) # 테스트데이터



library(glmnet)

# 하이퍼 파라미터로 lambda 파라미터 사용
fit = glmnet(dtm_fin[ss,], score_fin[ss], alpha=1, family = "binomial", nlambda = 100)
s <- fit$lambda[length(fit$lambda)]
s
coef(fit,s=s)

pred <- predict(fit, dtm_fin[-ss,],s=s)
pred

pred2 <- ifelse(pred>0,1,0)
pred2
sum(pred2 == score_fin[-ss]) / length(pred2) # 0.952862


# alpha = 0 
fit = glmnet(dtm_fin[ss,], score_fin[ss], alpha=0, family = "binomial", nlambda = 100)
s <- fit$lambda[length(fit$lambda)]
s
coef(fit,s=s)

pred <- predict(fit, dtm_fin[-ss,],s=s)
pred

pred2 <- ifelse(pred>0,1,0)
pred2
sum(pred2 == score_fin[-ss]) / length(pred2) # 0.959596

# 계수 확인
fit = glmnet(dtm_fin[ss,], score_fin[ss], alpha=1, family = "binomial", nlambda = 100)
s <- fit$lambda[length(fit$lambda)]
coef_data <- coef(fit,s=s)
class(coef_data) # [1] "dgCMatrix"
coef_data <- as.matrix(coef_data)

sort(coef_data[,1],decreasing = TRUE)[1:30]
sort(coef_data[,1],decreasing = FALSE)[1:30]   
dict <- coef_data[,1]


# -------------- 데이터 준비 (2000개의 리뷰) -------------------
sam_index <- sample(1:length(comments),2000)
dict_sam_com <- comments[sam_index] # 리뷰 샘플
dict_sam_score <- score[sam_index] # 평점 샘플

## 추출된 sample_comments 형태소 분리
data_list_dict <- list()
for (i in 1:length(dict_sam_com)){
  if(class(try(ss <- SimplePos09(dict_sam_com[i])))=='try-error') {
    data_list_dict[[i]] <- NA
  } else {
    data_list_dict[[i]] <- ss
  }
  cat("\n",i)
}

a <- function(x){
  sapply(str_split(x,"/"),function(x){x[1]})
} 

a_dict <- sapply(data_list,a)

# 모든 리뷰에 대해서 사전과 매칭
score_vec <- c()
for(i in 1:length(a_dict)){
  score_vec[i]<-sum(dict[names %in% a_dict[[i]]])
  cat('\n',i)
}

pred_score <- ifelse(score_vec>0,1,0)
true_score <- ifelse(dict_sam_score>3,1,0)

# 예측율 계산
sum(pred_score == true_score) / length(pred_score)

# ---------------------------------------------
sentment <- function(x) {
  # 형태소 나누기
  for (i in 1:length(sam_com)){
    if(class(try(ss <- SimplePos09(sam_com[i])))=='try-error') {
      data_list[[i]] <- NA
    } else {
      data_list[[i]] <- ss
    }
    cat("\n",i)
  }
  
  # 나누어진 형태소 필요부분 추출
  a <- function(x){
    sapply(str_split(x,"/"),function(x){x[1]})
  } 
  
  rv_s <- a(ss)
  
  score_vec<-sum(dict[names %in% rv_s])
  pred_score <- ifelse(score_vec>0,"긍정","부정")
  
  return(paste0("이 글의 감성값은 ", score_vec, "이며, 감성 분류결과 ", 
                pre_score,"으로 예상됩니다."))
}

sentiment("가격이 착하고 너무 맛있어요")