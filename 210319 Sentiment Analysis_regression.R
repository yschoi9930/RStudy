# DTM 형식으로 데이터를 전달
# 1) 형태소 분석기 없이 공백으로 구분해서 데이터를 전달
# 2) 형태소 분석기를 사용해서 구분한 후에 데이터를 전달

library(KoNLP)
library(stringr)
library(wordcloud2)

load('comments.Rdata')
load('score.Rdata')
head(comments,2)
head(score,10)

#sample을 추출해서 확인
sample_index <- sample(1: length(comments),1000)
sample_index

# 데이터 추출
sample_com <- comments[sample_index]
head(sample_com)

# SimplePos09('꺼지라그래 서비스 진짜 꽝임')
sample_score <- score[sample_index]
head(sample_score)

# 공백으로 분리 후 작업
sample_com_space <- str_split(sample_com, " ")
word_list <- unlist(sample_com_space)
terms <- table(word_list)

# 공백을 기준으로 분리한 단어 중 발생 빈도가 높은 300개를 추출해서 terms로 사용
terms_fin <- sort(terms,decreasing = TRUE)[1:300]
head(terms_fin)

terms_fin <- names(terms_fin)[-1] # 가장 높은 빈도는 공백이라서 제외

# DTM을 작성
sample_com[1]
#                               너무 정말 진짜 맛있어요 여기 맛있는 
# 짬뽕이 맛있는 집입니다 제 ...  0    0     0     0       0     1

dtm <- matrix(0,ncol=length(terms_fin),nrow=length(sample_com))
colnames(dtm)<-terms_fin
head(dtm) # 매칭되면 1 아니면 0으로 값을 변경

# ------------------------------
i <-1
sample_com[i]
sample_com_space[[i]]

dtm[i,terms_fin %in% sample_com_space[[i]]] <- 1
head(dtm,1)

# ------------ 모든 sample com에 대해서 dtm 설정

for (i in 1:length(sample_com)) {
  dtm[i,terms_fin %in% sample_com_space[[i]]] <- 1
  cat('\n',i)
}

dtm[1000,]

# 평점을 긍/부 분류 3보다 크면 긍정 1, 그 외는 0 
sample_score2 <- ifelse(sample_score>3,1,0)
head(sample_score2)

# dtm의 각 행을 더해서 0 이 아닌 행의 index 추출
# terms가 있는 행만 추출
data_index <- apply(dtm,1,sum) != 0
data_index

dtm_t<-dtm[data_index,]
sample_com_fin <- sample_com_space[data_index]
dim(dtm_t)
length(sample_com_fin)
length(score_t)
score_t <- sample_score2[data_index]
length(score_t)


install.packages("glmnet")
library(glmnet)

# alpha = 1 lasso alpha() ridge
# 하이퍼 파라미터로 lamda를 사용하고 있음
# 값이 0과 1를 갖는 이항인 경우에는 famlily binomial로 설정해야함
fit <- glmnet(dtm_t,score_t,alpha=1,family='binomial')
fit$beta
s <- fit$lambda[length(fit$lambda)]
s
coef(fit,s=s) # 회귀 계수, 긍부정 지수 산출

# sample 데이터를 train/test로 분류
train <- sample(1:length(score_t),length(score_t)*0.7)

# 70프로 학습데이터 이용
fit <- glmnet(dtm_t[train,],train,alpha=1,family='binomial')
s <- fit$lambda[length(fit$lambda)]
coef(fit,s=s)

# 예측
pred <- predict(fit,dtm_t[-train,],s=s)

pred_fin <- ifelse(pred > 0,1,0)

sum(pred_fin==score_t[-train])/length(pred_fin)

# -------- 위에는 형태소 분석 없이 진행한 것 --------------