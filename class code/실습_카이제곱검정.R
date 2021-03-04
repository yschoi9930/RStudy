#### 실습 : 범주형 데이터 분석

## 1.

# 1-1)
line <- matrix(c(62,35,28,40,72,71,
                 37,37,57,61,76,24),nrow=3,
               dimnames = list(line=c("1","2","3"),
                               combine=c("A","B","C","D")))
line

# 1-2)

table<-addmargins(line)
table


# 1-3)

pro_table <-prop.table(line, margin = 2)
pro_table

margin.table(prop.table(line),1)
margin.table(prop.table(line),2)

# 1-4)
# 생산라인 1에는 A유형의 결함이 많으며,
# 생산라인 II에는 D유형의 결함이 많고
# 생산라인 III에는 B유형의 결함이 많음

barplot(line, beside = T, legend=c("line1","line2",'line3'))

barplot(t(line), beside = TRUE)
barplot(t(pro_table), beside = TRUE)


# 1-5)
chi_line <- chisq.test(line)
chi_line

# p-value = 1.981e-10 유의수준 0.05보다 매우 작아 
# 라인별로 결함유형이 동일하다는 귀무가설을 기각함

# 1-6)
chi_line$expected


## 2.

# 2-1)
data <- matrix(c(11000, 9400, 700, 130, 30,
                 40, 30, 5, 1, 1), nrow=5,
               dimnames = list(A=c("1","2","3", "4", "5"),
                               B=c("I","II")))
data

# 2-2)
ch.result <- chisq.test(data)
ch.result$expected

# 2개의 셀에 기대빈도가 5이하이므로 pearson 카이제곱 검정 불가

# 2-3)
fisher.test(data)

# 유의 수준 0.05보다 크므로 귀무가설을 기각하지 못함
