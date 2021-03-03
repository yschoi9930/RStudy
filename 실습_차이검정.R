######################
## 실습문제6


# 1 두 모집단 평균차이 검정(모분산을 아는 경우)

# 1-1)
x <- c(91,115,96,90,120,108,82,118,
       105,97,113,119,90,106,116,92,
       108,115,114,101,96,96,96,97,
       89,99,90,85,91,124,93,90,
       100,100,91,96,120,78,96,114)
y <- c(102,117,82,104,77,110,93,115,
       75,103,126,79,81,118,93,106,
       104,97,115,80,78,109,116,104,
       102,137,99,100,113,112,96,106,
       76,102,111,105,85,125,77,111)

# 1-2)
par(margin(5,2,2,2))
box <- boxplot(x, y, names=c("A", "B"))

#1-3)
mean(x)
sd(x)
mean(y)
sd(y)


#1-4)
shapiro.test(x)
shapiro.test(y)

#1-5),6),7)
mean_z.test_two <- function(x, y, sigma1, sigma2,
                            conf.level, alternative) {
  n <- length(x)
  m <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  diff <- mean_x - mean_y
  z_alpha_half <- qnorm((1-conf.level)/2,
                        lower.tail = FALSE)
  var_mean <- (sigma1^2/n)+(sigma2^2/m)
  lower_limit <- diff - z_alpha_half * sqrt(var_mean)
  upper_limit <- diff + z_alpha_half * sqrt(var_mean)
  z_statistic <- diff / sqrt(var_mean)
  p_value_R <- pnorm(z_statistic, lower.tail = FALSE)
  p_value_L <- pnorm(z_statistic, lower.tail = TRUE)
  p_value_two <- pnorm(abs(z_statistic), lower.tail = FALSE)
  
  if(alternative == "two_sided"){
    p_value <- 2*p_value_two
    cat("Two sample Z-test", "\n")
  }else if(alternative == "greater"){
    p_value <- p_value_R
    cat("Two sample Z-test", "\n")
  }else {
    p_value <- p_value_L
    cat("Two sample Z-test", "\n")
  }
  result <- ifelse(p_value < (1-conf.level), "Reject H0", "Accept H0")
  cat("Z_statistics = ", z_statistic, "\n")
  cat("p_value = ", p_value, "\n")
  if(alternative == "two_sided"){
    cat("alternative hyphothesis: true difference in mean is not equal to 0 ", "\n")
  }else if(alternative == "greater"){
    cat("alternative hyphothesis: true difference in mean is greater than 0 ", "\n")
  }else{
    cat("alternative hyphothesis: true difference in mean is less than 0 ", "\n")
  }
  cat(conf.level * 100, "% confidence interval : (", lower_limit, ", ", upper_limit, ") \n")
  cat("sample mean of x = ", mean_x, ", sample mean of y = ", mean_y, "\n")
  cat("sample estimate difference of mean=", mean_x - mean_y, "\n")
  cat("result = ", result)
}

sigma1 <- sqrt(100)
sigma2 <- sqrt(225)
conf.level <- 0.95
alternative <- "two_sided"
mean_z.test_two(x=x, y=y, 
                sigma1 = sigma1, sigma2=sigma2, 
                conf.level=conf.level, 
                alternative=alternative )

#5) sample estimate difference of mean= -0.1 
#6) 95 % confidence interval : ( -5.686757 ,  5.486757 )
#7) result =  Accept H0


#2.두 모집단의 평균차이 검정
#  (모분산을 모르는 경우)

# 2-1)
x <- c(39.1, 38.4, 32.5,
       42.3, 43.9, 39.6,
       40.2, 38.0, 42.3,
       34.6, 41.4, 38.7,
       41.9, 37.9, 36.9)

y <- c(55.3, 50.2, 47.0,
       65.2, 44.0, 46.7,
       48.8, 36.6, 57.5,
       51.1, 46.7, 42.6,
       65.6, 56.4, 38.4)

# 2-2)
boxplot(x, y, names = c("A","B"))

# 2-3)
mean(x)
sd(x)
mean(y)
sd(y)

#2-4)
shapiro.test(x)
shapiro.test(y)

#2-5)
var.test(x, y)
# p-value = 0.0003937
# 이분산

#2-6)
# mean of x: 39.18
# mean of y: 50.14
# sample estimate difference of mean: -10.96

#2-7)
# 95% confidence interval: -15.92532  -5.99468

#2-8)
t_test <- t.test(x, y, mu=0,
                 var.equal = FALSE, 
                 paired = FALSE,
                 alternative = "two.sided")
t_test
summary(t_test)

#2-6)
Mean <- t_test$estimate
diff <- Mean[1] - Mean[2]
names(diff) <- c("difference in Mean")
diff



#3. 쌍체비교
#3-1)
x <- c(71, 72, 66, 69,
       69, 69, 70, 67,
       72, 67, 71, 72,
       69, 69, 70)
y <- c(69, 67, 68, 68,
       70, 67, 67, 64,
       65, 64, 66, 70,
       70, 67, 66)
di <- x - y

#3-2)
boxplot(di, horizontal = TRUE)
box <- boxplot(di, horizontal = TRUE)

box$out

#3-3)
shapiro.test(di)

#3-4)
mean(di)
sd(di)

#3-5), 6)
t.test(x, y, mu=0, paired = TRUE,
       alternative = "two.sided")

t.test(x, y, mu=0, paired = TRUE,
       alternative = "greater")

#3-5)
# 95% confidence interval: 0.9822447 3.6844220

#3-6)
# p-value = 0.002358


#4. 두 모비율 비교
products <- matrix(c(294, 6, 390, 10),
                   nrow = 2, byrow = TRUE)
rownames(products) <- c("line I", "line II")
colnames(products) <- c("good", "bad")

prod_prop <- prop.table(products, margin=1)
prod_prop

barplot(prod_prop, col=c("green", "blue"), beside=TRUE)
legend(locator(1), c("line I", "line II"), fill=c("red", "blue"))

#4-2)3)
prop_test <- prop.test(products, p=NULL, 
                       alternative = "two.sided",
                       correct = FALSE)
prop_test

#4-2)
# 95% confidence interval: -0.01702417  0.02702417


#4-4)

prop_test_correct <-prop.test(products, p=NULL,
                              alternative = "two.sided",
                              correct = TRUE)
prop_test_correct