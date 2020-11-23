# Setup
library(ISLR)
Auto <- na.omit(Auto)

# Question 1
set.seed(3)
train <- sample(392, 196)
q1_1 <- lm(mpg~horsepower, data=Auto, subset=train)
q1_2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
q1_3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)

attach(Auto)
mean((mpg - predict(q1_1, Auto))[-train]^2) # 26.29134 -> 26.29
mean((mpg - predict(q1_2, Auto))[-train]^2) # 21.5041 -> 21.50
mean((mpg - predict(q1_3, Auto))[-train]^2) # 21.50825 -> 21.51

# Question 2
library(boot)
q2 <- glm(mpg~poly(horsepower, 6), data=Auto)
cv.err <- cv.glm(Auto, q2)
cv.err$delta # 18.97864 18.97765 -> 18.98 18.98

# Question 3
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10){
  q3 <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto, q3, K=10)$delta[1]
}

cv.error.10
for (i in 2:10){
  print(cv.error.10[i-1] - cv.error.10[i])
} # 1 to 2: 5.015958

# Question 4
set.seed(2)
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y)))
}
boot(Portfolio, alpha.fn, R=500) # 0.5758321 -> 0.58

# Question 5
set.seed(2)
boot(Portfolio, alpha.fn, R=100) # 0.09928741
set.seed(2)
boot(Portfolio, alpha.fn, R=5000) # 0.09071149

# Question 6
set.seed(1)
boot.fn <- function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef

# Question 7
set.seed(1)
boot.fn <- function(data, index)
  coefficients(lm(mpg~cylinders+I(cylinders^2), data=data, subset=index))
boot(Auto, boot.fn, 1000) # 2.0519622 0.1678759 -> 2.05 0.17

summary(lm(mpg~horsepower+I(horsepower^2), data=Auto)) # horsepower is better
summary(lm(mpg~cylinders+I(cylinders^2), data=Auto))
