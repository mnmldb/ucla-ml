### 5.3.1 The Validation Set Approach ###
# Split the set of observations
library(ISLR)
set.seed(1)
train <- sample(392, 196)
?sample

# Fit a linear regression using only the validation set
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)

# Calculate the MSE of the validation set
attach(Auto)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
length(mpg - predict(lm.fit, Auto)) # 392
length((mpg - predict(lm.fit, Auto))[-train]) # 196

# Fit a linear regression using only the validation set (quadratic and cubic)
lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
     
lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# Different training set
set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data=Auto, subset=train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

### 5.3.2 Leave-One-Out Cross-Validation ###
# glm() function performs linear regression without passing in the family argument
glm.fit <- glm(mpg~horsepower, data=Auto)
coef(glm.fit)
lm.fit <- lm(mpg~horsepower, data=Auto)
coef(lm.fit)

# cv.glm() function for LOOCV
library(boot)
glm.fit <- glm(mpg~horsepower, data=Auto)
cv.err <- cv.glm(Auto, glm.fit)
cv.err$delta

# Repeat the fitting procedure for increasingly complex polynomial fits
cv.error <- rep(0, 5) # 0, 0, 0, 0, 0
for (i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.error

### 5.3.3 k-Fold Cross-Validation ###
# k = 10
set.seed(17)
cv.error.10 <- rep(0, 10)
for (i in 1:10){
  glm.fit <- glm(mpg~poly(horsepower, i), data=Auto)
  cv.error.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10

### 5.3.4 The Bootstrap ###
## Estimating the Accuracy of a Statistic of Interest
# Create a function that computes the statistic of interest
alpha.fn <- function(data, index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y)))
}

# Estimate alpha using all 100 observations
alpha.fn(Portfolio, 1:100)

# Randomely select 100 observations with replacement
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace=T))

# Bootstrap with boot() function
boot(Portfolio, alpha.fn, R=1000)

## Estimating the Accuracy of a Linear Regression Model
# Create a fuction for returning coefficients
boot.fn <- function(data, index)
  return(coef(lm(mpg~horsepower, data=data, subset=index))) # {} is not needed if it is only one line long

# Use boot.fn() function for Bootstrap
set.seed(1)
boot.fn(Auto, sample(392, 392, replace=T))
boot.fn(Auto, sample(392, 392, replace=T))

# Bootstrap
boot(Auto, boot.fn, 1000)

# Results from linear regression
summary(lm(mpg~horsepower, data=Auto))$coef

# Model with quadratic terms
boot.fn <- function(data, index)
  coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
set.seed(1)
boot(Auto, boot.fn, 1000)
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef
