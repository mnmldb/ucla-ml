##### Lab 1: Subset Selection Methods #####

# need for use leaps package
update.packages(ask = FALSE)
install.packages('leaps')

### 6.5.1 Best Subset Selection ###
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)

# missing data
sum(is.na(Hitters$Salary))

# remove all of the rows that have missing values
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

# subset selection
library(leaps)
regfit.full <- regsubsets(Salary~., Hitters)
summary(regfit.full)

# return as many variables as desired
regfit.full <- regsubsets(Salary~., Hitters, nvmax=19)
reg.summary <- summary(regfit.full)

# return scores
names(reg.summary)

# rsq
reg.summary$rsq

# plot rss and adjr2 to decide which model to select
par(mfrow=c(2, 2))
plot(reg.summary$rss, xlab='Number of Variables', ylab='RSS', type='l')
plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')

# find the maximum point for rsq
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col='red', cex=2, pch=20)

# plot cp and bic
plot(reg.summary$cp, xlab='Number of Variables', ylab='Cp', type='l')
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col='red', cex=2, pch=20)

which.min(reg.summary$bic)
plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
points(6, reg.summary$bic[6], col='red', cex=2, pch=20)

# display the selected variables for the best model
plot(regfit.full, scale='r2')
plot(regfit.full, scale='adjr2')
plot(regfit.full, scale='Cp')
plot(regfit.full, scale='bic')

# see the coefficient estimates associated with the model
coef(regfit.full, 6)

### 6.5.2 Forward and Backward Stepwise Selection ###
# forward stepwise selection
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method='forward')
summary(regfit.fwd)

# backward stepwise selection
regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19, method='backward')
summary(regfit.bwd)

# difference between forward and backward
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)

### 6.5.3 Choosing Among Models Using Validation Set Approach and Cross-Varidation ###
# create boolean vectors
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), rep=TRUE)
test <- (!train)

# train the model with the training set
regfit.best <- regsubsets(Salary~., data=Hitters[train,], nvmax=19)

# create the matrix 
test.mat <- model.matrix(Salary~., data=Hitters[test,])

# loop for each size
val.errors <- rep(NA, 19)
for (i in 1:19){
  coefi <- coef(regfit.best, id=i)
  pred <- test.mat[, names(coefi)]%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}

# see the best model
val.errors
which.min(val.errors)
coef(regfit.best, 10)

# create function for prediction
predict.regsubsets <- function(object, newdata, id, ...){
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id=id)
  xvars <- names(coefi)
  mat[, xvars]%*%coefi
}

# 10 variable mode on the full data set
regfit.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(regfit.best, 10)

# prepare to choose the best model
k <- 10
set.seed(1)
folds <- sample(1:k, nrow(Hitters), replace=TRUE)
cv.errors <- matrix(NA, k, 19, dimnames=list(NULL, paste(1:19)))

# loop for cross-validation
for (j in 1:k){
  best.fit <- regsubsets(Salary~., data=Hitters[folds!=j,], nvmax=19)
  for (i in 1:19){
    pred <- predict(best.fit, Hitters[folds==j,], id=i)
    cv.errors[j, i] <- mean((Hitters$Salary[folds==j] - pred)^2)
  }
}

# average over the colums
mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

# perform the best subset selection on the full data set
reg.best <- regsubsets(Salary~., data=Hitters, nvmax=19)
coef(reg.best,11)

##### 6.6 Lab 2: Ridge Regression and the Lasso #####
# preparation
x <- model.matrix(Salary~., Hitters)[, -1]
y <- Hitters$Salary

### 6.6.1 Ridge Regression ###
# package
install.packages('glmnet') # for the first time
library(glmnet)

# ridge
grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)

# coefficient
dim(coef(ridge.mod))

# lambda = 11498
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]

# lambda = 705
ridge.mod$lambda[60]
coef(ridge.mod)[, 60]

# coefficients for the new lambda
predict(ridge.mod, s=50, type='coefficients')[1:20, ]

# training data and test data
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2) # the vector of numbers 
test <- (-train)
y.test <- y[test]

# train and test model
ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred=predict (ridge.mod ,s=4, newx=x[test ,])
mean((ridge.pred -y.test)^2)

# a model with just an intercept
mean((mean(y[train]) - y.test)^2)

# the same result as a model with just an intercep if we set lambda very large
ridge.pred=predict (ridge.mod ,s=1e10, newx=x[test ,])
mean((ridge.pred -y.test)^2)

# comparison to the leaset squares regression
ridge.pred <- predict(ridge.mod, s=0, newx=x[test, ], exact=T) # error
ridge.pred <- predict(ridge.mod, s=0, newx=x[test, ]) 
mean((ridge.pred - y.test)^2)
lm(y~x, subset=train)
predict(ridge.mod, s=0, exact=T, type='coefficients')[1:20,] # error
predict(ridge.mod, s=0,  type='coefficients')[1:20,] # error

# cross validation to determine lambda
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

# the test MSE
ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred - y.test)^2)

# refit the model on the full data set
out <- glmnet(x, y, alpha=0)
predict(out, type="coefficients", s=bestlam)[1:20,]

### 6.6.2 The Lasso ###
# lasso
lasso.mod <- glmnet(x[train,], y[train], alpha=1, lambda=grid)
plot(lasso.mod)

# cross validation
set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s=bestlam, newx=x[test,])
mean((lasso.pred - y.test)^2)

# feature selection
out <- glmnet(x, y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

##### 6.7 Lab 3: PCR and PLS Regression #####
### 6.7.1 Principal Components Regression ###
# PCR
install.packages('pls') # for the first time
library(pls)
pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE, validation='CV')

# result of PCR
summary(pcr.fit)

# plot the cross-validation scores
validationplot(pcr.fit, val.type='MSEP')

# evaluate the test set performance
set.seed(1)
pcr.fit <- pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type='MSEP')

# compute the test MSE
pcr.pred <- predict(pcr.fit, x[test,], ncomp=7)
mean((pcr.pred - y.test)^2)

# refit the model on the full data set
pcr.fit <- pcr(y~x, scale=TRUE, ncomp=7)
summary(pcr.fit)

### 6.7.2 Partial Least Squares ###
# PLS
set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)

# compute the test MSE
pls.pred <- predict(pls.fit, x[test,], ncomp=2)
mean((pls.pred - y.test)^2)

# refit the model on the full data set
pls.fit <- plsr(Salary~., data=Hitters, scale=TRUE, ncomp=2)
summary(pls.fit)
