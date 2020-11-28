##### 9.6 Lab: Support Vector Machines #####
### Preparation ###
rm(list=ls())

### 9.6.1 Support Vector Classifier ###
# create sample data
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=(3-y))

# fit support vector classifier
dat <- data.frame(x=x, y=as.factor(y))
install.packages("e1071")
library(e1071)
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=FALSE)

# plot the decision boundary
plot(svmfit, dat)

# access support vectors
svmfit$index

# summary
summary(svmfit)

# fit support vector classifier with a smaller cost
svmfit <- svm(y~., data=dat, kernel="linear", cost=0.1, scale=FALSE)
plot(svmfit, dat)
svmfit$index

# cross validation
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)

# best model
bestmod <- tune.out$best.model
summary(bestmod)

# create test data
xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1, 1), 20, rep=TRUE)
xtest[ytest==1,] <- xtest[ytest==1,] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

# prediction
ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)

# prediction with another cost
svmfit <- svm(y~., data=dat, kernel="linear", cost=.01, scale=FALSE)
ypred <- predict(svmfit, testdat)
table(predict=ypred, truth=testdat$y)

# prepare new data which are linearly separable
x[y==1,] <- x[y==1,] + 0.5
plot(x, col=(y+5)/2, pch=19)

# support vector classifier with large cost
dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear", cost=1e5)
summary(svmfit)
plot(svmfit, dat)

# prediction with new test data
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
summary(svmfit)
plot(svmfit, dat)

### 9.6.2 Support Vector Machine ###
# generate some data with a non-linear class boundary
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))

# plot data
plot(x, col=y)

# traind the model with a radial kernel
train <- sample(200, 100)
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1)
plot(svmfit, dat[train,])

# summary
summary(svmfit)

# larger cost
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svmfit, dat[train,])

# cross validation
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(tune.out)

# result of the model with the best parameters
table(true=dat[-train, "y"], pred=predict(tune.out$best.model, newdata=dat[-train,]))

### 9.6.3 ROC Curves ###
# write a function
library(ROCR)
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

# output the fitted values
svmfit.opt <- svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.opt, dat[train,], decision.values = TRUE))$decision.values

# produce the ROC plot
par(mfrow=c(1,2))
rocplot(fitted, dat[train,"y"], main="Training Data")

# increase gamma
svmfit.flex <- svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted <- attributes(predict(svmfit.flex, dat[train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

# ROC on the test data
fitted <- attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], main="Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], add=T, col="red")

### 9.6.4 SVM with Multiple Classes ###
# create the data
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol=2))
y <- c(y, rep(0, 50))
x[y==0, 2] <- x[y==0, 2] + 2
dat <- data.frame(x=x, y=as.factor(y))
par(mfrow=c(1, 1))
plot(x, col=(y + 1))

# SVM
svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit, dat)

### 9.6.5 Application to Gene Expression Data ###
# the dimension of the data
library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

# classes
table(Khan$ytrain)
table(Khan$ytest)

# fit SVM
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm(y~., data=dat, kernel="linear", cost=10)
summary(out)
table(out$fitted, dat$y)

# prediction
dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.te <- predict(out, newdata=dat.te)
table(pred.te, dat.te$y)
