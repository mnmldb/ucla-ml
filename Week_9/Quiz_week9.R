# Preparation
RNGkind(sample.kind = "Rounding")
set.seed(1)
sample(10)
library(e1071)

# Question 1
set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] <- x[y==1,] + 1
plot(x, col=(3-y))
dat <- data.frame(x=x, y=as.factor(y))

svmfit <- svm(y~., data=dat, kernel="linear", cost=1, scale=FALSE)
plot(svmfit, dat)
summary(svmfit) # Number of Support Vectors:  10

# Question 2
set.seed(1)
tune.out <- tune(svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out) # 1e-03, 1e-02

# Question 3

# Question 4
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame(x=x, y=as.factor(y))

train <- sample(200, 100)

svmfit.20 <- svm(y~., data=dat[train,], kernel="radial", gamma=20, cost=1)
svmfit.10 <- svm(y~., data=dat[train,], kernel="radial", gamma=10, cost=1)
svmfit.2 <- svm(y~., data=dat[train,], kernel="radial", gamma=2, cost=1)
svmfit.0.1 <- svm(y~., data=dat[train,], kernel="radial", gamma=.1, cost=1)

par(mfrow=c(2,2))
plot(svmfit.20, dat[train,]) # most over fitting
plot(svmfit.10, dat[train,])
plot(svmfit.2, dat[train,])
plot(svmfit.0.1, dat[train,])

# Question 5
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 0.5), gamma=c(0.1, 0.5, 1, 1.5, 2, 3, 4, 5)))
summary(tune.out) # C: 0.5, Gamma: 0.5

# Question 6
set.seed(1)
tune.original <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
tune.new <- tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1, 0.5), gamma=c(0.1, 0.5, 1, 1.5, 2, 3, 4, 5)))
summary(tune.original) # C: 1, Gamma: 2
summary(tune.out) # C: 0.5, Gamma: 0.5

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
svmfit.flex <- svm(y~., data=dat[train,], kernel="radial", gamma=0.5, cost=0.5, decision.values=T)
fitted <- attributes(predict(svmfit.flex, dat[train,], decision.values = TRUE))$decision.values
rocplot(fitted, dat[train,"y"], add=T, col="red")

# ROC on the test data
fitted <- attributes(predict(svmfit.opt, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], main="Test Data")
fitted <- attributes(predict(svmfit.flex, dat[-train,], decision.values = T))$decision.values
rocplot(fitted, dat[-train,"y"], add=T, col="red")

# Question 7
library(ISLR)
dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out.5 <- svm(y~., data=dat, kernel="linear", cost=5)
out.10 <- svm(y~., data=dat, kernel="linear", cost=10)
out.20 <- svm(y~., data=dat, kernel="linear", cost=20)
out.50 <- svm(y~., data=dat, kernel="linear", cost=50)

table(out.5$fitted, dat$y)
table(out.10$fitted, dat$y)
table(out.20$fitted, dat$y)
table(out.50$fitted, dat$y)

dat.te <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.5 <- predict(out.5, newdata=dat.te)
pred.10 <- predict(out.10, newdata=dat.te)
pred.20 <- predict(out.20, newdata=dat.te)
pred.50 <- predict(out.50, newdata=dat.te)

table(pred.5, dat.te$y)
table(pred.10, dat.te$y)
table(pred.20, dat.te$y)
table(pred.50, dat.te$y)
