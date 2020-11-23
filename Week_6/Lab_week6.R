### 4.6.1 The Stock Market Data ###
# Smarket data
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

# pairwise correlations
cor(Smarket) # error
cor(Smarket[, -9])

# plot
attach(Smarket)
plot(Volume)

### 4.6.2 Logistic Regression ###
# fit logistic regression
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fits)

# access coefficients
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[, 4]

# training data probability
glm.probs <- predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)

# predict
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] <- "Up"

# confusion matrix
table(glm.pred, Direction)
(507 + 145) / 1250
mean(glm.pred == Direction)

# preparation for yielding a more realistic error rate
train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# logistic regression with the subset
glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fits, Smarket.2005, type="response")

# see the performance
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

# refit the model without unnecessary variables
glm.fits <- glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fits, Smarket.2005, type="response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
106 / (106 + 76)

# predict the returns associated with particular values
predict(glm.fits, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")

### 4.6.3 Linear Discriminant Analysis ###
# basic LDA
library(MASS)
lda.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)

# prediction
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)

# result
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

# recreate the prediction
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

# posterior probability output
lda.pred$posterior[1:20, 1]
lda.class[1:20]

# change the threshold
sum(lda.pred$posterior[,1] > .9)

### 4.6.4 Quadratic Discriminant Analysis ###
# basic qda
qda.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
qda.fit

# prediction
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

### 4.6.5 K-Nearest Neighbors ###
# prepare variables
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

# fitting and prediction (k = 1)
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83 + 43) / 252

# fitting and prediction (k = 3)
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
(48 + 87) / 252

### 4.6.6 An Application to Caravan Insurance Data ###
# data
dim(Caravan)
attach(Caravan)
summary(Purchase)

# starndardize the data
standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

# prediction
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean(test.Y != knn.pred)
mean(test.Y != "No")

# KNN with k = 1
table(knn.pred, test.Y)
9 / (68 + 9)

# KNN with k = 3 and 5
knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
5 / 26

knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
4 / 15

# logistic regression with 0.5 and 0.25 threshold
glm.fits <- glm(Purchase~., data=Caravan, family=binomial, subset=-test)
glm.probs <- predict(glm.fits, Caravan[test,], type="response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .5] = "Yes"
table(glm.pred, test.Y)

glm.pred[glm.probs > .25] = "Yes"
table(glm.pred, test.Y)
11 / (22 + 11)
