##### 8.3 Lab: Decision Trees #####
### 8.3.1 Fitting Classification Trees
install.packages("tree")
library(tree)

# create a binary variable
library(ISLR)
attach(Carseats)
High <- as.factor(ifelse(Sales <= 8, "No", "Yes"))

# merge with the data frame
Carseats <- data.frame(Carseats, High)

# use tree
tree.carseats <- tree(High~.-Sales, Carseats)

# summary
summary(tree.carseats)

# visualize
plot(tree.carseats)
text(tree.carseats, pretty=0)

# other visualization
tree.carseats

# test data
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High~.-Sales, Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(86 + 57) / 200

# cv and pruning
set.seed(3)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
names(cv.carseats)
cv.carseats

# plot the error rate
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")

# prune the tree
prune.carseats <- prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# predict with the pruned tree
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(94 + 60) / 200

# simulation with the larger value of best
prune.carseats <- prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(86 + 62) / 200

### 8.3.2 Fitting Regression Trees
# fit the regression tree
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)

# plot the tree
plot(tree.boston)
text(tree.boston, pretty=0)

# cv and pruning
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')

# prune the tree
prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# test
yhat <- predict(tree.boston, newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)

### 8.3.3 Bagging and Random Forests
# bagging
install.packages(("randomForest"))
library(randomForest)
set.seed(1)
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston

# test
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)

# change the number of trees
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag - boston.test)^2)

# random forest
set.seed(1)
rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf - boston.test)^2)

# feature importance
importance(rf.boston)

# plot the importance measures
varImpPlot(rf.boston)

### 8.3.4 Boosting
# boosting
install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)

# summary
summary(boost.boston)

# partical dependence plot
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")

# predict
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2)

# different shrinkage parameter
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2)
