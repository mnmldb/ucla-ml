# preparation
RNGkind(sample.kind = "Rounding")
set.seed(1)
sample(10)

# question 1
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

# answer
plot(prune.carseats)
text(prune.carseats, pretty=0) # Price and ShelveLoc

# question 2
table(tree.pred, High.test)
(94 + 60) / 200 # 77% 

# question 3
summary(tree.carseats) # 7 variables are used
dim(Carseats) # 12 - 1 = 11 variables are available

prune.carseats2 <- prune.misclass(tree.carseats, best=13)
tree.pred2 <- predict(prune.carseats2, Carseats.test, type="class")
table(tree.pred2, High.test)
(87 + 59) / 200 # 73%

# question 4
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree.boston) # lstat, rm, dis

# question 5
set.seed(1)
library(randomForest)
rf.boston <- randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
boston.test <- Boston[-train,"medv"]
mean((yhat.rf - boston.test)^2) # 11.66454

# question 6
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=5, shrinkage=0.1)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2) # 10.36727

# question 7
set.seed(1)
boost.boston2 <- gbm(medv~., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=5, shrinkage=0.01)
yhat.boost2 <- predict(boost.boston2, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost2 - boston.test)^2) # 10.23728