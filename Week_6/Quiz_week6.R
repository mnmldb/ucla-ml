# preparation
RNGkind(sample.kind = "Rounding")
set.seed(1)
sample(10)

# question 1
library(ISLR)
attach(Smarket)

train <- (Year < 2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005) # 252 x 9
Direction.2005 <- Direction[!train]

q1.fits <- glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
summary(q1.fits)

# question 2
q2.probs <- predict(q1.fits, Smarket.2005, type="response")
q2.pred <- rep("Down", 252)
q2.pred[q2.probs > .5] = "Up"
table(q2.pred, Direction.2005)
mean(q2.pred == Direction.2005) # 0.5595238
(35 + 106) / 252 # 0.5595238

# question 3
35 / (35 + 76) # 0.3153153

# question 4
library(MASS)
q4.fit <- lda(Direction~Lag1+Lag2, data=Smarket, subset=train)
q4.pred <- predict(q4.fit, Smarket.2005)
q4.pred$posterior[,2]
max(q4.pred$posterior[,2]) # 0.5422133
min(q4.pred$posterior[,2]) # 0.479765

# question 5
q5.fit <- qda(Direction~Lag1+Lag2, data=Smarket, subset=train)
q5.class <- predict(q5.fit, Smarket.2005)$class
table(q5.class, Direction.2005)
121 / (121 + 20) # 0.858156

# question 6
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
q6.pred <- knn(train.X, test.X, train.Direction, k=4)
table(q6.pred, Direction.2005)

66 / (45 + 66) # False Positive 0.5945946
1 - 83 / (58 + 83) # type 2 0.4113475

# question 7
attach(Caravan)
standardized.X <- scale(Caravan[,-86])
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

lda.fit=lda(Purchase~.,data=Caravan,subset=-test)
lda.probs=predict(lda.fit, Caravan[test,])$posterior[,2]

q7_1.pred <- rep("No", 1000)
q7_1.pred[lda.probs > .3] = "Yes"
table(q7_1.pred, test.Y)
9 / (19 + 9) # 0.3214286

q7_2.pred <- rep("No", 1000)
q7_2.pred[lda.probs > .2] = "Yes"
table(q7_2.pred, test.Y)
16 / (46 + 16) # 0.2580645
