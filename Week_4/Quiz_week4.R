# check environment
set.seed(1)
sample(10) # 9  4  7  1  2  5  3 10  6  8

# setup
RNGkind(sample.kind = "Rounding")
set.seed(1)
sample(10) # 3  4  5  7  2  8  9  6 10  1

### Question 1
# import
library(ISLR)
names(Hitters)
dim(Hitters) # 322 20

# remove nulls
Hitters <- na.omit(Hitters)
dim(Hitters) # 263  20

# train the model
library(leaps)
regfit.full <- regsubsets(Salary~., Hitters, nvmax=19)

# coefficient comparison
coef(regfit.full, 7) # (Intercept)         Hits        Walks       CAtBat        CHits       CHmRun    DivisionW      PutOuts 
coef(regfit.full, 8) # (Intercept)        AtBat         Hits        Walks       CHmRun        CRuns       CWalks    DivisionW      PutOuts

### Question 2
reg.summary <- summary(regfit.full)

plot(reg.summary$adjr2, xlab='Number of Variables', ylab='Adjusted RSq', type='l')
which.max(reg.summary$adjr2) # 11
reg.summary$adjr2[11] # 0.5225706

plot(reg.summary$bic, xlab='Number of Variables', ylab='BIC', type='l')
which.min(reg.summary$bic) # 6
reg.summary$bic[6] # -147.9169

### Question 3
# alpha=0: ridge
# alpha=1: lasso

### Question 4
library(glmnet)

x <- model.matrix(Salary~., Hitters)[, -1]
y <- Hitters$Salary

grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
plot(ridge.mod)

coef(ridge.mod)[,20]
coef(ridge.mod)[,50]
coef(ridge.mod)[,100] # DivisionW

coef(ridge.mod)[16,100] # DivisionW 16 - 1 (Intercept) = 15

### Question 5
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2) # the vector of numbers 
test <- (-train)
y.test <- y[test]

set.seed(1)
cv.out <- cv.glmnet(x[train,], y[train], alpha=1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam # 16.78016

### Question 6
lasso.mod <- glmnet(x[train,], y[train], alpha = 1, lambda = grid)
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test,])
mean((lasso.pred - y.test)^2) # 100743.4
out <- glmnet(x, y, alpha = 1,lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20,]

lasso.coef # 7
length(lasso.coef[lasso.coef!=0]) - 1

### Question 7
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type='MSEP')
summary(pcr.fit) # 4 comps

