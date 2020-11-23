### 3.6.1 Libraries ###
library(MASS) # import a library
library(ISLR) # error
install.packages("ISLR") # first install the package
library(ISLR)

### 3.6.2 Simple Linear Regression ###
fix(Boston)
names(Boston)
?Boston

# Linear Regression
lm.fit <- lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit <- lm(medv~lstat)

# Summary for the Linear Regression 
lm.fit
summary(lm.fit)

# Information stored in lm.fit
names(lm.fit)

# Coefficient
coef(lm.fit)
lm.fit$coefficients

# Coefficient intervals
confint(lm.fit)

# Prediction with the 95% confidence interval
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")

# Prediction with the 95% prediction interval
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")

# Plot
plot(lstat, medv)
abline(lm.fit) # add the least squares regression line

abline(lm.fit, lwd=3) # lwd: the width of a line
abline(lm.fit, lwd=3, col="red")
plot(lstat, medv, col="red")
plot(lstat, medv, pch=20) # pch: plotting symbols
plot(lstat, medv, pch="+")
plot(1:20, 1:20, pch=1:20)

# Diagnostic plot
par(mfrow=c(2,2)) # view all plots together (subplot)
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit)) # residuals
plot(predict(lm.fit), rstudent(lm.fit)) # studentized residuals

plot(hatvalues(lm.fit)) # leverage statistics
which.max(hatvalues(lm.fit)) # the index of the largest elements of a vector

### 3.6.3 Multiple Linear Regression ###
# Fit multiple linear regression
lm.fit <- lm(medv~lstat+age, data=Boston)
summary(lm.fit)

lm.fit <- lm(medv~., data=Boston) # fit with all variables
summary(lm.fit)

# Access the individual components of a summary
?summary.lm
summary(lm.fit)$r.sq # R squared
summary(lm.fit)$sigma # RSE

# Compute variance inflation factors
install.packages("car") # for the first time -> error
library(car) 
vif(lm.fit) # compute variance inflation factors

# Fit multiple linear regression except a variable
lm.fit1 <- lm(medv~.-age, data=Boston)
summary(lm.fit1)
lm.fit1 <- update(lm.fit, ~.-age) # alternative way for fitting

### 3.6.4 Interaction Terms ###
summary(lm(medv~lstat*age, data=Boston)) # lstat, age, lstat*age
summary(lm(medv~lstat+age+lstat:age, data=Boston)) # same fitting as the above

### 3.6.5 Non-linear Transformations of the Predictors ###
lm.fit2 <- lm(medv~lstat+I(lstat^2)) # I() for transformations
summary(lm.fit2)

# Quantify the extent to which the quadratic fit is superior
lm.fit <- lm(medv~lstat)
anova(lm.fit, lm.fit2) # hypothesis test comparing the two models

# Check the plot
par(mfrow=c(2,2))
plot(lm.fit2)

# Fitting with the polynomial terms
lm.fit5 <- lm(medv~poly(lstat,5)) # better approach to create polynomial terms
summary(lm.fit5)

# Fitting with the log terms
summary(lm(medv~log(rm)), data=Boston)

### 3.6.6 Qualitative Predictors ###
fix(Carseats)
names(Carseats)

# R generates dummy variables automatically
lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

# Return the dummy variables
attach(Carseats)
contrasts(ShelveLoc)
?contrasts

### 3.6.7 Writing Functions ###
LoadLibraries # error
LoadLibraries() # error

# Create the function
LoadLibraries <- function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded.")
}

# Check the definition of the function
LoadLibraries

# Call the function
LoadLibraries()
