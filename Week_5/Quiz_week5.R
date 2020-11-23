# preparation
RNGkind(sample.kind = "Rounding")
set.seed(1)
sample(10)

# Question 1
library(ISLR)
attach(Wage)

q1.1 <- lm(wage~education+age, data=Wage)
q1.2 <- lm(wage~education+poly(age, 2), data=Wage)
q1.3 <- lm(wage~education+poly(age, 3), data=Wage)
anova(q1.1, q1.2, q1.3) # Model 2 vs 3: 0.0341

# Question 2
coef(summary(q1.3)) # -10.719285

# Question 3
cut(age, breaks=c(0, 25, 35, 45, 55, 80))
q3 <- lm(wage~cut(age, breaks=c(0, 25, 35, 45, 55, 80)), data=Wage)
coef(summary(q3))
predict(q3, newdata=list(age=35), se=T) # 104.164

# Question 4
library(splines)
q4.1 <- lm(wage~bs(age, knots=c(25, 40, 60)), data=Wage)
q4.2 <- lm(wage~ns(age, df=4), data=Wage)
q4.3 <- smooth.spline(age, wage, df=16)
q4.4 <- loess(wage~age, span=.5, data=Wage)

age.grid <- seq(from=agelims[1], to=agelims[2])

par(mfrow=c(1,1))
plot.new()
plot(age, wage, xlim=agelims, cex=.5, col='darkgrey')
lines(age.grid, predict(q4.1, data.frame(age=age.grid)), col='red', lwd=2)
lines(age.grid, predict(q4.2, data.frame(age=age.grid)), col='blue', lwd=2)
lines(q4.3, col='green', lwd=2)
lines(age.grid, predict(q4.4, data.frame(age=age.grid)), col='orange', lwd=2)

predict(q4.1, newdata=list(age=55)) # 118.2185 
predict(q4.2, newdata=list(age=55)) # 118.406
predict(q4.3)$y[55-18+1] # 118.1214
predict(q4.4, newdata=list(age=55)) # 117.593

# Question 5
library(gam)
q5 <- gam(wage~s(age, 5)+year+education, data=Wage)
coef(q5)

# Question 6
predict(q5, newdata=data.frame(age=48, year=2008, education='5. Advanced Degree')) # 156.9727

# Question 7
q7 <- gam(wage~s(age, 3)+year+education, data=Wage)
predict(q7, newdata=data.frame(age=48, year=2008, education='5. Advanced Degree')) # 157.7089

