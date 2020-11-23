library(ISLR)
attach(Wage)

### 7.8.1 Polynomial Regression and Step Functions ###
# orthogonal polynomials
fit <- lm(wage~poly(age, 4), data=Wage)
coef(summary(fit))

# raw polynomials
fit2 <- lm(wage~poly(age, 4, raw=T), data=Wage)
coef(summary(fit2))

fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4), data=Wage)
coef(fit2a)

fit2b <- lm(wage~cbind(age, age^2, age^3, age^4), data=Wage)
coef(fit2b)

# prediction
agelims <- range(age) # 18 80
age.grid <- seq(from=agelims[1], to=agelims[2]) # all integers from 18 to 80
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit) # standard error bands

# plot
par(mfrow=c(1,2), mar=c(4.5, 4.5, 1, 1), oma=c(0,0,4,0))
plot(age, wage, xlim=agelims, cex=.5, col='darkgrey')
title('Degree-4 Polynomial', outer=T)
lines(age.grid, preds$fit, lwd=2, col='blue')
matlines(age.grid, se.bands, lwd=1, col='blue', lty=3)

# two ways are identical
preds2 <- predict(fit2, newdata=list(age=age.grid), se=TRUE)
max(abs(preds$fit - preds2$fit))

# decide the degree by ANOVA
fit.1 <- lm(wage~age, data=Wage)
fit.2 <- lm(wage~age+poly(age, 2), data=Wage)
fit.3 <- lm(wage~age+poly(age, 3), data=Wage)
fit.4 <- lm(wage~age+poly(age, 4), data=Wage)
fit.5 <- lm(wage~age+poly(age, 5), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# obtain p-values
coef(summary(fit.5))

# the square of the t-statistics are equal to the F-statistics
(-11.983)^2

# ANOVA works when we have other terms
fit.1 <- lm(wage~education+age, data=Wage)
fit.2 <- lm(wage~education+poly(age, 2), data=Wage)
fit.3 <- lm(wage~education+poly(age, 3), data=Wage)
anova(fit.1, fit.2, fit.3)

# predict whether an individual earns more than $250,000
fit <- glm(I(wage > 250)~poly(age, 4), data=Wage, family=binomial)
preds <- predict(fit, newdata=list(age=age.grid), se=T)

# calculate the confidence intervals
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit, preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# directly compute the probabilities
preds <- predict(fit, newdata=list(age=age.grid), type='response', se=T)

# plot
plot(age, I(wage > 250), xlim=agelims, type='n', ylim=c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex=.5, pch='|')
lines(age.grid, pfit, lwd=2, col='blue')
matlines(age.grid, se.bands, lwd=1, col='blue', lty=3)

# step function
table(cut(age, 4))
coef(summary(fit))

### 7.8.2 Splines ###
# regression spline
library(splines)
fit <- lm(wage~bs(age, knots=c(25, 40, 60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)
plot(age, wage, col='gray')
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit + 2 * pred$se, lty='dashed')
lines(age.grid, pred$fit - 2 * pred$se, lty='dashed')

# produce a spline with knots at uniform quantiles
dim(bs(age, knots=c(25, 40, 60)))
dim(bs(age, df=6))
attr(bs(age, df=6), 'knots')

# natural spline with four degrees of freedom
fit2 <- lm(wage~ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col='red', lwd=2)

# smoothing spline
plot(age, wage, xlim=agelims, cex=.5, col='darkgrey')
title('Smoothing Spline')
fit <- smooth.spline(age, wage, df=16)
fit2 <- smooth.spline(age, wage, cv=TRUE) # warning
fit2$df
lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=2)
legend('topright', legend=c('16 DF', '6.8 DF'), col=c('red', 'blue'), lty=1, lwd=2, cex=.8)

# local regression
plot(age, wage, xlim=agelims, cex=.5, col='darkgrey')
title('Local Regression')
fit <- loess(wage~age, span=.2, data=Wage)
fit2 <- loess(wage~age, span=.5, data=Wage)
lines(age.grid, predict(fit, data.frame(age=age.grid)), col='red', lwd=2)
lines(age.grid, predict(fit2, data.frame(age=age.grid)), col='blue', lwd=2)
legend('topright', legend=c('Span=0.2', 'Span=0.5'), col=c('red', 'blue'), lty=1, lwd=2, cex=.8)

### 7.8.3. GAMs ###
# GAM by lm() function
gam1 <- lm(wage~ns(year, 4)+ns(age,5)+education, data=Wage)

# GAM by gam() function
library(gam)
gam.m3 <- gam(wage~s(year, 4)+s(age, 5)+education, data=Wage)

# plot gam object
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE, col='blue')

# plot lm object
plot.Gam(gam1, se=TRUE, col='red')

# ANOVA tests to determine the best model
gam.m1 <- gam(wage~s(age, 5)+education, data=Wage)
gam.m2 <- gam(wage~year+s(age, 5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test='F')

# summary
summary(gam.m3)

# prediction
preds <- predict(gam.m2, newdata=Wage)

# use local regression fits
gam.lo <- gam(wage~s(year, df=4)+lo(age, span=0.7)+education, data=Wage)
plot.Gam(gam.lo, se=TRUE, col='green')

# create interactions
gam.lo.i <- gam(wage~lo(year, age, span=0.5)+education, data=Wage)

# plot
library(akima)
plot(gam.lo.i)

# logistic regression GAM
gam.lr <- gam(I(wage > 250)~year+s(age, df=5)+education, family=binomial, data=Wage)
par(mfrow=c(1, 3))
plot(gam.lr, se=T, col='green')

table(education, I(wage > 250))

gam.lr.s <- gam(I(wage > 250)~year+s(age, df=5)+education, family=binomial, data=Wage, subset=(education!='1. < HS Grad'))
plot(gam.lr.s, se=T, col='green')
