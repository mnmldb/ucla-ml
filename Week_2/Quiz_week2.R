# Question 1
names(Boston)
q1 <- lm(medv~lstat, data=Boston)
par(mfrow=c(1,1))
plot(Boston$medv~Boston$lstat)
abline(q1,col="red")

predict(q1, data.frame(lstat=c(10)), interval = "prediction") # 25.053
predict(q1, data.frame(lstat=c(10)), interval = "confidence")

# Question 2
q2 <- lm(medv~.-age, data=Boston)
summary(q2) # indus

# Question 3
q3 <- lm(medv~lstat*age, data=Boston)
summary(q3) # 0.556

# Question 4
q4 <- lm(medv~lstat*black, data=Boston)
summary(q4) # 0.561

# Question 5
q5_1 <- lm(medv~rm, data=Boston)
summary(q5_1) # 9.102

q5_2 <- lm(medv~log(rm), data=Boston)
summary(q5_2) # 54.055

# Question 6
names(Carseats)
q6 <- lm(Sales~.+Income:Advertising+Age:Price, data=Carseats, contrasts = list(ShelveLoc=contr.treatment(c("Bad", "Good", "Medium"), base = 3)))
summary(q6) # ShelveLocMedium
summary(Carseats$ShelveLoc)

q6_check <- lm(Sales~.+Income:Advertising+Age:Price, data=Carseats)
summary(q6_check)

# Question 7