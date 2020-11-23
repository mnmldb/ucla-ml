# Question 1
set.seed(3)
y <- rnorm(1000)
median(y) # 0.03233806

# Question 2
?plot
?matrix

# Question 3
X <- matrix(c(1:25), nrow=5, ncol=5)
X
X[3,]

# Question 4
Auto <- read.table('auto.data', header=T)
sapply(Auto, class)
print(Auto)
str(Auto) # structure

# Question 5
attach(Auto)
cylinders <- as.factor(cylinders)
str(cylinders) # factor
plot(cylinders, mpg)
median(Auto[Auto$cylinders==8,]$mpg) # sanity check, 14

# Question 6
hist(mpg)
hist(mpg, breaks=15)

# Question 7
pairs(~ mpg + displacement + horsepower + weight + acceleration)
