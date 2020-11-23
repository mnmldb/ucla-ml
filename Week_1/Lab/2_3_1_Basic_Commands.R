# vector
x <- c(1, 3, 2, 5)
x

# assign
x = c(1, 6, 2)
x
y = c(1, 4, 3)

# help
?c

# length
length(x)
length(y)

# element wise calculation
x + y
x * y

# list of the objects, delete the object
ls()
rm(x, y)
ls()
rm(list=ls()) # remover all objects at once

# matrix
?matrix
x = matrix(data=c(1, 2, 3, 4), nrow=2, ncol=2)
x
x = matrix(c(1, 2, 3, 4), nrow=2, ncol=2) # omit the argument name
matrix(c(1, 2, 3, 4), 2, 2, byrow=TRUE) # default: successively filling in columns

# square root, power of 2
sqrt(x)
x^2

# random normal variables, correlation
x = rnorm(50)
y = x + rnorm(50, mean=50, sd=.1)
cor(x, y)

# reproduce the exact same set of random numbers
set.seed(1303)
rnorm(50)

# mean, variance, standard deviation
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)
