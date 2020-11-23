# import data set continued from 2.3.4
Auto <- na.omit(read.table("Auto.data", header=T, na.strings="?"))
dim(Auto)

# produce scatterplots 1
plot(cylinders, mpg) # returns an error
plot(Auto$cylinders, Auto$mpg)

# produce scatterplots 2
attach(Auto) # make the variables available by name
plot(cylinders, mpg)

# convert from quantitative variables int qualitative variables
cylinders <- as.factor(cylinders)

# produce boxplots
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col="red", varwidth=T)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)
plot(cylinders, mpg, col="red", varwidth=T, xlab="cylinders", ylab="MPG")

# produce a histogram
hist(mpg)
hist(mpg, col=2)
hist(mpg, col=2, breaks=15)

# create a scatterplot matrix
pairs(Auto) # all
pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto) # subset

# interactive method for identifying the value for a particular variable
plot(horsepower, mpg)
identify(horsepower, mpg, name)

# produce a numerical summary
summary(Auto) # dataframe
summary(mpg) # single variable

# shut down
q()
quit
savehistory()
loadhistory()