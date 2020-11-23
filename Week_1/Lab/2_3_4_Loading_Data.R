# import data from a text file
Auto <- read.table("Auto.data")
fix(Auto) # view the data frame in a spreadsheet like window

# set header and a missing element
Auto <- read.table("Auto.data", header=T, na.strings="?")
fix(Auto)

# import data from a csv file
Auto <- read.csv("Auto.csv", header=T, na.strings="?")
fix(Auto)
dim(Auto) # returns the number of observations and variables
Auto[1:4,]

# omit rows containing missing observations
Auto <- na.omit(Auto)
dim(Auto)

# check the variable names
names(Auto)
