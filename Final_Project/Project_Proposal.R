### preparation ###
# delete all variables
rm(list=ls())

# set working directory 
setwd("~/Documents/UCLA_Extension/Final_Project")

# import training data
app_train <- read.csv("./Data/application_train.csv", header=T, sep=",", na.strings=c('', 'NULL', '""')) # need na.strings to capture all missing values pattern
dim(app_train) # 307511 x 122
head(app_train)
names(app_train) # column names
summary(app_train)

### EDA ###
# distribution of the target
table(app_train$TARGET) # 0: 282686, 1: 24825
hist(app_train$TARGET) # imbalanced class problem

# missing values
col_train <- app_train[, colnames(app_train) != "TARGET"] # dataframe without target column
sum_na_train <- sapply(col_train, function(y) sum(is.na(y))) # vector with names (index)
length(sum_na_train[sum_na_train > 0]) # number of columns that have missing values: 67
sort(sum_na_train[sum_na_train > 0], decreasing=TRUE)[1:20] # top 20 columns with missing values

# column types
col_type <- sapply(col_train, class)
table(col_type) # character: 16, integer: 40, numeric: 65

# number of classes in the character columns
category_columns <- names(col_type[col_type == "character"]) # names of 16 columns
col_train[category_columns]
cat <- c()
for (i in 1:length(category_columns)){
  cat <- append(cat, dim(table(col_train[category_columns[i]])))
}
num_class <- data.frame(category_columns, cat)
num_class # number of classes
          
# anomalies, outliers
# TBD

# corelation
# TBD
