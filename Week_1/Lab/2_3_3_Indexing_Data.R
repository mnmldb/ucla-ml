# select the element
A = matrix(1:16, 4, 4)
A
A[2, 3]

# select multiple rows and columns
A[c(1, 3), c(2, 4)]
A[1:3, 2:4]
A[1:2,] # no index is equal to all rows/columns
A[, 1:2]

# select a singlw row or column -> vector
A[1,]

# negative sign == exception
A[-c(1, 3),]

# the number of rows and columns of a matrix
dim(A)