# plot data
?plot
x = rnorm(100)
y = rnorm(100)
plot(x, y)
plot(x, y, xlab="this is the x-axis", ylab="this is the y-axis", main="Plot of X vs Y")

# save the output
getwd() # current directory
pdf("Figure.pdf")
plot(x, y, col="green")
dev.off() # done creating the plot

# create a sequence of numbers
x = seq(1, 10)
x
x = 1:10
x
x = seq(-pi, pi, length=50)
x

# create contour plot
y = x
f = outer(x, y, function(x, y)cos(y)/(1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels=45, add=T)
fa = (f - t(f)) / 2
contour(x, y, fa, nlevels=15)

# create a heatmap
image(x, y, fa)

# create a three-dimensional plot
persp(x, y, fa)
persp(x, y, fa, theta=30)
persp(x, y, fa, theta=30, phi=20)
persp(x, y, fa, theta=30, phi=70)
persp(x, y, fa, theta=30, phi=40)