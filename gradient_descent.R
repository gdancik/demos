library(scatterplot3d)

# the function y = x1^2 + x2^2
f <- function(x) {
  (x[1]**2+x[2]**2)
}

# create a matrix with x1 = first column and x2 = second column
# each column contains random values between +/-2
x <- cbind(runif(1000,-2,2), runif(1000,-2,2))

# apply the function to each row of x to get corresponding y values
y <- apply(x, 1, f)

# construct a 3d scatterplot for x1,x2, and y
s <- scatterplot3d(x[,1], x[,2], y, pch = 18) 
s$points3d(0, 0, 0, col = 'red', pch = 18)
                 

# gradient descent with step size 'r'
#     to minimize f(x), update is x -f'(x)*r
#     in this case, update is the same for x1 and x2
f_update <- function(x, r) {
  x - r*2*x
}

ans <- matrix(0,ncol=2,nrow=100)
ans[1,] <- c(-3,-5) # initial value
for (i in 2:nrow(ans)) {
  ans[i,] <- f_update(ans[i-1,], .1)
}


# construct a 3d scatterplot for x1,x2, and y
s<-scatterplot3d(x[,1], x[,2], y, 
                 xlim = c(-4,6),
                 ylim = c(-4,6),
                 zlim = c(-5,10))


n <- nrow(ans)
s$points3d(ans[1:n,1], ans[1:n,2], 
           apply(ans, 1, f)[1:n], col = 'red',
           xlim = c(-4,6),
           ylim = c(-4,6),
           pch = 18)






