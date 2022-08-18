library(lattice)

# generate some inputs
x1 <- 1:10
x2 <- 1:10

# generate grid design -- 
# all combinations of each input
d <- expand.grid(p1 = x1,p2 = x2)

# generate responses (y-values)
y1 <- d$p1 + 5         # y = p1 + 5
y2 <- d$p1 + 2*d$p2    # y = p1 + 2p2

# generate 'interaction' effect where slope of 'p2' 
# depends on 'p1': y3 = p1 + k*p2, where 
# k = 2 if p1 <= 5 or 4 if p1 > 5
k <- rep(2, nrow(d))
k[d$p1>5] <- 4
y3 <- d$p1 + k*d$p2

# create data frame with inputs and responses
df <- cbind(d, y1, y2, y3)

# contour plot for y1
# for example, if p1 is 3, the response y = 8, and changing
# p2 has no effect. If we change p1 from 3 to 5, then y 
# changes from 8 to 10
contourplot(y1 ~ p1 *p2, data = df, 
            main = 'y = p1 + 5\nChanging p2 has no effect on the response')

# contour plot for y2
# lines are parallel, so the slopes for p1 and p2 are the same
# for example, if p1 = 2 and p2 = 4, then y = 10; if we
# increase p2 by 2.5, then the y value increases by 5, and 
# this is true regardless of the value of p1
contourplot(y2 ~ p1 *p2, data = df,
            main = 'y = p1 + 2*p2\nx1 and x2 are independent (parallel lines)')

# contour plot for y3
# lines are not parallel, so there is an interaction (the slope
# for p2 depends on the value of p1)
# when p1 < 5, the contour lines for 10 and 20 are the same as in
# the previous graph, (though the 15 line is not shown). The
# slope for p2 is 2, so increasing p2 by 5 increases the 
# y value by 10; however, if p1 >= 5, the slope of p2 doubles;
# now, increasing p2 by 5 (e.g., from 4 to 9), increases the
# y value by 20.
contourplot(y3 ~ p1 *p2, data = df, 
            main = 'y = p1 + k*p2, k = 2 if p1 <5 or 4 otherwise\ninteraction effect (impact of changing p2 depends on p1)')

# add some color -- makes it easy to quickly determine where
# the 'optimal' parameters are. For example, if we wanted
# large y-values, we would pick high values of p1 and p2.
contourplot(y3 ~ p1 *p2, data = df, 
            main = 'y = p1 + k*p2, k = 2 if p1 <5 or 4 otherwise\ninteraction effect (impact of changing p2 depends on p1)',
            region = TRUE)

# another example (equation for a circle)
y <- -2*(d$p1 - 6)**2 - (d$p2 - 5)**2

# it should be clear from the equation and the contour plot that
# the maximum value (0) occurs when p1 = 6 and p2 = 5
contourplot(y ~ d$p1*d$p2, 
            main = 'y = p1 + k*p2, k = 2 if p1 <5 or 4 otherwise\ninteraction effect (impact of changing p2 depends on p1)',
            region = TRUE)

