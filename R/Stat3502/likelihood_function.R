

## Plot the density function and the likelihood function of N(\theta, 1)
##              theta=1
##              X=1
par(mfrow=c(1,2))
x <- c(1:1000)/100
# assume lambda = 1
plot(x, dexp( x, 1), 'l', col='red', xlab="x", ylab="density", main="pdf", cex.lab=1.5, cex.main=2)


lambda.all <- c(1:1000)/100
# assume lambda varies from 0.01 to 10
# result: function is maximized when lambda = 1 -> maximizer = 1
plot(lambda.all, dexp( 1, lambda.all), 'l', col='red', xlab="lambda", ylab="likelihood", main="Likelihood", cex.lab=1.5, cex.main=2)
