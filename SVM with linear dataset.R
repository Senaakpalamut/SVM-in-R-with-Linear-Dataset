#SVM for Linear Dataset
set.seed(10111)
#we generate a 2D dataset, x normally distr with 20 observ in 2 classes on 2 variables
#y is either 1 or -1 with 10 in each class.
x=matrix(rnorm(40), 20, 2)
y=rep(c(-1,1), c(10,10))

#for y=1, means can be moved from 0 to 1 in each coordinate
x[y==1, ]=x[y==1, ]+1
plot(x, col=y+3, pch=19)

#load e1071 to use SVM
library(e1071)

#make a data frame of the dataset and turn y into a factor variable.
data2=data.frame(x, y=as.factor(y))

#call SVM by using y as a response variable and the other variables are as predictors
#tell the SVM that kernel is linear, tune-in cost is 10 and scale is closed.
svmfit= svm(y~. , data=data2, kernel='linear', scale=FALSE)
print(svmfit)
plot(svmfit, data2)

#create a grid of values for x1 and x2 (x1 and x2 are the new columns created 
#by unpacking x into) that covers the whole domain. use 'make.grid'
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

#apply make.grid funtion to x
xgrid = make.grid(x)
xgrid[1:10,]

#make predictions with xgrid (newdata)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col=c('red', 'blue')[as.numeric(ygrid)], pch=20,
     cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)

#extracting the data point coefficients/ linear coefficients
beta=drop(t(svmfit$coefs)%*%x[svmfit$index,])
beta0=svmfit$rho

#replot the points on the grid and put the points back in.
plot(xgrid, col=c('red','blue')[as.numeric(ygrid)], pch=20, cex=.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)

#use abline funtion to define upper and lower limits of the decision boundary.
abline(beta0 / beta[2], -beta[1] / beta[2])
abline((beta0-1)/beta[2], -beta[1]/beta[2], lty=2)
abline((beta0+1)/beta[2], -beta[1]/beta[2], lty=2)
