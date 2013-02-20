library(MASS)

#a 
TheMean = c(0,0)
TheVar = matrix(c(1,.5,.5,1),nrow = 2)
Results = t(sapply(1:500,function(i){
Data = mvrnorm(30,mu = TheMean,Sigma = TheVar)
y = Data[,1]
x = Data[,2]
Model = lm(y~x)
ybar.reg = mean(y)+ Model$coefficients[2]*(-mean(x))
return(c(mean(y),ybar.reg))
}))
hist(Results[,1],main="Histogram of ybar")
hist(Results[,2],main="Histogram of ybar_hat_reg")

#b
TheMean = c(0,0)
TheVar = matrix(c(1,.5,.5,1),nrow = 2)
Results = t(sapply(1:500,function(i){
Data = mvrnorm(60,mu = TheMean,Sigma = TheVar)
y = Data[,1]
x = Data[,2]
Model = lm(y~x)
ybar.reg = mean(y)+ Model$coefficients[2]*(-mean(x))
return(c(mean(y),ybar.reg))
}))
hist(Results[,1],main="Histogram for ybar")
hist(Results[,2],main="Histogram for ybar_hat_reg")


