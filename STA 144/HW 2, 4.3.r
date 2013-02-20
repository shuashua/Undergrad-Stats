Data = read.csv("Chap4Prob3.csv")

#a
Dia = Data$Diameter   #x
Age = Data$Age        #y

plot(Dia,Age,ylab="Diameter",main="Scatterplot of y vs. x",ylim=c(0,180),xlim=c(0,13))

#b
N = 1132
n = 20

ybar = mean(Age)
xbar = mean(Dia)
Bhat = ybar/xbar
Xu = 10.3
Yr = Bhat*Xu
S2 = var(Age)
ei = Age - Bhat*Dia; 
se.2 = (1/(n-1))*sum(ei^2)
se=sqrt(se.2) 

#c
Reg = lm(Age~Dia)
Bo = Reg$coefficients[1]
Ba = Reg$coefficients[2]

Se2 = sum(Reg$residuals^2/(n-1))
Se=sqrt(Se2)

#d
abline(0,Bhat,lty=1)
abline(Bo,Ba,lty=2)

legend("topleft",c("Ratio","Regression"),lty=c(1,2))











