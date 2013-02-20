data = read.csv("counties.csv")

#a
Phy = data$physician
hist(Phy)

#b
ybar = mean(Phy)
N = 3141
that = ybar*N
n = 100 
S2 = var(Phy)

SEy = sqrt((1-n/N)*S2/n)
SEt = N*SEy


#c
Pop = data$totpop
plot(Pop,Phy,xlim=c(0,150000),ylim=c(0,300),xlab="County Population",
	ylab="Number of Physicians",main = "Physicians vs. Population")

ybar = mean(Phy); xbar = mean(Pop)
B.hat = ybar/xbar

abline(0,B.hat,lty=1)


###
Reg = lm(Phy~Pop)
Bo = Reg$coefficients[1]
Ba = Reg$coefficients[2]

abline(Bo,Ba,lty=2)

legend("topleft",c("Ratio","Regression"),lty=c(1,2))

#d (Choose Regression)
tx = 255077536
xbaru = tx/N
ybar.reg = Bo + Ba*xbaru
t.hat.reg = N*ybar.reg
se.2 = 1/(n-1)*sum(Reg$residuals^2)
SE.t.reg = N*sqrt((1-n/N)*(se.2/n))
CI.yreg = c(t.hat.reg - 1.96*SE.t.reg,t.hat.reg + 1.96*SE.t.reg)

#e
#ratio

t.hat.yr = B.hat*tx






