#a

Data = read.csv("golfsrs.csv")
wkday9 = Data$wkday9
hist(wkday9,main="Histogram for Weekday Greens Fees for Nine Holes of Golf",
	xlab="Weekday Greens Fees for Nine Holes of Golf")

#b

Mean = mean(wkday9)
var = var(wkday9)

n = 120
N = 14938
VyBar = (1-n/N)*(var/n)
SEyBar = sqrt(VyBar)