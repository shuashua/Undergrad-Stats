#HW 2, Problem 3.3

#a
data = c(66, 59,70,83,82,71)
ybar = mean(data)
S2 = var(data)

#c
library(gtools)
WoR = permutations(6,4,1:6,repeats.allowed=FALSE)
WoR[,1] = data[WoR[,1]]
WoR[,2] = data[WoR[,2]]
WoR[,3] = data[WoR[,3]]
WoR[,4] = data[WoR[,4]]

#d
n = 4
N = 6
RowMeans = rowMeans(WoR)
varYbar = ((S2)/n)*(1-n/N)

#e
Stra1 = data[1:3]
Stra2 = data[4:6]
PerOut = permutations(3,2,1:3,repeats.allowed=FALSE)

PerGroup = permutations(6,2,1:6,repeats.allowed=TRUE)

Stra1Per = cbind(Stra1[PerOut[,1]],Stra1[PerOut[,2]])
Stra2Per = cbind(Stra2[PerOut[,1]],Stra2[PerOut[,2]])

STRsample = cbind(Stra1Per[PerGroup[,1],], Stra2Per[PerGroup[,2],])
#Samples containing all three values from stratum 1 or stratum 2 cannot occur.

#f
ybarSTR = rowMeans(STRsample)
VybarSTR = (1-2/3)*(3/6)^2*(var(Stra1)+var(Stra2))/2
#much smaller compared to V in part c.










