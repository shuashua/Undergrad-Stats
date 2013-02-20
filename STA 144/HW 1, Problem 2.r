#Problem 2

#a
library(gtools)
Data = c(15,34,35,36,11,17,36,15)
WR = permutations(8,3,1:8,repeats.allowed=TRUE)
WR[,1] = Data[WR[,1]]
WR[,2] = Data[WR[,2]]
WR[,3] = Data[WR[,3]]

SampleTotal = rowSums(WR)
table(SampleTotal)

#b
Table = as.data.frame(table(SampleTotal))
Probability = as.data.frame(Table[,2]/512)
DistTable = cbind(Table,Probability)

names(DistTable)[names(DistTable)=="SampleTotal"]<-"Xi"
names(DistTable)[names(DistTable)=="Table[, 2]/512"]<-"Probability"
head(DistTable)

#d
WoR = permutations(8,3,1:8,repeats.allowed=FALSE)
WoR[,1] = Data[WoR[,1]]
WoR[,2] = Data[WoR[,2]]
WoR[,3] = Data[WoR[,3]]

SampleTotal_o = rowSums(WoR)
table(SampleTotal_o)

Table_o = as.data.frame(table(SampleTotal_o))
Probability_o = as.data.frame(Table_o[,2]/336)
DistTable_o = cbind(Table_o,Probability_o)

names(DistTable_o)[names(DistTable_o)=="SampleTotal"]<-"Xi"
names(DistTable_o)[names(DistTable_o)=="Table[, 2]/336"]<-"Probability"
head(DistTable_o)

#f
#With Replacement:
Mean = mean(SampleTotal)
Var = var(SampleTotal)

#Without Replacement:
Mean_o = mean(SampleTotal_o)
Var_o = var(SampleTotal_o)






