#a
No.Child = c(13,35,44,69,36,24,7,3,2,5,1,1)
Age.Month = c(9:20)
Data = rep(Age.Month,No.Child)
hist(Data,main="Historgram of Mayr et al. Data",xlab="Age (Month)")

#b
n = 240
Mean = mean(Data)
Var = var(Data)
SampleVar = (1/(n-1))*sum((Data-Mean)^2)

EstVarYbar = SampleVar/n

SEYbar = sqrt(EstVarYbar)

# 95% Confidence Interval:
Mean - 1.96*SEYbar
Mean + 1.96*SEYbar

#c
e = 0.5
n = (1.96^2)*Var/e^2
