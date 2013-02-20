####Problem 4
data = read.csv("/Users/shuhualiang/Documents/Davis/STA 144/journal.csv")

#b
phat = sum(data$nonprob)/sum(data$numemp)  #same as ybar

tSum = sum(data$nonprob)
mSum = sum(data$numemp)
yrbar = tSum/mSum

mbar = mean(data$numemp)
n = 26
N = 1285

SEyrbar = sqrt(((1-n/N)/(n*mbar^2))*(sum((data$nonprob-yrbar*data$numemp)^2)/(n-1)))


####Problem 6
data = matrix(c(1,4,0,3,4,0,5,3,7,3,4,0,5,2,1,6,9,7,5,0,3,1,7,0,7,4,2,6,8,3,1,2,5,4,9,0),ncol=12,nrow=3,byrow=TRUE)

N = 580
n = 12
Mi = 24
mi = 3

Wij = N*Mi/(n*mi)

that_unb = Wij*sum(data)

yi = array(0,3)
for (i in 1:3){
	yi[i] = sum(data[i,])/12
}

ti = array(0,12)
for (i in 1:12){
	ti[i] = (Mi/mi)*sum(data[,i])
}

S2i = array(0,3)
for (i in 1:3){
	S2i[i] = (1/2)*(sum((data[i,]-yi[i])^2))
}

S2t = sum((ti-that_unb/N)^2)/11

Vtunb = N^2*(1-n/N)*S2t/n + ((N/n)*sum((1-mi/Mi)*Mi^2*S2i/mi))
SEtunb = sqrt(Vtunb)

# 95% CI
that_unb - 1.96*SEtunb   #Lower Bound
that_unb + 1.96*SEtunb	 #Upper Bound

#SAS Output
Vwrtunb = N^2*S2t/n
SEsas = sqrt(Vwrtunb)


####Problem 11

N = 828
n = 85
Mi = 215

Data = c(4,3,rep(2,4),rep(1,22),rep(0,57))

＃a
tunb = (N/n)*sum(Data)
errorRate = tunb/(Mi*N)

S2t = var(Data)

SEtunb = (1/Mi)*sqrt((1-n/N)*S2t/n)

＃b
that = (N/n)*sum(Data)
Vthat = (N^2)*(1-n/N)*S2t/n
SEthat = sqrt(Vthat)

#c
N = 178020
n = 18275

t_hat_unb = N/n*sum(Data)
ybar = t_hat_unb/N

VpSRS = ((N-n)/(N-1))*ybar*(1-ybar)/n

####14
Data = as.data.frame(matrix(c(1471,792,25,10,890,447,15,3,1021,511,20,6,1587,800,40,27),nrow=4,ncol=4,byrow=TRUE))

names(Data)[names(Data)=="V1"]="#Stud"
names(Data)[names(Data)=="V2"]="Mi"
names(Data)[names(Data)=="V3"]="mi"
names(Data)[names(Data)=="V4"]="#Smoke"

N = 29
n = 4

#a
yi_bar = Data["#Smoke"]/Data["mi"]
Si2 = (yi_bar)*(1-yi_bar)/Data['mi']

Ybh_r = sum(Data['Mi']*yi_bar)/sum(Data['Mi'])

Sr2 = sum((Data['Mi']*yi_bar-Data['Mi']*Ybh_r)^2)/(n-1)

VYbh_r = (1/mean(Data['Mi'])^2)*(1-n/N)*Sr2/n + (1/(n*N*mean(Data['Mi'])^2))*sum(Data['Mi']^2*(1-Data['mi']/Data['Mi'])*Si2/Data['mi'])

SEYbh_r = sqrt(VYbh_r)
	# 95% CI
Ybh_r - 1.96*SEYbh_r
Ybh_r + 1.96*SEYbh_r

#b
ti_hat = Data['Mi']*yi_bar
t_hat_unb = (N/n)*sum(ti_hat)

St2 = sum((ti_hat-t_hat_unb/N)^2)/(n-1)

Vthat_unb = (N^2)*(1-n/N)*St2/n + (N/n)*sum(Data['Mi']^2*Si2/Data['mi']*(1-mi/Mi))

SEthat_unb = sqrt(Vthat_unb)
	#95% CI
t_hat_unb - 1.96*SEthat_unb
t_hat_unb + 1.96*SEthat_unb

#c

M = mean(Data$Mi)
m = mean(Data$mi)
N = 35

MSW_hat = sum((Data$mi-1)*Si2)/(n*(m-1))
MSB_hat = sum((Data$mi)*(yi_bar-Ybh_r)^2)/(n-1)

S2_hat = ((N-1)*MSB_hat + N*(M-1)*MSW_hat)/(N*(M-1))

C = 300
c1 = 50
c2 = 0.5

Ra2 = 1 - MSW_hat/S2_hat

m_opt = sqrt(c1*M*(N-1)*(1-Ra2)/(c2*(N*M-1)*Ra2))
n_opt = C/(c1+c2*m_opt)

##### 16

N=46
n=10

#a
Data = read.csv("/Users/shuhualiang/Documents/Davis/STA 144/measles.csv")

Ret = Data[which(Data$returnf == 1),]
NoRet = Data[which(Data$returnf != 1),]

P = array(0,10)
for (i in 1:10){
	P[i] = length(Ret[which(Ret$school == i),]$returnf)/length(Data[which(Data$school == i),]$returnf)
}


#b
New <- Data[which(Data$returnf != 9),]

Sums = aggregate(New[,1:11],by = list(New$school),sum)

Mi = unique(New$Mitotal)

mi = array(0,10)
for (i in 1:10){
	mi[i] = length(New[which(New$school == i),]$returnf)
}




w = array(0,10)
for (i in 1:10){
	w[i] = (N*Mi[i])/(n*mi[i])
}

＃c
t_hat_unb = sum(w*Sums$returnf)
ybh_r = n/N*t_hat_unb/sum(Mi)

ti_hat = array(0,10)
for(i in 1:10){
	ti_hat[i] = Sums$returnf[i]*Mi[i]/mi[i]
}

Mbar = mean(Mi)

yi_bar = ti_hat/Mi

Sr2 = sum((Mi*yi_bar - Mi*ybh_r)^2)/(n-1)

Si2 = array(0,10)
for (i in 1:10){
	Si2[i] = sum((New[which(New$school == i),]$returnf-mean(New[which(New$school == 	i),]$returnf))^2)/(mi[i]-1)
}

Vyhb_r = (1-n/N)*Sr2/n/Mbar^2 + sum(Mi^2*(1-mi/Mi)*Si2/mi)/(n*N*Mbar^2)
SEybh_r = sqrt(Vyhb_r)

# 95% CI
ybh_r - 1.96*SEybh_r
ybh_r + 1.96*SEybh_r

#d (SRS)

N = 9962
n = 281

S2 = var(New$returnf)
Vy_hat = (1-n/N)*S2/n

VarRatio = Vyhb_r/Vy_hat











