#### Prob 6.9
data = read.csv("/Users/shuhualiang/Documents/Davis/STA 144/statepps.csv")

##a
Psi.la = data$landarea/sum(data$landarea)
Newdata.la = cbind(data,as.data.frame(Psi.la))

set.seed(200)
Index.la = sample(length(Newdata.la[,1]),10,replace = TRUE,prob = Psi.la)
Sample.la = Newdata.la[Index.la,]

Samp_psi.la = Sample.la$Psi.la
Samp_psi.la

## b
Psi.pop = data$popn/sum(data$popn)
Newdata.pop = cbind(data, as.data.frame(Psi.pop))

set.seed(100)
Index.pop = sample(length(Newdata.pop[,1]),10,replace = TRUE,prob = Psi.pop)
Sample.pop = Newdata.pop[Index.pop,]

Samp_psi.pop = Sample.pop$Psi.pop
Samp_psi.pop

#### Prob 6.10

## a -- Use Sample.pop
n = 10
t.hat.psi = sum(Sample.pop$counties/Sample.pop$Psi.pop)/n
t.hat.psi

t.counties = sum(data$counties)
t.counties

ui = Sample.pop$counties/Sample.pop$Psi.pop
Vhat.t.hat.psi = (1/n)*(1/(n-1))*sum((ui-t.hat.psi)^2)
Vhat.t.hat.psi

SE.t.hat.psi = sqrt(Vhat.t.hat.psi)
SE.t.hat.psi

## b
N = 51
t.hat.SRS = N*mean(Sample.pop$counties)
t.hat.SRS

s2 = var(Sample.pop$counties)
SE.t.hat = N*sqrt(s2)

SE.t.hat

#### 6.11
agpop = read.csv("/Users/shuhualiang/Documents/Davis/STA 144/agpop.csv")

states = Sample.pop$state
ST = c("FL","TX","GA","CA","MI","MI","KY","IL","GA","NY")

Index.States = sapply(1:10,function(i){
		which(agpop$state == ST[i])
})

Index.Counties = sapply(1:10, function(i){
	sample(Index.States[[i]],5)
})

Sample.Counties = agpop[Index.Counties,]

Mi = sapply(1:10, function(i){
	length(Index.States[[i]])
})

mi = 5 

y.bar.i= c()
for (i in 1:10){
	t.bar.i[i] = mean(Sample.Counties$acres92[1:5])
	Sample.Counties = Sample.Counties[6:50,]
}

t.hat.i.acres92 = y.bar.i*Mi
t.hat.i.acres92

t.by.psi = t.hat.psi.acres92/Samp_psi.pop
t.by.psi

t.hat.psi.acres92 = sum(t.by.psi)/n
t.hat.psi.acres92

Vhat.t.hat.psi = var(t.by.psi)
std.dev.t.hat.psi = sqrt(Vhat.t.hat.psi)

SE.t.hat.psi = std.dev.t.hat.psi/sqrt(n)
SE.t.hat.psi

Mo = sum(data$counties)
y.bar.hat.psi = t.hat.psi.acres92/Mo
y.bar.hat.psi

















