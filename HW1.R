install.packages("PASWR2")
install.packages("lattice")
install.packages("ggplot2")

library(lattice)
library(ggplot2)
library(PASWR2)

#1
d1=VIT2005
mu=mean(d1$totalprice)
mu
stdev=sd(d1$totalprice)
stdev
boxplot(d1$totalprice,main="totalprice")

par(mfrow=c(2,1)) 
hist(d1$totalprice,xlab="",main="totalprice distribution") 
boxplot(d1$totalprice)
par(mfrow=c(1,1)) 
outlier=boxplot.stats(d1$totalprice)$out
outlier
which(grepl(outlier,d1$totalprice))


d2 = subset(d1,totalprice,subset= d1$floor==4&d1$rooms ==5)
d4=apply(d2,2,mean)
d4

d5=subset(d1,totalprice,subset = d1$rooms==6)
d6=d5[d5>200000]
length(d6)

table(d1$garage,d1$storage)


plot(totalprice~area,d1)
m1=lm(totalprice~area,d1)
abline(m1,col="red",lwd=2)

label = rep("",392) 
res = resid(m1) 
idx = which(res==max(res)) 
label[idx]=idx 
text(totalprice~area,d1,labels=label,pos=1,offset=0.5,cex=0.6,col=2)

label = rep("",392) 
res = resid(m1) 
idx = which(res==min(res)) 
label[idx]=idx 
text(totalprice~area,d1,labels=label,pos=1,offset=0.5,cex=0.6,col=2)



newval1=data.frame(area=100)
predict(m1,newval1)

plot(totalprice~age,d1)
m2=lm(totalprice~age,d1)
abline(m1,col="red",lwd=2)


summary(m1)
summary(m2)

#area is better predictor as we can see from picture the relation of age and totalprice is weak and hard to find the rules, but area have strong influence to the totalprice, as we can see from summary, the line of m1 R-squared =Explained variation / Total variation.explained variation measures the proportion to which a mathematical model accounts for the variation (dispersion) of a given data set.explained variation measures the proportion to which a mathematical model accounts for the variation (dispersion) of a given data set.
#and explained variation measures the proportion to which a mathematical model accounts for the variation (dispersion) of a given data set. m1.R-squared=0.6548 which is good enough to predict the totalprice.

d4=subset(d1,select = c(totalprice,area,age))
trainingset=d4[1:109,1:3]
F1=lm(totalprice~area,trainingset)
F2=lm(totalprice~age,trainingset)
testset=d4[110:218,1:3]
newval2=data.frame(area=testset$area)
r1=predict(F1,newval2)
newval3=data.frame(age=testset$age)
r2=predict(F2,newval3)
e1=(sum(testset$totalprice-r1)^2)/109
e1
e2=(sum(testset$totalprice-r2)^2)/109
e2

#2
d0=CARS2004
d0$total.cars=d0$cars*d0$population/1000
d0$death.rate=d0$deaths/d0$total.cars



plot(total.cars~death.rate,D1)
#it is like power function f(x)=x^a (a<0)
d0$ltc=log(d0$total.cars)
d0$ldr=log(d0$death.rate)
plot(ltc~ldr,d0,xlab="log(death.rate)",ylab="log(total.cars)")
M1=lm(ltc~ldr,d0)
abline(M1,col="red",lwd=2)
summary(M1)
coefficients(M1)
#it is like linear funtion f(x) = kx + b (k<0,b>0)
d20=min(d0$death.rate)

x=log(d20,base=exp(1))
newval2=data.frame(ldr=x)
A=predict(M1,newval2)
n=exp(A)
n


