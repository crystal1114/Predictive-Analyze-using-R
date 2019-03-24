library(faraway)
library(carData)
library(car)
install.packages("randomForest")
library(randomForest)
#rfNews()
#1
d0=data.frame(hsb)
dim(d0)
head(d0)
set.seed(1)
bag1=randomForest(prog~math+science,d0,mtry=2,importance=T) 
par(mfrow=c(1,2))
ypred=predict(bag1,newdata = d0)
labels=c("academic","general","vocation")
colors = c("black","green","red")
plot(math~science,d0,col=d0$prog,pch=18,main="observed")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()
plot(math~science,d0,col=ypred,pch=18,main="predicted")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()

par(mfrow=c(1,1))
1-sum(diag(prop.table(table(d0$prog,ypred))))
#error rate is 0.09

#2
#a)
library(ISLR)
library(randomForest)
d0=data.frame(Caravan)
sapply(Caravan,table)
dim(d0)
d0=d0[,-which(names(d0)%in% c("PVRAAUT","AVRAAUT"))]
train=1:1000
d0train=d0[train,]
d0test=d0[-train]
set.seed(1)
forest1=randomForest(Purchase~.,d0train,mtry=83,importance=T)
importance(forest1)
varImpPlot(forest1,main="")
#the best predictor is MOSTYPE


#b)
library(gbm)
set.seed(1)
n=nrow(d0)
y=rep(0,n)
y[d0$Purchase=="Yes"]=1
d0$Purchase=y
train=1:1000
d0train=d0[train,]
d0test=d0[-train,]
boost1 = gbm(Purchase~.,data=d0[train,],distribution="bernoulli", n.trees=1000,shrinkage=0.01)
summary(boost1)
grid()

#the most important predictor is PPERSAUT

curve(-8+x,col="red")

abline(-3,-13,col="black")
abline(-6,-5,col="green")
abline(0,-21,col="blue")
abline(-10,9,col="pink")

