rm(list = ls())
setwd("/Users/shutingchen/Desktop/ISE 529               Data Analytics/Midterm")
d0=read.csv("homes.csv",header = T)
#1
#a

d0$style=as.factor(d0$style)
d0$age=2018-d0$year
str(d0)
d0$year=NULL
d1=d0[,c(1,2,3,4,5,7,12)]
cor(d1)
#         area       beds        baths     garage   lotsize     age
#price   0.8194701  0.4133239  0.6836854  0.5777863 0.2241685 -0.5555164
#r2 is the square of cor(d1)
#so the best numeric predictor is the area the r^2 is 0.8194701*0.8194701=0.67153
m1=lm(price~style,d0)
summary(m1)
#Multiple R-squared:  0.1741
m2=lm(price~ac,d0)
summary(m2)
#Multiple R-squared:  0.08329
m3=lm(price~pool,d0)
summary(m3)
#Multiple R-squared:  0.02149
m4=lm(price~quality,d0)
summary(m4)
#Multiple R-squared:  0.6601
m5=lm(price~highway,d0)
summary(m5)
#Multiple R-squared:  0.002598
#best predictor is area and worst predictors is highway

#b
library(MASS)
set.seed(1)
m0=lm(price~.,d0)
step1=stepAIC(m0)
m1=lm(price~area + baths + garage + style + lotsize + quality + highway + age,data=d0)
summary(m1)
d1 = d0 
levels(d1$style)[-c(2,7)]="0" 
m1=lm(price~area + baths + garage + style + lotsize + quality + highway + age,data=d1)
summary(m1)

#Multiple R-squared:  0.829
r1=data.frame(fitted=m1$fitted.values,observed=d0$price)
b=cor(r1)
r2=b^2
r2
#0.8289666

#c
n1=nrow(d0)
step2=stepAIC(m0,k=log(n1))
m2=lm(price ~ area + lotsize + quality + age,data=d0)
summary(m2)
newval1=data.frame(area=3500,lotsize=24000,quality="MEDIUM",age=50)
predict(m2,newval1)
#price of it is 379019.2

#d
set.seed(1)
library(boot)
m1=glm(price~area + baths + garage + style + lotsize + quality + highway + age,data=d1)
cverrors1=cv.glm(d1,m1,K=7)$delta[1]
cverrors1
#mspe of model 1 is 3438713508
set.seed(1)
m2=glm(price ~ area + lotsize + quality + age,data=d0)
cverrors2=cv.glm(d0,m2,K=7)$delta[1]
cverrors2
#mspe of model 2 is 3613129176


#2
#a
set.seed(1)
library(ISLR)
#library(Matrix)
#library(foreach)
library(glmnet)
d2=College
str(d2)
n2=nrow(d2)
n2

x=model.matrix(Apps~.,d2)[,-1]
y=d2$Apps
# lambdas from 10^10 to 10^{-2} 
a = seq(from=10,to=-2,length=100) 
grid=10^a
models=glmnet(x,y,alpha=0,lambda=grid) 
summary(models)
cv.out=cv.glmnet(x,y,alpha=0,lambda=grid,nfolds=10)
cv.out$cvm
#test mspe

#b
plot(cv.out$cvm,type="l",ylab="test mspe",xlab="") 
grid()


#c
set.seed(1)
m3=glm(Apps~.,data=d2)
cv.glm(d2,m3,K=10)$delta[1]
abline(h=1287415,lty=2,col="red")

#3
#a
d3=read.csv("bodyfat.csv",header = T)
d4=d3[,-4]
str(d4)
rxx=cor(d4)
rxx
#low correlate predictors are midarm and thigh
m4=lm(midarm~.,d4)
summary(m4)
#Multiple R-squared:  0.9904, and as we can see from p-value of summary p-value: < 2.2e-16
#so skinfold and thigh are both klinearly releted to midarm
#b
det(rxx)
#0.001400637>0
solve(rxx)
lambda=seq(0,1,0.001)
length(lambda)
m=matrix(0,1001,3)
id=diag(1,3)
for(i in 1:1001){
  bb1=solve(rxx + (lambda[i] * id))
  bb2=bb1%*%rxx%*%bb1
  m[i,]=diag(bb2)
}
colnames(m) = c("skinfold", "thigh","midarm")
head(m)
d5=cbind(m,lambda)
d5=data.frame(d5)
head(d5)
plot(1, type="n", xlab="lambda", ylab="", xlim=c(0, 1), ylim=c(0, 1000))
lines(d5$lambda[order(d5$lambda,decreasing=TRUE)],d5$skinfold[order(d5$skinfold)],col='red',type = "l")
lines(d5$lambda[order(d5$lambda,decreasing=TRUE)],d5$thigh[order(d5$skinfold)],col='green',type = "l")
lines(d5$lambda[order(d5$lambda,decreasing=TRUE)],d5$midarm[order(d5$skinfold)],col='blue',type = "l")
legend("topright",c("skinfold","thigh","midarm"),cex = 0.6,lty=1,col = c("red","green","blue"))
