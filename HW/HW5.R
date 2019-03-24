library(faraway)
library(MASS)
library(car)

d0=data.frame(hsb)
d0 = hsb[,-1]
str(d0)
#a)
table(d0$gender,d0$prog)
round(prop.table(table(d0$gender,d0$prog),2)*100,2)
# The proportion of females choosing the three different programs is almost similar, 55.24%，53.33%，54.00%
# Likewise,the proportion of males choosing the three different programs is almost similar, 44.76%，46.67%，46.00%
# Therefore, for different levels of program, proportions of female and male is close to proportion of female and male in total population
# Therefore, gender is not a good predictor.

table(d0$ses,d0$prog)
round(prop.table(table(d0$ses,d0$prog),2)*100,2)
# The proportion of high SES choosing the three different program is different,40.00%, 20.00%, 14.00%
#Likewise, the proportion of low and middle SES choosing the three different program are different
# Therefore, SES is a good predictor to predict the program they choose. 

#b)
library(nnet)
m1=multinom(prog~.,data=d0)
summary(m1)

#Coefficients read       write        math      science    socst
#general  -0.05445264 -0.03716360 -0.1037470 0.1065258 -0.01786542
#vocation -0.04078359 -0.03220268 -0.1099712 0.0537472 -0.07959798
#the unexpected coefficients is the what science subject have 
#which is the only one subject have positive influence 
#to choose general or vocation program rather than academic program
#(Since the base level of program is academic, so when other predictors hold
#the score of subject science is high, the probability of choosing general and vocation program will increase
#And because the sum of probability of three program is 1
#the probability of choosing academic program will decreases.)

#c)
n1=nrow(d0)
step1=stepAIC(m1,k=log(n1))
#iteration stop when BIC=374.52, the variables I choose is math and socst


#d)
m3=multinom(prog~math+science,data=d0)
par(mfrow=c(1,2))
#predict probabilities
pi.hat=predict(m3,newdata = d0,type="probs")
ypred = apply(pi.hat,1,which.max)
labels=c("academic","general","vocation")
colors = c("black","green","red")
plot(math~science,d0,col=d0$prog,pch=18,main="observed")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()
plot(math~science,d0,col=ypred,pch=18,main="predicted")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()

#error rate
1-sum(diag(prop.table(table(d0$prog,ypred))))
#so the error rate is 42.5%

#e)
library(class)
y = d0$prog
str(d0)
x = d0[,c(8,9)] #math and science
str(x)
x=scale(x)
head(x)
#k=3
set.seed(1)
ypred3=knn(x,x,y,3)
par(mfrow=c(1,2))
plot(math~science,d0,col=d0$prog,pch=18,main="observed")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()
plot(math~science,d0,col=ypred3,pch=18,main="predicted")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()
#error rate
1-sum(diag(prop.table(table(y,ypred3))))
#so the error rate is 31%

#k=5
set.seed(1)
ypred5=knn(x,x,y,5)
plot(math~science,d0,col=d0$prog,pch=18,main="observed")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()
plot(math~science,d0,col=ypred5,pch=18,main="predicted")
legend("topleft",legend=labels,bty="n",text.col = colors,cex = 0.7)
grid()
#error rate
1-sum(diag(prop.table(table(y,ypred5))))
#so the error rate is 32.5%

