library(lattice)
library(ggplot2)
library(PASWR2)
library(MASS)
library(boot)
#1
#AIC
d0=data.frame(VIT2005)
str(d0)
m0=lm(totalprice~.,d0)
step1=stepAIC(m0)

m1=glm(totalprice ~ area + zone + category + age + floor + toilets + garage + elevator + heating + storage,data=d0)
yhat1=predict(m1,d0)
plot(yhat1~totalprice,d0)
abline(0,1,col="red")
#BIC
n1=nrow(d0)
step2=stepAIC(m0,k=log(n1))
m2=glm(totalprice~area + zone + category + age + toilets + garage + elevator,data=d0)
yhat2=predict(m2,d0)
plot(yhat2~totalprice,d0)
abline(0,1,col="red")
#5-fold cross validation MSPE of the models m1 and m2
cverrors1=cv.glm(d0,m1,K=5)$delta[1]
cverrors1
cverrors2=cv.glm(d0,m2,K=5)$delta[1]
cverrors2


#2
setwd("/Users/shutingchen/Desktop/ISE 529               Data Analytics/L5")
d1=read.csv("chlorine.csv")
head(d1)
m1=glm(Pct.Dep~Temperature+PH.Level+I(PH.Level^2)+Weather,data=d1)
summary(m1)
#p-value of Temperature=2.28e-10,is too small so we cannot accept H0, so we can infer that higher temperatures deplete chlorine more quickly
#p-value of PH.Level and (PH.Level^2)<2e-16 is too small so we cannot accept H0, and conclude that the belief about the relationship between chlorine depletion and pH level is correct
d2=d1
d2$Weather=as.factor(d2$Weather)
m2=glm(Pct.Dep~Temperature+PH.Level+I(PH.Level^2)+Weather,data=d2)
summary(m2)
#combine levels
levels(d2$Weather)
levels(d2$Weather)=c("1","2","1")
levels(d2$Weather)
m3=glm(Pct.Dep~Temperature+PH.Level+I(PH.Level^2)+Weather,data=d2)
summary(m3)
#p-value of weather2= 0.0059, is smaller than 0.01 we reject H0, so the weather have relationship with chlorine depletion.


#3
setwd("/Users/shutingchen/Desktop/ISE 529               Data Analytics/L5")
d=read.csv("commercial.csv")
str(d3)
m3=lm(Test~.,d)
summary(m4)
d4=d3
d4$Type=as.factor(d4$Type)
str(d4)
m5=lm(Test~.,d4)
summary(m5)
#p-value=0.1589 so we can conclude that the memory test score is related to the type of commercial
#Multiple R-squared:  0.3138,	Adjusted R-squared:  0.2897 
#Multiple R-squared:  0.3882,	Adjusted R-squared:  0.3554 

