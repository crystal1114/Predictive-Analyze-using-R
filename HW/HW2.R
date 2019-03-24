install.packages("MASS")
install.packages("PASWR2")
install.packages("lattice")
install.packages("ggplot2")
install.packages("leaps")
library(leaps)
library(MASS)
library(lattice)
library(ggplot2)
library(PASWR2)
setwd("/Users/shutingchen/Desktop/ISE 529               Data Analytics/HW")
d0=read.csv("homes.csv",header = T)

#1
d1=d0[,c(1,2,3,4,5,6,8)]
cor(d1)
# baths and area are the predictors with highest correlation

#2
subset(d1,area,subset=price==max(d0$price))
#the area of the most expensive house is 3857 or use another way
a=which.max(d0$price)
d2=d0[a,]
d2
#Fit the full model.
m1=lm(price~.,d1)
coef(m1)
#full model: yhat=-3.567709e+06 + 1.257386e+02*area+ -1.304139e+04*beds+  7.987552e+03*baths+  2.253038e+04*garage +  1.779611e+03*year+  1.554990e+00*lotsize 

#3
checking.plots(m1)
#outliers(72,73,96)
plot(m1)
d1[c(73),]
#the largest outlier is in row 73


#4
confint(m1,level = 0.99)
#99% conﬁdence interval for area is(1.077357e+02,1.437415e+02)

#5
newval=data.frame(area=2650,beds=3,baths=3,garage=2,year=1990,lotsize=24500)
predict(m1,newval,interval = "conf",level = 0.95)
# 95% conﬁdence interval for the mean price is(362128.4，387712.6)

#6 the predicted price when all predictors are equal to their median values
#best subset of predictors (in terms of adj-R 2 )

summary(models)
a=summary(models)$adjr2
which.max(a)
#best model is in row 6
#best model includes  area, beds， baths，garage, year, lotsize
#these variables are highly correlated with price
newval2=data.frame(area=mean(d1$area),beds=mean(d1$beds),baths=mean(d1$baths),garage=mean(d1$garage),year=mean(d1$year),lotsize=mean(d1$lotsize))
predict(m1,newval2)

#full model: yhat=-3.567709e+06 + 1.257386e+02*area+ -1.304139e+04*beds+  7.987552e+03*baths+  2.253038e+04*garage +  1.779611e+03*year+  1.554990e+00*lotsize 

#7 best predictor is area, worst predictor is baths

m2=lm(price~beds,d1)
coef(m2)
#price=82808.80 + 56200.08*beds
#8 Interpret the slope value b:This means with number of bedroom increasing by 1, the price increases by $56200.08, on average

d3=subset(d1,subset = beds == 2|beds == 3|beds == 4)
m3=lm(price~.,d3)
coef(m3)
#full model for houses having between two to four bedrooms
#yhat2=-3.240850e+06  1.327428e+02*area -1.274040e+04*beds  8.812354e+03*baths  2.207092e+04*garage  1.603830e+03*year  1.581008e+00 *lotsize

#9
anova(m3)
#MSE=4.2413e+09, is the estimate of the variance 

summary(m3)
#R^2=0.7584, 75.84% of variation of prices is explained by variation of model factors

#10
newval3=data.frame(area=3150,beds=2,baths=3,garage=2,year=1996,lotsize=26250)
predict(m3,newval3,interval = "pred",level = 0.95)
#95% prediction interval for the price is （334969.9，595299.1）
