#1 a)
d0=data.frame(USArrests)
d1=daisy(d0)
seg.hc1=hclust(d1,method = "complete") 
plot(seg.hc1,cex=0.5,xlab = "")
grid()

# b)
cut1=rect.hclust(seg.hc1,k=3,border = "red")
seg.hc1.segment=cutree(seg.hc1,k=3)
cut1


# c)
d2=daisy(d0,metric = "gower")
seg.hc2=hclust(d2,method = "complete") 
plot(seg.hc2,cex=0.5,xlab = "")
grid()
cut2=rect.hclust(seg.hc2,k=3,border = "red")
seg.hc2.segment=cutree(seg.hc2,k=3)
cut2

table(seg.hc1.segment, seg.hc2.segment)

#We don't get the same clusters so it effects the hierarchical clustering.

#see some similarity of  Illinois 13 and New York 32
#two merge very early in cluster 1 from cluster dendrogram using not scaling data
d0[c(13,32),]
#see some similarity of Iowa 15 and New Hampshire 29
#two merge very early in cluster 1 from cluster dendrogram using scaling data
d0[c(15,29),]
#the difference of most similiar one in each group from cluster dendrogram 
#using not scaling data is bigger than scaling data

#so the data should be scaled before the inter-observation dissimilarities are computed
#since different variables are measure in different units


#2 a)
library(MASS)
library(tree)
d0=data.frame(UScereal)
str(d0)
#method1
#to test whether we can classify the manufacture by using classification tree method 
tree0=tree(mfr~.,d0)
plot(tree0)
text(tree0,cex=0.75)
summary(tree0)
#Variables actually used in tree construction: "fat"     "sugars"  "protein" "fibre"   "carbo"  
#but as we can see from the cereal characteristics discriminate the K,G,P manufacturers are fat, fibre
#other 3 manufacturers do not shown in this tree 
#so in general, we can conclude that they each have a balanced portfolio of cereals

#method2
#use cluster dendrogram method to cluster the cereals 
#and see whether different cluster are mainly produced by same manufacture
d1=daisy(d0)
seg.hc1=hclust(d1,method = "complete") 
str(seg.hc1)
plot(seg.hc1,cex=0.5,xlab = "")
grid()
cut1=rect.hclust(seg.hc1,k=6,border = "red")
seg.hc1.segment=cutree(seg.hc1,k=6)
table(d0$mfr)
(22+21+9)/65
#as shown in table, the major manufactures are G, K and P, they produce 80% of cereals
table(seg.hc1.segment,d0$mfr)
#analyze from that table, most of cereals manufactures G, K and P produce are in cluster 2 and 3
#the small difference is that G and K are also produce few cluster 5 cereals(which is only produce by them)
#and P is the only manufacture produce 2 cereals in cluster 4
#since major manufacturers G, K and P are all produce cereals in cluster 2 and 3,
round(prop.table(table(seg.hc1.segment,d0$mfr),2),3)
#and the different cereals they produce have only small proportion so we still hard to discriminate them by cereal characteristics
#so we can conclude that they each have a balanced portfolio of cereals


#2 b)
table(seg.hc1.segment)
summary(d0)
#as we get from a) cut1
#cluster 1, 2, 3, 5 only have 2, 3, 3, 5 cereal seperately,
#after I look at their measurements indivisually
#cluster1, cereal row number is 31, 32
d0[31,]
d0[32,]
which.max(d0$calories)
d1=d0
d1[31,]$calories=0
which.max(d1$calories)
#for cluster 1 they are the two have highest two calories,
#high protein, high fibre, both in shelf 3 and enriched vitamins


#cluster2, cereal row number is 1, 2, 3
d0[1,]
d0[2,]
d0[3,]
#for cluster 2, they all have high protein, high fibre and high potassium 
#they all almost achieve the max of all cereals
#and they are all in shelf 3 and enriched vitamins 

#cluster3, cereal row number is 47, 54, 55
d0[47,]
d0[54,]
d0[55,]
#for cluster 3, they are all 0 fat, 0 suger, none vitamins

#cluster5, cereal row number is 36, 46, 58, 59, 60
a=d0[c(36,46,58,59,60),]
a
#for cluster 5, these cereals are all low fat, low fibre, in shelf 3 and 100% vitamins


#for cluster 4 and cluster 6, which have most of cereals
table(seg.hc1.segment)
#as is shown, the cluster number 2 from seg.hc1.segment is corresponding with cut1 cluster 4
#cluster number 3 is corresponding with cut1 cluster 6

cmeans=function(data,groups) aggregate(data,list(groups),function(x)mean(as.numeric(x)))
cmeans(d0,seg.hc1.segment)
#for cluster 4, they are low protein, low fat, low fibre and low potassium cereals
#for cluster 6, they are the cereals which have medium value in most measurements


#2 c)
table(d0$shelf,seg.hc1.segment)
round(prop.table(table(d0$shelf,seg.hc1.segment),1),3)
#as table show shelf do not have much relation with previous 6 clusters
table(d0$shelf,d0$mfr)
#as table show shelf do not have much relation with manufactures

#so further analyze whether it depends on characteristics of cereals
d1=subset(d0,shelf==1)
summary(d1)
#calories         protein           fat            sodium          fibre           
#Median :111.82   Median :2.6667   Median :0.0000   Median :236.0   Median :1.467 
#carbo             sugars          shelf     potassium         
#Median :19.51   Median : 3.739   Median :1   Median : 82.00   
#low fat, low sugars

d2=subset(d0,shelf==2)
summary(d2)
#calories         protein           fat            sodium          fibre          
#Median :122.50   Median :1.333   Median :1.167   Median :180.0   Median :0.000
#carbo             sugars          shelf     potassium          
#Median :13.50   Median :12.50   Median :2   Median : 54.17      
#low protein low fibre, low carbo and low potassium 
d3=subset(d0,shelf==3)
summary(d3)
#calories         protein           fat            sodium          fibre  
#Median :179.1   Median : 4.478   Median :1.333   Median :280.0   Median : 4.000   
#carbo             sugars          shelf     potassium          
#Median :21.00  Median :12.000   Median :3   Median :220.0 
#high calories, high protein, high fibre

#so in general, cereals display on shelf 1 are low fat and low sugars,
#cereals display on shelf 2 are low protein low fibre, low carbo and low potassium 
#cereals display on shelf 3 are high calories, high protein, high fibre


# 3)
setwd("/Users/shutingchen/Desktop/ISE 529               Data Analytics/L13")
set.seed(1)
d0=read.csv("cereals.csv")
d1=d0[,-which(names(d0)%in% "shelf")]
d1=d1[,-c(1,2,3)]
myKmeans<-function(dataSet,k){
  p=ncol(dataSet) 
  n=nrow(dataSet)
  #randomly assign the cluster to each row
  t=sample(1:k,size=nrow(dataSet),replace = T)
  #create matrices to store the cluster, mean and dist
  pointProperty=matrix(data=t,nrow =n ,ncol = 1)
  centerPointSet<-matrix(data = NA,nrow = k,ncol = p)
  dist<-matrix(data=NA,nrow =n,ncol = k)
  #create assign use for condition
  assign=matrix(0,nrow=n,ncol = 1)
  #stop when cluster assignments stop changing
  if(identical(assign,pointProperty)==FALSE){
    assign=pointProperty
    for (i in 1:n) {
      for (cent in 1:k) {
        #calculate the p predictors' mean for each cluster
        d=dataSet[which(pointProperty==cent),] 
        centerPointSet[cent,]=apply(d,2,mean)
        #calculate the eudist for each row to k cluster
        dist[i,cent]=daisy(rbind(dataSet[i,],centerPointSet[cent,]))
        #assign the cluster which have min eudist to that row
        pointProperty[i,]=as.numeric(which.min(dist[i,]))
      }
    }
  }
  return(pointProperty)
}

m=myKmeans(d1,4)
clusplot(d1,m,color=T,shade=T,labels=4,lines=0,main = "K-means",cex=0.5)
table(m)


library(cluster)
m1=kmeans(d1,centers=4)
clusplot(d1,m1$cluster,color=T,shade=T,labels=4,lines=0,main = "K-means",cex=0.5)
table(m1$cluster)

