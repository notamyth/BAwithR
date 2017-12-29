###############################################################
# Title:        ps5.r
# Author:       Jason Parker
# Date:         2017-12-24
# Description:  Problem Set 5
###############################################################

rm(list=ls())
library(data.table)
context1 <- read.csv("Wage1.csv")

############################
## K-means Estimation
############################
?kmeans
seed        <-	2	# NOT a good random seed!
maxClusters	<-	10 #try with 50, then with 15

## Use within-group variation to choose k
wss	<- rep(-1,maxClusters)
for (i in 1:maxClusters) { # i represents the k value
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10)
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,	wss, type="b", 
     xlab="Number of Clusters",
     ylab="Aggregate Within Group SS")

## Run the model
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers
groups1 <- model1$cluster
groups1


############################
## Clustering in Practice
############################
context1 <- fread("WAGE1.csv")
context1$cluster <- groups1

model2 <- lm(wage~educ+exper+tenure,data=context1[cluster==1])
model3 <- lm(wage~educ+exper+tenure,data=context1[cluster==2])
model4 <- lm(wage~educ+exper+tenure,data=context1[cluster==3])

table(groups1)
summary(model2)
summary(model3)
summary(model4)

################################################################
##INTERPRETATIONS
###############################################################
#A) Using the elbow test on the within- sum of squares plot,the optimal number of clusters for this data
#   set is 10.  
#B) On Comparing Education, Expertise and Tenure variables in each clusters we infer that 
#   education variable is more closely related with very less difference between their centroids, 
#clusters with expertise variabe has a large distance between their centroids whereas tenure variable are also having a large distance between their centroids.
#C) The significance level of variables is different in every model.Variable exper is not significant
#   in model 2 & model 4 but in model 3 all variables are significant.



####Question 2
context2 <- read.csv("ffportfolios.csv")
library(tseries)
kpss.test((context2$Portfolio1),null="Level")
kpss.test((context2$Portfolio1),null="Trend")
kpss.test((context2$Portfolio2),null="Level")
kpss.test((context2$Portfolio2),null="Trend")
kpss.test((context2$Portfolio3),null="Level")
kpss.test((context2$Portfolio3),null="Trend")
kpss.test((context2$Portfolio4),null="Level")
kpss.test((context2$Portfolio4),null="Trend")
kpss.test((context2$Portfolio5),null="Level")
kpss.test((context2$Portfolio5),null="Trend")
kpss.test((context2$Portfolio6),null="Level")
kpss.test((context2$Portfolio6),null="Trend")
kpss.test((context2$Portfolio7),null="Level")
kpss.test((context2$Portfolio7),null="Trend")
kpss.test((context2$Portfolio8),null="Level")
kpss.test((context2$Portfolio8),null="Trend")
kpss.test((context2$Portfolio9),null="Level")
kpss.test((context2$Portfolio9),null="Trend")
kpss.test((context2$Portfolio10),null="Level")
kpss.test((context2$Portfolio10),null="Trend")
kpss.test((context2$Portfolio11),null="Level")
kpss.test((context2$Portfolio11),null="Trend")
kpss.test((context2$Portfolio12),null="Level")
kpss.test((context2$Portfolio12),null="Trend")
kpss.test((context2$Portfolio13),null="Level")
kpss.test((context2$Portfolio13),null="Trend")
kpss.test((context2$Portfolio14),null="Level")
kpss.test((context2$Portfolio14),null="Trend")
kpss.test((context2$Portfolio15),null="Level")
kpss.test((context2$Portfolio15),null="Trend")
kpss.test((context2$Portfolio16),null="Level")
kpss.test((context2$Portfolio16),null="Trend")
kpss.test((context2$Portfolio17),null="Level")
kpss.test((context2$Portfolio17),null="Trend")
kpss.test((context2$Portfolio18),null="Level")
kpss.test((context2$Portfolio18),null="Trend")
kpss.test((context2$Portfolio19),null="Level")
kpss.test((context2$Portfolio19),null="Trend")
kpss.test((context2$Portfolio20),null="Level")
kpss.test((context2$Portfolio20),null="Trend")
kpss.test((context2$Portfolio21),null="Level")
kpss.test((context2$Portfolio21),null="Trend")
kpss.test((context2$Portfolio22),null="Level")
kpss.test((context2$Portfolio22),null="Trend")
kpss.test((context2$Portfolio23),null="Level")
kpss.test((context2$Portfolio23),null="Trend")
kpss.test((context2$Portfolio24),null="Level")
kpss.test((context2$Portfolio24),null="Trend")
kpss.test((context2$Portfolio25),null="Level")
kpss.test((context2$Portfolio25),null="Trend")
kpss.test((context2$Portfolio26),null="Level")
kpss.test((context2$Portfolio26),null="Trend")
kpss.test((context2$Portfolio27),null="Level")
kpss.test((context2$Portfolio27),null="Trend")
kpss.test((context2$Portfolio28),null="Level")
kpss.test((context2$Portfolio28),null="Trend")
kpss.test((context2$Portfolio29),null="Level")
kpss.test((context2$Portfolio29),null="Trend")
kpss.test((context2$Portfolio30),null="Level")
kpss.test((context2$Portfolio30),null="Trend")
kpss.test((context2$Portfolio31),null="Level")
kpss.test((context2$Portfolio31),null="Trend")
kpss.test((context2$Portfolio32),null="Level")
kpss.test((context2$Portfolio32),null="Trend")

Xdata <- context2[,2:33] 
head(Xdata)

model5 <- prcomp(Xdata)
screeplot(model5,type="lines")

factors <- model5$x[,1]
head(factors)
summary(factors)
#cov(factors)
var(factors)

library(expm)
factors <- factors/ sqrt(var(factors)) 
# crossprod(factors)/nrow(factors)
# cov(factors)
var(factors)

yearLists = toString(context2$Year[factors < -2.58])
yearLists
model5$factors <- factors
summary(model5)
#year values with standardized factors less than 1

#Answers for Question 2
#a) 1 PCA is enough to cover the maximum vairance in the data as inferred from screenplot graph.

#b) Looking at the years where the standardized factor is less than the first percentile (????2:58),
#   this principal component will be reason of US financial crises.