###############################################################
# Title:        ps5.r
# Author:       David Joy
# Date:         2017-12-05
# Description:  Turn-in product for problem set 5
###############################################################

rm(list=ls(all=TRUE))

## Import packages
library(data.table)
library(sandwich) 
library(lmtest) 

# QUESTION 1 
## Data import and validation
context1 <- fread('WAGE1.csv')

## setting seed & clusters 
seed <-  2
maxClusters <- 10

## elbow method to determine no of optimal clusters 

wss <- rep(-1,maxClusters)
for( i in 1:maxClusters){
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10) 
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,wss,type='b')

# nstart=10 , This means that R will try 10 different random starting assignments 
# and then select the one with the lowest within cluster variation.

# K means Clustering 
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers
groups <- model1$cluster
groups                                          # cluster numbers by observation 
table(model1$cluster)                           # gives observations in each cluster 

#segmenting the clusters & Running linear regression models for cluster groups 
context1$clusters <- model1$cluster
model2 <- lm(wage~educ+exper+tenure,data=context1[clusters==1])
model3 <- lm(wage~educ+exper+tenure,data=context1[clusters==2])
model4 <- lm(wage~educ+exper+tenure,data=context1[clusters==3])
           

#summary of regression output 
summary(model2)
summary(model3)
summary(model4)

# Interpretations 
# a) the optimal number of clusters for this data set after using elbow method is 3. 

# b) We observe after clustering that in cluster 1 the exper and tenure means are largest whereas cluster 2 has 
#    lowest means for exp and tenure, we also observe that the wage means for cluster 1 and 2 is similar however 
#    cluster 3 is larger mean for wage. The education means for cluster 2 & 3 are similar wereas for cluster 1 is low.

# c) Model 3 we are observe that all the 3 variables educ, exper and tenure are significant and an increase in any 
#    of these variables increases wage, however experience doesn't seem to be significant in model 2 and model 4. 
#    we also observe that the coefficients of model 4 for educ and tenure are greater than model 2 , implying that the 
#    effect of increase in these variables will be higher in cluster 4 than cluster 2. 


# QUESTION 2 
library(tseries)

context2 <- read.csv('ffportfolios.csv')
Xdata <- context2[,2:33]
ts.plot(context2$Portfolio1)

kpss.test(context2$Portfolio1,null="Level") # Do not reject null
kpss.test(context2$Portfolio2,null="Level") # Do not reject null
kpss.test(context2$Portfolio3,null="Level") # Do not reject null
kpss.test(context2$Portfolio4,null="Level") # Do not reject null
kpss.test(context2$Portfolio5,null="Level") # Do not reject null
kpss.test(context2$Portfolio6,null="Level") # Do not reject null
kpss.test(context2$Portfolio7,null="Level") # Do not reject null
kpss.test(context2$Portfolio8,null="Level") # Do not reject null
kpss.test(context2$Portfolio9,null="Level") # Do not reject null
kpss.test(context2$Portfolio10,null="Level")# Do not reject null
kpss.test(context2$Portfolio11,null="Level")# Do not reject null
kpss.test(context2$Portfolio12,null="Level")# Do not reject null
kpss.test(context2$Portfolio13,null="Level")# Do not reject null
kpss.test(context2$Portfolio14,null="Level")# Do not reject null
kpss.test(context2$Portfolio15,null="Level")# Do not reject null
kpss.test(context2$Portfolio16,null="Level")# Do not reject null
kpss.test(context2$Portfolio17,null="Level")# Do not reject null
kpss.test(context2$Portfolio18,null="Level")# Do not reject null
kpss.test(context2$Portfolio19,null="Level")# Do not reject null
kpss.test(context2$Portfolio20,null="Level")# Do not reject null
kpss.test(context2$Portfolio21,null="Level")# Do not reject null
kpss.test(context2$Portfolio22,null="Level")# Do not reject null
kpss.test(context2$Portfolio23,null="Level")# Do not reject null
kpss.test(context2$Portfolio24,null="Level")# Do not reject null
kpss.test(context2$Portfolio25,null="Level")# Do not reject null
kpss.test(context2$Portfolio26,null="Level")# Do not reject null
kpss.test(context2$Portfolio27,null="Level")# Do not reject null
kpss.test(context2$Portfolio28,null="Level")# Do not reject null
kpss.test(context2$Portfolio29,null="Level")# Do not reject null
kpss.test(context2$Portfolio30,null="Level")# Do not reject null
kpss.test(context2$Portfolio31,null="Level")# Do not reject null
kpss.test(context2$Portfolio32,null="Level")# Do not reject null

############################
## Dimension reduction
############################
model5 <- prcomp(Xdata)  
# model5 <- prcomp(Xdata,center = TRUE,scale.=TRUE)   #standarizing data during pca 
screeplot(model5,type="lines") # looks like there are 1 principal component exmplains most of the variation 
print(model5)   # provides Standard deviation of each of the Principal Componenets and rotation ie x coeficient 
summary(model5)

#store factor in context2 & standardize 

context2$factor <- model5$x[,1]
factors <- model5$x[,1:2]
context2$scaledfactor <- scale(context2$factor)#standardizing the column to mean 0 and var 1 
context2$factors <- factors %*% solve(sqrtm(crossprod(factors))) * sqrt(nrow(factors)) 
context2$factorm <- context2$factor %*% solve(sqrtm(crossprod(context2$factor))) * sqrt(nrow(context2)) 
crossprod(context2$scaledfactor)/nrow(context2$scaledfactor)
cov(context2$scaledfactor) #mean zero, var one

# Values with standardized factor less than -2.58 
yearslessthan <- subset(context2,scaledfactor < -2.58)
yearslessthan$Year

# Secondary method for Standardizing and analyzing 
factor <- model5$x[,1]
factor2 <- model5$x[,2]
factors <- model5$x[,1:2]  # same as factor and factor 2 combined 
model5$rotation[,1]

# plotting to see first 2 components 
plot(factor,factor2)
boxplot(factor,factor2)
dim(model5$x)
biplot(model5, scale = 0) # to check scales 

# Interpretations 
# 
# a) Based on the screenplot the number of prinicpal components we should use is 1. 
# 
# b)  yearslessthan -2.58  
#  
#    [1] 1970.250 1973.833 1978.750 1980.167 1987.750 1998.583 2001.667 2002.500 2008.750 2009.083
#
#    This prinicipal component captures variations in the variables where there have been high negative returns during the years. 
#    We can see that most of the years where the factor is less than -2.58 , the protolio returns are highly negative. 
#    hence, this PCA component is capturing this variation across portfolios. 



