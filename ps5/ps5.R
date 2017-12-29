###############################################################
# Title:        ps5.r
# Name:         Amit Sawant     
# Date:         2017-12-05
# Description:  Problem set 5
###############################################################

rm(list=ls(all=TRUE))
install.packages('expm')
library(data.table)
library(sandwich) 
library(lmtest) 
library(tseries)
library(plm)
library(expm)


# Read wage1.csv to context1
context1 <- fread('WAGE1.csv')

#seed & clusters 
seed <-  2
maxClusters <- 10

wss <- rep(-1,maxClusters)
for( i in 1:maxClusters){
  set.seed(seed)
  model <- kmeans(context1,centers=i,nstart=10) 
  wss[i] <- model$tot.withinss
}
plot(1:maxClusters,wss,type='b') #elbow test on wss plot


# K means Clustering 
set.seed(seed)
model1 <- kmeans(context1,centers=3,nstart=10)
model1$centers

model1$centers[,2] #education

# Education
# 1        2        3 
# 10.96296 13.12548 12.72258 

model1$centers[,3] #experience

# Experience
# 1         2         3 
# 38.777778  5.946768 20.638710 

model1$centers[,4] #tenure

# Tenure
# 1         2         3 
# 11.657407  1.699620  6.316129

groups <- model1$cluster
groups                                          

# segment the data into three groups and run the following linear model for clusters 1, 2,and 3
context1$clusters <- groups
model2 <- lm(wage~educ+exper+tenure,data=context1[clusters==1])
model3 <- lm(wage~educ+exper+tenure,data=context1[clusters==2])
model4 <- lm(wage~educ+exper+tenure,data=context1[clusters==3])
           

#summary 
summary(model2)
summary(model3)
summary(model4)

# Interpretations 
# a) After using elbow test on the within sum of square plot, the optimal number of clusters for this data set is 3.

# b)  Cluster 1 has lowest education and highest exper and tenure values for centers and 
#     Cluster 2 has highest education and lowest exper and tenure. 
#     Cluster 3 has 2nd highest education, 2nd highest experience & low tenure center values.

#
#c) In model 2, the intercept is positive where as in model 3 and 4 it is negative. 
#   In model 3 we are observe that all the 3 variables educ, exper and tenure are significant.
#   Experience is not significant in model 2 and model 4.
#   we also observe that the coefficients of model 4 for educ and tenure are greater than model 2 , implying that the 
#   effect of increase in these variables will be higher in model 4 than model2. 


########################################### QUESTION 2 ##############################################

## read data in context 2
context2 <- fread("ffportfolios.csv")
head(context2)

## verifying that every series is level stationary
lapply(context2[,2:ncol(context2)], kpss.test)  

model5 <- prcomp(context2[,2:33])
## Generating screeplot
screeplot(model5,type="lines")
model5$rotation[,2]*100

## geting the principal components
context2$factor <- model5$x[,1]
head(context2)
summary(context2$factor)

context2$factor <- context2$factor %*% solve(sqrtm(crossprod(context2$factor))) * sqrt(nrow(context2)) 
crossprod(context2$factor)/nrow(context2)
c(mean(context2$factor), var(context2$factor))

newdata <- subset(context2, factor<(-2.58))
year_values <- cbind(newdata$Year, newdata$factor)
year_values

## Interpretations
#a) Based on the screenplot the number of prinicpal components we should use is 1. 
# 
#b) This prinicipal component captures variation across portfolios where there have been high negative returns during the years. 
#    The protolio returns are highly negative for the years where the factor is less than -2.58  
#
