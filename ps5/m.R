###############################################################
# Title:        PS5.r
# Author:       Madhumita Jadhav
# Date:         2017-12-05
# Description:  Problem set 5
###############################################################

## Clear the workspace and setting working directory
rm(list=ls(all=TRUE))
setwd("D:/UTD/BA with R/Homeworks/ps5/ps5/")

## Load packages
library(data.table)

## Importing the dataset WAGE1.csv and viewing
context1 <- fread("WAGE1.csv")
head(context1)

## Reading Summary statistics and variables
summary(context1)

## Set Seed to 2 and maximum no of cluster to 10
seed <-	2	
maxClusters	<-	10 

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
model1
model1$centers

groups1 <- model1$cluster
groups1
context1$cluster <-groups1
model2 <- lm(wage~educ+exper+tenure,data=context1[cluster==1])
model3 <- lm(wage~educ+exper+tenure,data=context1[cluster==2])
model4 <- lm(wage~educ+exper+tenure,data=context1[cluster==3])

summary(model2)
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1[cluster ==  1])
#                                                               
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6.4894 -2.0198 -0.5852  1.2467 14.4291 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  3.78799    2.83502   1.336 0.184419    
# educ         0.40735    0.10276   3.964 0.000135 *
# exper       -0.10172    0.05851  -1.739 0.085077 .  
# tenure       0.13653    0.03098   4.407 2.55e-05 *
#   ---
#   Signif. codes:  0 ‘*’ 0.001 ‘*’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.292 on 104 degrees of freedom
# Multiple R-squared:  0.3365,	Adjusted R-squared:  0.3174 
# F-statistic: 17.58 on 3 and 104 DF,  p-value: 2.628e-09

summary(model3)
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1[cluster == 2])
#                                                                
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.7853 -1.2968 -0.3433  0.6621 12.0409 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -3.21053    0.81295  -3.949 0.000101 *
#   educ         0.52524    0.06025   8.717 3.49e-16 *
#   exper        0.17286    0.03806   4.542 8.55e-06 *
#   tenure       0.29156    0.06613   4.409 1.52e-05 *
#   ---
#   Signif. codes:  0 ‘*’ 0.001 ‘*’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.278 on 259 degrees of freedom
# Multiple R-squared:  0.3765,	Adjusted R-squared:  0.3693 
# F-statistic: 52.14 on 3 and 259 DF,  p-value: < 2.2e-16

summary(model4)
# Call:
#   lm(formula = wage ~ educ + exper + tenure, data = context1[cluster == 3])
#                                                                
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.6701 -2.2514 -0.5411  1.6947 13.8507 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -4.85740    2.03152  -2.391    0.018 *  
#   educ         0.73631    0.11888   6.194 5.30e-09 *
#   exper        0.05905    0.05871   1.006    0.316    
#   tenure       0.21796    0.04655   4.683 6.26e-06 *
#   ---
#   Signif. codes:  0 ‘*’ 0.001 ‘*’ 0.01 ‘’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.755 on 151 degrees of freedom
# Multiple R-squared:  0.2939,	Adjusted R-squared:  0.2798 
# F-statistic: 20.95 on 3 and 151 DF,  p-value: 2.118e-11


## Interpretations
#
# a)   Using k-means cluster, the optimal number of clusters for this data set is 2.
#
# b)   Cluster 1 has lowest education and highest exper and tenure values for centers and Cluster 2 has highest education and lowest exper
#      and tenure. Cluster 3 has 2nd highest education and low tenure center values. Cluster 1 can be thought of as workers who have comparatively
#      low level of education but a lot of experience as well as highest number of years of experience. where as Cluster 2 can be thought of for workers with comparatively
#      higher level of education (Like PhD, Masters) but very low experience and tenure, they can be classified as workers in initial phase of their careers. Cluster 
#      3 has workers with medium level of education and a lot of experience but lesser tenure as compared to cluster 1
#
# c)   In model 2, the intercept is positive where as in model 3 and 4 are negative. So, for cluster 1, if educ, exper and tenure are
#      all zero then the model predicts a positive number for wage. However, it is the opposite for model3 and model4.Education has a similar effect
#      on all three models and it has the highest effect in model4. Experience has a negative coefficient in model2 and positive coefficients in
#      model3 and 4. Tenure has the similar effect in all the three models. The differences that we observed in these three models in terms of a positve
#      or negative effect are for intercept and exper only.
#             
###########################################################################################

library(data.table)
library(sandwich) 
library(lmtest)
library(tseries)
library(plm)
## Importing the dataset loanapp and viewing
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

# loading exmp Library
library(expm)
context2$factor <- context2$factor %*% solve(sqrtm(crossprod(context2$factor))) * sqrt(nrow(context2)) 
crossprod(context2$factor)/nrow(context2)
c(mean(context2$factor), var(context2$factor))

newdata <- subset(context2, factor<(-2.58))
year_values <- cbind(newdata$Year, newdata$factor)
year_values

## Interpretations
# a)  Based on the screenplot the number of prinicpal components we should use is 1. 
# 
# b)  year_values 
#     [1] 1970.250 1973.833 1978.750 1980.167 1987.750 1998.583 2001.667 2002.500 2008.750
#
#    This PCA component is captures variation across portfolios,where there have been high negative returns during the years 
#    the protolio returns are highly negative for the years where the factor is less than -2.58  
#
