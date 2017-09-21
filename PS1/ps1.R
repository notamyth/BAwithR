###############################################################
# Title:        ps1.r
# Author:       Amit Sawant
# Date:         12th September 2017
# Description:  Problem set 1
###############################################################

# set working directory
setwd("~/Desktop/BA with R/ps1/ps1-selected")

# clear environment
rm(list=ls(all=TRUE))

#data.table package in library
library(data.table)

# Reading WAGE1.csv to context1
context1 <-  fread("WAGE1.CSV")

#summary of context1
summary(context1)

# natural log of wage 
lwage <-  log(context1$wage)

# model1
model1 <- lm(wage~educ, data=context1)
summary(model1)
#Call:
#  lm(formula = wage ~ educ, data = context1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-5.3707 -2.1578 -0.9854  1.1864 16.3975 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.93389    0.68769  -1.358    0.175    
#educ         0.54470    0.05346  10.189   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 3.392 on 524 degrees of freedom
#Multiple R-squared:  0.1654,	Adjusted R-squared:  0.1638 
#F-statistic: 103.8 on 1 and 524 DF,  p-value: < 2.2e-16


#model2
model2 <-  lm(wage~educ+exper+tenure, data=context1)
summary(model2)


#Call:
#  lm(formula = wage ~ educ + exper + tenure, data = context1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.6498 -1.7708 -0.6407  1.2051 14.7201 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.91354    0.73172  -3.982 7.81e-05 ***
#  educ         0.60268    0.05148  11.708  < 2e-16 ***
#  exper        0.02252    0.01210   1.861   0.0633 .  
#tenure       0.17002    0.02173   7.825 2.83e-14 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 3.096 on 522 degrees of freedom
#Multiple R-squared:  0.3072,	Adjusted R-squared:  0.3032 
#F-statistic: 77.15 on 3 and 522 DF,  p-value: < 2.2e-16

#model3

model3 <- lm(lwage~educ+exper+tenure, data=context1)
summary(model3)

#Call:
#  lm(formula = lwage ~ educ + exper + tenure, data = context1)
#
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.05911 -0.29563 -0.03302  0.28590  1.42657 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.282635   0.104331   2.709  0.00697 ** 
#  educ        0.092256   0.007340  12.569  < 2e-16 ***
#  exper       0.004137   0.001726   2.397  0.01687 *  
#  tenure      0.022112   0.003098   7.138 3.19e-12 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4415 on 522 degrees of freedom
#Multiple R-squared:  0.3165,	Adjusted R-squared:  0.3125 
#F-statistic: 80.56 on 3 and 522 DF,  p-value: < 2.2e-16

# Since we are using log on wage, there will be a percent change associated 
# in the hourly wage with respect to unit change in each variable.
coef(model3)*100

# (Intercept)        educ       exper      tenure 
# 28.2634948   9.2256203   0.4136804   2.2111673

#############################################################
#Interpretations:
##############################################################
#a. Interpret the estimated coefficient on educ from model1 (eq 1).
#   Every one year of education is associated with 0.545 unit increase
#   in hourly wage. 
#
#b. Interpret the estimated coefficient on educ from model2 (eq 2).
#   Every one year of education is associated with 0.603 unit increase
#   in hourly wage controlling for experience and tenure.
#
#c. Interpret the estimated coefficient on exper from model2 (eq 2).
#   Every one year of experience is associated with 0.0225 unit increase 
#   in hourly wage controlling for education and tenure
#
#d. Interpret the estimated coefficient on tenure from model2 (eq 2).
#   Every one year of tenure is associated with 0.170 unit increase 
#   in hourly wage controlling for experience and education.
#
#e. Interpret the estimated intercept from model2 (eq 2).
#   The average wage for a person having no education, 
#   no experience and no tenure is -2.91354 units per hour
#   (since hourly wage cannot be negative, we can consider it zero).
#
#f. Interpret the estimated coefficient on educ from model3 (eq 3).
#   Every one year of education is associated with 9.226% increase 
#   in hourly wage controlling experiece and tenure.
#
#g. Interpret the estimated coefficient on exper from model3 (eq 3).
#   Every one year of experience is associated with 0.414% increase 
#   in hourly wage controlling education and tenure.
#
#h. Interpret the estimated coefficient on tenure from model3 (eq 3).
#   Every one year of tenure is associated with 2.211% increase 
#   in hourly wage controlling experiece and education.