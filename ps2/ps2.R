###############################################################
# Title:        ps2.r
# Author:       Amit Sawant
# Date:         26th September 2017
# Description:  Problem set 2
###############################################################


rm(list=ls(all=TRUE))

library(data.table)

####################################################################################################
########################################## Question 1 ##############################################
####################################################################################################
# Read attend.csv in context1
context1 <- fread("attend.csv")
summary(context1)

# Attendance rate variable (Number of classes attended out of 32)
context1$attendrt <- context1$attend/32

# Homework rate variable (Number of homeworks turned in out of 8)
context1$hwrt <- context1$hw/8

# model1 linear model
model1 <- lm(termGPA~priGPA+ACT+attendrt+hwrt, data=context1)
summary(model1)

# Call:
#   lm(formula = termGPA ~ priGPA + ACT + attendrt + hwrt, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.87210 -0.28100  0.00001  0.30164  1.49711 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.286983   0.164169  -7.839 1.77e-14 ***
#   priGPA       0.548962   0.042418  12.942  < 2e-16 ***
#   ACT          0.036099   0.006051   5.966 3.92e-09 ***
#   attendrt     1.052246   0.155436   6.770 2.81e-11 ***
#   hwrt         0.913031   0.116932   7.808 2.22e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.4788 on 675 degrees of freedom
# Multiple R-squared:   0.58,	Adjusted R-squared:  0.5775 
# F-statistic:   233 on 4 and 675 DF,  p-value: < 2.2e-16


################################ INTERPRETATIONS ###############################################

# a. Interpret the estimated coefficient  on attendrt from model1 (eq 1).
#   One unit of attendance rate is associated with 1.052246 unit increase  
#   in term GPA controlling for prior GPA, ACT score and Homework rate.
#
# b. Interpret the estimated coe¢ cient on hwrt from model1 (eq 1).
#   One unit of homework rate is associated with 0.913031 unit increase
#   in term GPA controlling for prior GPA, ACT score and attendance rate.
#
# c. Predict the termGPA for a student with a 32 ACT and a 2.2 priGPA who attended 28 lectures and
#   turned-in 8 homework assignments.
   GPA1c <-  -1.286983 + 32*(0.036099) + 2.2*(0.548962) + (28/32)*(1.052246) + (8/8)*(0.913031)
   GPA1c
#  GPA1c =  2.909648
#
# d. Predict the termGPA for a student with a similar attendence and homework pattern who had a 20 ACT
#   and a 3.9 priGPA.
   GPA1d = -1.286983 + 20*(0.036099) + 3.9*(0.548962) + (28/32)*(1.052246) + (8/8)*(0.913031)
   GPA1d 
#  GPA1d = 3.409695
#
# e. Intuitively, which variable is more important to the termGPA, ACT or priGPA?
#   priGPA is more important to term GPA
#
# f. Predict the termGPA for a student with a 25 ACT and a 3.0 priGPA who attends all the classes, but
# only finishes half the homework assignments.
   GPA1f = -1.286983 + 25*(0.036099) + 3.0*(0.548962) + (32/32)*(1.052246) + (4/8)*(0.913031)
   GPA1f 
#  GPA1f = 2.77114    
#
# g. Predict the termGPA for a similarly qualified student who turns in all the homwork assignments, but
# only attends half the classes.
   GPA1g = -1.286983 + 25*(0.036099) + 3.0*(0.548962) + (16/32)*(1.052246) + (8/8)*(0.913031)
   GPA1g
#  GPA1g = 2.701532
#
# h. Intuitively, which variable is more important to the termGPA, attendance or homework completion?
#   Attendance is more important to term GPA
#
# i. Why is it easier to compare attendrt and hwrt than it is to compare priGPA and ACT score?
#   It is easier to compare attendrt and hwrt because they are converted into rates(percentages) 
#   and so they have the same range of values ranging between 0 and 1.
#
####################################################################################################
################################# Question 2 #######################################################
####################################################################################################
   
# Read CEOSAL2.csv into context2
context2 <- fread("CEOSAL2.csv")
summary(context2)

# model 2
model2 <- lm(log(salary)~log(mktval)+profits+ceoten, data=context2)
summary(model2)
# 
# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten, data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.63382 -0.34660  0.00627  0.35059  1.96220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.7095052  0.3954502  11.909  < 2e-16 ***
#   log(mktval) 0.2386220  0.0559166   4.267 3.25e-05 ***
#   profits     0.0000793  0.0001566   0.506   0.6132    
# ceoten      0.0114646  0.0055816   2.054   0.0415 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5289 on 173 degrees of freedom
# Multiple R-squared:  0.2514,	Adjusted R-squared:  0.2384 
# F-statistic: 19.36 on 3 and 173 DF,  p-value: 7.141e-11

# model 3
model3 <- lm(log(salary)~log(mktval)+profits+ceoten+log(sales), data=context2)
summary(model3)

# 
# Call:
#   lm(formula = log(salary) ~ log(mktval) + profits + ceoten + log(sales), 
#      data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.48792 -0.29369  0.00827  0.29951  1.85524 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.558e+00  3.803e-01  11.986  < 2e-16 ***
#   log(mktval) 1.018e-01  6.303e-02   1.614   0.1083    
# profits     2.905e-05  1.503e-04   0.193   0.8470    
# ceoten      1.168e-02  5.342e-03   2.187   0.0301 *  
#   log(sales)  1.622e-01  3.948e-02   4.109 6.14e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5062 on 172 degrees of freedom
# Multiple R-squared:  0.3183,	Adjusted R-squared:  0.3024 
# F-statistic: 20.08 on 4 and 172 DF,  p-value: 1.387e-13

################################ INTERPRETATIONS ###############################################
# 
# j. We used natural logs on all the dollar-valued quantities except proits in models 2 & 3 (eq. 2 & 3). Why
#  did we not take the log of profits?
#   We get the following output when log is applied to profits.
#   model2 <- lm(log(salary)~log(mktval)+log(profits), data=context2)
#   Warning message:
#   In log(profits) : NaNs produced
#   The natural logarithm function ln(x) is defined only for x>0.
#   So the natural logarithm of a negative number gives NaN error. 
#   The profits column in the data-set contains negative values. 
#   Hence we cannot use log of profits in models 2 & 3.
#
# k. Interpret the estimated coefficient on log mktval in model 2 (eq 2).
#   Every one percent increase in market value is associated with a 0.2386220 percent increase
#   in salary controlling for profits and Ceo Tenure (ceoten)
#
# l. Interpret the estimated coefficient on log mktval in model 3 (eq 3).
#   Every one percent increase in market value is associated with a 0.1018 percent increase
#   in salary controlling for profits, Ceo Tenure (ceoten) and sales
#
#
# m. Compare the test statistics on log mktval between model 2 and model 3. Please explain the differences
#    you find in terms of the biases we discussed in class.
#   As per the observation, we can conclude that in model 2 we omitted the sales variable and we got
#   market value as a significant variable. When we included sales in model 3, market values was 
#   not significant. This is a case of Omitted-Variable Bias.
#
# n. Is the coefficient on profits significant in model 3 (eq 3)?
#   No, the coefficient on profits is not significant in model 3 
#
# o. Interpret the estimated coefficient on log sales in model 3 (eq 3).
#   Every one percent increase in sales is associated with a 0.1622 percent increase
#   in salary controlling for market value, profits, and Ceo Tenure (ceoten).
#
#####################################################################################################
################################# Question 3 #######################################################
####################################################################################################

# Read hprice1.csv in context3
context3 <- fread("hprice1.csv")
summary(context3)

#model 4
model4 <- lm(price~bdrms+log(lotsize)+log(sqrft)+colonial, data=context3)
summary(model4)

# 
# Call:
#   lm(formula = price ~ bdrms + log(lotsize) + log(sqrft) + colonial, 
#      data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -109.603  -38.258   -4.325   22.984  220.766 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -2030.455    210.967  -9.625 3.68e-15 ***
#   bdrms           18.572      9.308   1.995   0.0493 *  
#   log(lotsize)    61.446     12.372   4.966 3.60e-06 ***
#   log(sqrft)     225.508     30.072   7.499 6.41e-11 ***
#   colonial         4.134     14.509   0.285   0.7764    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 59.66 on 83 degrees of freedom
# Multiple R-squared:  0.6781,	Adjusted R-squared:  0.6626 
# F-statistic: 43.71 on 4 and 83 DF,  p-value: < 2.2e-16


# model 5
model5 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft)+colonial, data=context3)
summary(model5)

# 
# Call:
#   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft) + 
#        colonial, data = context3)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.69479 -0.09750 -0.01619  0.09151  0.70228 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.34959    0.65104  -2.073   0.0413 *  
#   bdrms         0.02683    0.02872   0.934   0.3530    
# log(lotsize)  0.16782    0.03818   4.395 3.25e-05 ***
#   log(sqrft)    0.70719    0.09280   7.620 3.69e-11 ***
#   colonial      0.05380    0.04477   1.202    0.2330    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1841 on 83 degrees of freedom
# Multiple R-squared:  0.6491,	Adjusted R-squared:  0.6322 
# F-statistic: 38.38 on 4 and 83 DF,  p-value: < 2.2e-16

################################ INTERPRETATIONS ###############################################
#
# p. Interpret the estimated coefficient on log lotsize from model4 (eq 4).
#   Every 1 percent increase in lot size is associated with 0.61446 unit (614.46 $) increase 
#   in price controlling for number of bedrooms, size of house in square feet and colonial style.
#
#
# q. Interpret the estimated coefficient on log lotsize from model5 (eq 5).
#   Every 1 percent increase in lot size is associated with 0.16782 percent increase
#   in price controlling for number of bedrooms, size of house in square feet and colonial style.
#
# r. Interpret the estimated coefficient on colonial from model4 (eq 4, please ignore significance here).
#   A colonial style home is associated with 4.134 unit ( 4134$ ) increase in price
#   controlling for lot size, number of bedrooms, and size of house in square feet.
#
#
# s. Which model (4 or 5) better fits the data for this data set? On what criterion/criteria are you basing
#   your judgement?
#   Model 4 better fits the data for this data set. Model 4 has higher Adjusted R squared value.
#   Model 4 Adjusted R squared = 0.6626 whereas, Model 5 Adjusted R squared =  0.6322   
#
# t. Suppose your house is worth $300k. You are considering an expansion of your home to add a master suite
#   (+1 bedroom to your home). This expansion would increase your square-footage by 10% and would cost
#   $50k. You have valued your enjoyment of the additional space at $20k, so you would only be willing to
#   consider the build if it were to also increase your property value accordingly. Does the appropriate model
#   indicate that you should pursue the expansion?
#   
#   Using model 4
#   New Price of house = 300 + 18.572 * 1 [additional 1 bedroom] + 225.508 * 0.10 [10% additional sqft]
#   300+(18.572)+(0.10*225.508)
#   = $341.1228k [This is the cost of house after increasing the property values as per model]
#   Now you have valued your enjoyment of space at $20k 
#    341.1228 + 20 = $361.1228k [Final Price of house]
#   As mentioned, The cost to build is $50k so cost price 300 + 50 = $350k
#   Thus, the model indicates that I should pursue the expansion.


#####################################################################################################
################################# Question 4 #######################################################
####################################################################################################

# Read JTrain2.csv in context4
context4 <- fread("JTRAIN2.csv")
summary(context4)

# model 6
model6 <- lm(re78~re75+train+educ+black, data=context4)

summary(model6)
# 
# Call:
#   lm(formula = re78 ~ re75 + train + educ + black, data = context4)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -9.120 -4.377 -1.756  3.353 54.058 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  1.97686    1.89028   1.046   0.2962   
# re75         0.14697    0.09811   1.498   0.1349   
# train        1.68422    0.62700   2.686   0.0075 **
#   educ         0.41026    0.17267   2.376   0.0179 * 
#   black       -2.11277    0.82941  -2.547   0.0112 * 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.496 on 440 degrees of freedom
# Multiple R-squared:  0.04917,	Adjusted R-squared:  0.04053 
# F-statistic: 5.688 on 4 and 440 DF,  p-value: 0.00018

################################ INTERPRETATIONS ###############################################
#

# u. Interpret the estimated coefficient on re75 from model6 (eq 6).
#   Every unit increase in real earns in 1975 is associated with 0.14697 unit increase
#   in real earns in 1978 controlling for job training, years of education and black
#
# v. Interpret the estimated coefficient on train from model6 (eq 6). Is the coefficient significant?
#   Job training (train = 1) is associated with 1.68422 unit increase in real earns in 1978
#   controlling for real earns in 1975, years of education and black.
#   Yes, the coefficient Train is significant
#
# w. Interpret the estimated coefficient on black from model6 (eq 6).
#   Being black (black = 1 ) is associated with 2.11277 unit decrease in real earns in 1978
#   controlling for real earns in 1975, job training and  years of education.
#



