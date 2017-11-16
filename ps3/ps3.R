########################################
# Title:   :  ps3.r
# Author   :  Amit Sawant
# Date     :  2017-10-31
# Homework :  Problem set 3
########################################

rm(list=ls(all=TRUE))

library(data.table)
library(lmtest)
library(sandwich)
library(tseries)
library(plm)

# read hprice1 in context1
context1 <- fread("hprice1.csv")

# model1
model1 <- lm(price~bdrms+lotsize+sqrft, data=context1)

summary(model1)
# 
# Call:
#   lm(formula = price ~ bdrms + lotsize + sqrft, data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -120.026  -38.530   -6.555   32.323  209.376 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.177e+01  2.948e+01  -0.739  0.46221    
# bdrms        1.385e+01  9.010e+00   1.537  0.12795    
# lotsize      2.068e-03  6.421e-04   3.220  0.00182 ** 
#   sqrft        1.228e-01  1.324e-02   9.275 1.66e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 59.83 on 84 degrees of freedom
# Multiple R-squared:  0.6724,	Adjusted R-squared:  0.6607 
# F-statistic: 57.46 on 3 and 84 DF,  p-value: < 2.2e-16

coeftest(model1,vcov.=vcov) # Old school t test for significance (like summary)
# 
# t test of coefficients:
#   
#   Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept) -2.1770e+01  2.9475e+01 -0.7386  0.462208    
# bdrms        1.3853e+01  9.0101e+00  1.5374  0.127945    
# lotsize      2.0677e-03  6.4213e-04  3.2201  0.001823 ** 
#   sqrft        1.2278e-01  1.3237e-02  9.2751 1.658e-14 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(model1,vcov.=vcovHC) # White-corrected t test for significance
# 
# t test of coefficients:
#   
#   Estimate  Std. Error t value Pr(>|t|)   
# (Intercept) -21.7703086  41.0326944 -0.5306 0.597124   
# bdrms        13.8525219  11.5617901  1.1981 0.234236   
# lotsize       0.0020677   0.0071485  0.2893 0.773101   
# sqrft         0.1227782   0.0407325  3.0143 0.003406 **
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



# model2
model2 <- lm(log(price)~bdrms+log(lotsize)+log(sqrft), data=context1)

summary(model2)

# 
# Call:
#   lm(formula = log(price) ~ bdrms + log(lotsize) + log(sqrft), 
#      data = context1)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.68422 -0.09178 -0.01584  0.11213  0.66899 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -1.29704    0.65128  -1.992   0.0497 *  
#   bdrms         0.03696    0.02753   1.342   0.1831    
# log(lotsize)  0.16797    0.03828   4.388 3.31e-05 ***
#   log(sqrft)    0.70023    0.09287   7.540 5.01e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.1846 on 84 degrees of freedom
# Multiple R-squared:  0.643,	Adjusted R-squared:  0.6302 
# F-statistic: 50.42 on 3 and 84 DF,  p-value: < 2.2e-16

coeftest(model2,vcov. = vcov) # OLS

# 
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.651284 -1.9915   0.04967 *  
#   bdrms         0.036958   0.027531  1.3424   0.18308    
# log(lotsize)  0.167967   0.038281  4.3877 3.307e-05 ***
#   log(sqrft)    0.700232   0.092865  7.5403 5.006e-11 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


coeftest(model2, vcov. = vcovHC) # White-corrected
# 
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)  -1.297042   0.850457 -1.5251  0.130988    
# bdrms         0.036958   0.035576  1.0389  0.301845    
# log(lotsize)  0.167967   0.053275  3.1528  0.002243 ** 
#   log(sqrft)    0.700232   0.121392  5.7683 1.298e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# 
# Interpretations
# a. Identify which variables are significant using the OLS test for model1.
#   lotsize (1%) and sqrft (0.1%) are significant using OLS test for model1
# b. Which variables are still significant after using the White-corrected significance test for model1?
#   Only sqrft (1%) is significant after White-corrected significance test for model1
# c. Identify which variables are significant using the OLS test for model2.
#   log(lotsize) and log(sqrft)  (both at 0.1%) are significant using OLS test for model2
# d. Which variables are still significant after using the White-corrected significance test for model2?
#   log(lotsize) (1%) and log(sqrft) (0.1%) are significant using White-corrected test for model2
# e. Keeping these results in mind, what is the effect of taking logs on heteroskedasticity in the data?
#   Heteroskedasticity exist when the variance of data is not equal. Applying a log transformation causes the data 
#   to be normally distributed. In other words, the log transform causes the variance to be equal and 
#   thus eliminate heteroskedasticity.

#read beverridge.csv into context2
context2 <- fread("beveridge.csv")
#model3
model3 <- lm(urate~vrate, data = context2)
summary(model3)
# 
# Call:
#   lm(formula = urate ~ vrate, data = context2)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.1399 -0.9063 -0.1726  0.7893  2.9342 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  17.1194     0.5920   28.92   <2e-16 ***
#   vrate        -3.7414     0.2068  -18.09   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.202 on 133 degrees of freedom
# Multiple R-squared:  0.7111,	Adjusted R-squared:  0.7089 
# F-statistic: 327.3 on 1 and 133 DF,  p-value: < 2.2e-16

coeftest(model3, vcov. = vcov) #OLS
# 
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    0.59200  28.918 < 2.2e-16 ***
#   vrate       -3.74145    0.20681 -18.091 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(model3, vcov. = NeweyWest(model3,lag = 5)) #NeweyWest
# 
# t test of coefficients:
#   
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept) 17.11942    1.36561  12.536 < 2.2e-16 ***
#   vrate       -3.74145    0.39575  -9.454 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


kpss.test(context2$urate,null="Level") #urate level
kpss.test(context2$urate,null="Trend") #urate trend
kpss.test(context2$vrate, null = "Level") #vrate level
kpss.test(context2$vrate, null = "Trend") #vrate trend

kpss.test(diff(context2$urate),null="Level") #diff(urate) level - stationary
# KPSS Test for Level Stationarity
# 
# data:  diff(context2$urate)
# KPSS Level = 0.25265, Truncation lag parameter = 2, p-value = 0.1
# 
# Warning message:
#   In kpss.test(diff(context2$urate), null = "Level") :
#   p-value greater than printed p-value

kpss.test(diff(context2$urate),null="Trend") #diff(urate) trend

kpss.test(diff(context2$vrate), null = "Level") #diff(vrate) level - stationary
# KPSS Test for Level Stationarity
# 
# data:  diff(context2$vrate)
# KPSS Level = 0.30923, Truncation lag parameter = 2, p-value = 0.1
# 
# Warning message:
#   In kpss.test(diff(context2$vrate), null = "Level") :
#   p-value greater than printed p-value

kpss.test(diff(context2$vrate), null = "Trend") #diff(vrate) trend


kpss.test(diff(diff(context2$urate)),null="Level") #diff(diff(urate)) level - stationary
kpss.test(diff(diff(context2$urate)),null="Trend") #diff(diff(urate)) trend - stationary
kpss.test(diff(diff(context2$vrate)), null = "Level") #diff(diff(vrate)) level  - stationary
kpss.test(diff(diff(context2$vrate)), null = "Trend") #diff(diff(vrate)) trend  - stationary


#model4

model4 <- lm(diff(urate)~diff(vrate), data=context2)
summary(model4)

# 
# Call:
#   lm(formula = diff(urate) ~ diff(vrate), data = context2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.54257 -0.13705 -0.03429  0.06847  0.66571 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.03705    0.01781   2.080   0.0394 *
#   diff(vrate) -0.02760    0.10732  -0.257   0.7974  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.2058 on 132 degrees of freedom
# Multiple R-squared:  0.0005008,	Adjusted R-squared:  -0.007071 
# F-statistic: 0.06614 on 1 and 132 DF,  p-value: 0.7974

coeftest(model4,vcov. = vcov)

# 
# t test of coefficients:
#   
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.037046   0.017809  2.0802  0.03944 *
#   diff(vrate) -0.027599   0.107318 -0.2572  0.79745  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

coeftest(model4,vcov. = NeweyWest(model4,lag = 5))
# 
# t test of coefficients:
#   
#   Estimate Std. Error t value Pr(>|t|)
# (Intercept)  0.037046   0.030041  1.2332   0.2197
# diff(vrate) -0.027599   0.081122 -0.3402   0.7342

# 
# Interpretations
# f. Do the OLS and NeweyWest significance tests show that the coefficient on the vanancy rate is significant
# or not (before we correct for stationarity)?
#   Yes, the OLS and NeweyWest significance tests show that the coefficient on the vacancy rate is significant (0.1%).
#
# g. Based on the KPSS findings, which transformation/transformations should we apply to the unemployment
# rate before modeling?
#   Based on KPSS findings, we should use first differencing transformation on the unemployment rate before modeling
#
# h. Based on the KPSS findings, which transformation/transformations should we apply to the vacancy rate
# before modeling?
#    Based on KPSS findings, we should use first differencing transformation on the vacancy rate before modeling
#
# i. How have the signifcance tests changed from model3 to model4?
#   In model4 the coefficient on the vacancy rate is not significant. whereas, it was significant in model3

# j. Which model better describes the data?
#   Model 4 better describes the data, this is because applying differences causes the data to be stationary 
#   and also stabalizes the mean of time series.

# read JTRAIN into context3
context3 <- fread("JTRAIN.csv")
context3<- plm.data(context3,index=c("fcode","year"))

# variable d88 is 1 for year = 1988
context3$d88 = ifelse(context3$year == 1988, 1, 0)


# variable d89 is 1 for year = 1988
context3$d89 = ifelse(context3$year == 1989, 1, 0)

# variable grant_t-1 is 0 for year = 1987
context3$grant_t_1<-0

gfcodes<-context3[which(context3$d88==1 & context3$grant==1),"fcode"]

for(i in seq(1:length(gfcodes)))
{
  context3[which(context3$fcode==gfcodes[i] & context3$d89==1),"grant_t_1"]<-1
}


#model 5
model5 <- plm(log(scrap)~d88+d89+grant+grant_t_1,model="pooling",data=context3)
summary(model5)

# 
# Pooling Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_t_1, data = context3, 
#       model = "pooling")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.  1st Qu.   Median  3rd Qu.     Max. 
# -5.20260 -0.89599 -0.08461  1.02417  3.30029 
# 
# Coefficients :
#   Estimate Std. Error t-value Pr(>|t|)   
# (Intercept)  0.597434   0.203063  2.9421 0.003754 **
#   d88         -0.239370   0.310864 -0.7700 0.442447   
# d89         -0.496524   0.337928 -1.4693 0.143748   
# grant        0.200020   0.338285  0.5913 0.555186   
# grant_t_1    0.048936   0.436066  0.1122 0.910792   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    355.75
# Residual Sum of Squares: 349.59
# R-Squared:      0.017311
# Adj. R-Squared: -0.0077257
# F-statistic: 0.691427 on 4 and 157 DF, p-value: 0.59893
coeftest(model5, vcov. = vcov) #OLS
summary(model5, vcov=vcovHC(model5, method = "arellano")) #HAC (Arellano)


#model 6
model6 <- plm(log(scrap)~d88+d89+grant+grant_t_1,model="within",data=context3)
summary(model6)
# 
# Oneway (individual) effect Within Model
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_t_1, data = context3, 
#       model = "within")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.286936 -0.112387 -0.017841  0.144272  1.426674 
# 
# Coefficients :
#   Estimate Std. Error t-value Pr(>|t|)  
# d88       -0.080216   0.109475 -0.7327  0.46537  
# d89       -0.247203   0.133218 -1.8556  0.06634 .
# grant     -0.252315   0.150629 -1.6751  0.09692 .
# grant_t_1 -0.421590   0.210200 -2.0057  0.04749 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    32.25
# Residual Sum of Squares: 25.766
# R-Squared:      0.20105
# Adj. R-Squared: -0.23684
# F-statistic: 6.5426 on 4 and 104 DF, p-value: 9.7741e-05

coeftest(model6, vcov. = vcov) #OLS
# 
# t test of coefficients:
#   
#   Estimate Std. Error t value Pr(>|t|)  
# d88       -0.080216   0.109475 -0.7327  0.46537  
# d89       -0.247203   0.133218 -1.8556  0.06634 .
# grant     -0.252315   0.150629 -1.6751  0.09692 .
# grant_t_1 -0.421590   0.210200 -2.0057  0.04749 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(model6, vcov=vcovHC(model6, method = "arellano")) #HAC (Arellano)
# 
# Oneway (individual) effect Within Model
# 
# Note: Coefficient variance-covariance matrix supplied: vcovHC(model6, method = "arellano")
# 
# Call:
#   plm(formula = log(scrap) ~ d88 + d89 + grant + grant_t_1, data = context3, 
#       model = "within")
# 
# Balanced Panel: n=54, T=3, N=162
# 
# Residuals :
#   Min.   1st Qu.    Median   3rd Qu.      Max. 
# -2.286936 -0.112387 -0.017841  0.144272  1.426674 
# 
# Coefficients :
#   Estimate Std. Error t-value Pr(>|t|)  
# d88       -0.080216   0.095719 -0.8380  0.40393  
# d89       -0.247203   0.192514 -1.2841  0.20197  
# grant     -0.252315   0.140329 -1.7980  0.07507 .
# grant_t_1 -0.421590   0.276335 -1.5256  0.13013  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Total Sum of Squares:    32.25
# Residual Sum of Squares: 25.766
# R-Squared:      0.20105
# Adj. R-Squared: -0.23684
# F-statistic: 7.38691 on 4 and 53 DF, p-value: 8.3412e-05

# 
# Interpretations
# k. Interpret the estimated coefficient on grant in model5.
#   A grant in the current year is associated with a 20.00197 percent increase in scrap rate controlling for d88, d89 and grant t-1
#   for overall population of comapanies
# l. Interpret the estimated coefficient on grant t-1 in model5.
#   A grant given in the last year to the company is associated with 4.89357 percent increase in scrap rate
#   controlling for d88, d89 and grant for overall population of comapanies.
# m. How do you interpret the signs of B3 and B4?
#   Both B3 (grant) and B4 (grant t-1) have positive sign which means that overall population of companies with a grant in current or
#    previous year is associated with higher scrap rate.
# n. Interpret the estimated coefficient on grant in model6.
#   A grant given in current year is associated with a 25.231487 percent decrease in scrap rate controlling for d88, d89 and grant t-1
#   for individual company for the current year
# o. Interpret the estimated coefficient on grant t-1 in model6
#   A grant given in the last year to the company is associated with 42.158951 percent decrease in scrap rate controlling for 
#   d88, d89, grant for individual company for the current year.
# p. How do you interpret the signs of B3 and B4 now?
#   The signs are negative, which means that if a company has grant in the current year or in the previous year
#   then there is an associated decrease in the scrap rate for the current year
# q. How do the significance results change from using the HAC (Arellano) significance results compared to OLS?
#   In OLS, we have 3 significant variables d89, grant and grant t-1
#   In HAC, we have only 1 significant variable that is grant (10% significance)
#   After HAC (removing Heteroskedasticity and Auto Correlation) we conclude that only grant has significant effect on scrap rate.
