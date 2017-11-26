#################################
# Name: Amit Sawant
# Desc: Problem Set 4
# Date: 11-21-2017
################################

rm(list=ls(all=TRUE))
install.packages("olsrr")
install.packages("evtree")
install.packages("party")
library(olsrr)
library(data.table)
library(lmtest)
library(sandwich)
library(evtree)
library(party)

# read htv.csv in context1
context1    <- fread('htv.csv')
context1$lwage <- log(context1$wage)

#model1
model1 <- lm(lwage~abil+educ+exper, data = context1)
summary(model1)
# AIC and BIC for model1
c(AIC(model1),BIC(model1))


context1$abil2 <- context1$abil^2
context1$educ2 <- context1$educ^2
context1$exper2 <- context1$exper^2
context1$abileduc <- context1$abil*context1$educ
context1$abilexper <- context1$abil*context1$exper
context1$educexper <- context1$educ*context1$exper
# model2
#model2 <- lm(lwage~abil+educ+exper+abil2+educ2+exper2+abileduc+abilexper+expereduc, data=context1)

# AIC & BIC on model2
c(AIC(model2),BIC(model2))


model2 <- lm(lwage~exper+abileduc+educexper,data=context1)
summary(model2)
AIC(model2)
BIC(model2)

#Interpretations
# a.Model2 has lower BIC & AIC value than model1 therefore the variables (abil x educ & educ x exper)  in model2 expalin the wages more accurately.
#
# b.Adding an interaction term to a model drastically changes the interpretation of all of the coefficients.
#   The variable (educ x exper) has effects on the coefficients of variables educ and exper. The wage is 
#   not only depended on Education or Experience but partially on both the variables.
#   Interactions allow us assess the extent to which the association between one predictor and the outcome depends on a second predictor.
#   In other words, the interactive variable here in the model explains that the effect of experience on wage also depends on the years of education.


########################################### Question 2 ###################################################
# read loanapp.csv into context2
context2 <- fread("loanapp.csv")
context2$whiteobrat <- context2$white*context2$obrat
# model 3
model3 <- glm(approve~white,family=binomial(link="logit"),data=context2)

summary(model3)
#summary
# 
# Call:
#   glm(formula = approve ~ white, family = binomial(link = "logit"), 
#       data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1864   0.4384   0.4384   0.4384   0.8314  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   0.8847     0.1253   7.061 1.65e-12 ***
#   white         1.4094     0.1512   9.325  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1480.7  on 1988  degrees of freedom
# Residual deviance: 1401.8  on 1987  degrees of freedom
# AIC: 1405.8
# 
# Number of Fisher Scoring iterations: 5
#white test
coeftest(model3,vcov.=vcovHC)


# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  0.88469    0.12570   7.038  1.95e-12 ***
#   white        1.40942    0.15152   9.302 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# model4
model4<-glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,family=binomial(link="logit"),data=context2)
summary(model4)
#summary
# Call:
#   glm(formula = approve ~ white + hrat + obrat + loanprc + unem + 
#         male + married + dep + sch + cosign + chist + pubrec + mortlat1 + 
#         mortlat2 + vr, family = binomial(link = "logit"), data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.9549   0.2545   0.3458   0.4768   2.0827  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.80171    0.59467   6.393 1.63e-10 ***
#   white        0.93776    0.17290   5.424 5.84e-08 ***
#   hrat         0.01326    0.01288   1.030  0.30313    
# obrat       -0.05303    0.01128  -4.702 2.58e-06 ***
#   loanprc     -1.90495    0.46041  -4.138 3.51e-05 ***
#   unem        -0.06658    0.03281  -2.029  0.04242 *  
#   male        -0.06639    0.20642  -0.322  0.74776    
# married      0.50328    0.17799   2.828  0.00469 ** 
#   dep         -0.09073    0.07333  -1.237  0.21598    
# sch          0.04123    0.17840   0.231  0.81723    
# cosign       0.13206    0.44608   0.296  0.76720    
# chist        1.06658    0.17121   6.230 4.67e-10 ***
#   pubrec      -1.34067    0.21736  -6.168 6.92e-10 ***
#   mortlat1    -0.30988    0.46351  -0.669  0.50378    
# mortlat2    -0.89468    0.56857  -1.574  0.11559    
# vr          -0.34983    0.15372  -2.276  0.02286 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1476  on 1970  degrees of freedom
# Residual deviance: 1201  on 1955  degrees of freedom
# (18 observations deleted due to missingness)
# AIC: 1233
# 
# Number of Fisher Scoring iterations: 5
# white test
coeftest(model4,vcov.=vcovHC)
# 
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  3.801710   0.653953  5.8134 6.120e-09 ***
#   white        0.937764   0.177764  5.2753 1.325e-07 ***
#   hrat         0.013263   0.013924  0.9525 0.3408381    
# obrat       -0.053034   0.012809 -4.1403 3.469e-05 ***
#   loanprc     -1.904951   0.535160 -3.5596 0.0003714 ***
#   unem        -0.066579   0.036124 -1.8430 0.0653225 .  
# male        -0.066385   0.210174 -0.3159 0.7521100    
# married      0.503282   0.186857  2.6934 0.0070728 ** 
#   dep         -0.090734   0.075412 -1.2032 0.2289086    
# sch          0.041229   0.179024  0.2303 0.8178605    
# cosign       0.132059   0.406794  0.3246 0.7454585    
# chist        1.066577   0.173265  6.1558 7.472e-10 ***
#   pubrec      -1.340665   0.233076 -5.7520 8.817e-09 ***
#   mortlat1    -0.309882   0.545600 -0.5680 0.5700580    
# mortlat2    -0.894675   0.608995 -1.4691 0.1418053    
# vr          -0.349828   0.156653 -2.2331 0.0255398 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#model5
model5<-glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep+sch+cosign+chist+pubrec+mortlat1+mortlat2+vr+whiteobrat,family=binomial(link="logit"),data=context2)

summary(model5)

# Call:
#   glm(formula = approve ~ white + hrat + obrat + loanprc + unem + 
#         male + married + dep + sch + cosign + chist + pubrec + mortlat1 + 
#         mortlat2 + vr + (obrat * white), family = binomial(link = "logit"), 
#       data = context2)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.9379   0.2580   0.3472   0.4722   2.1809  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  4.30653    0.83493   5.158  2.5e-07 ***
#   white        0.29688    0.75565   0.393 0.694407    
# hrat         0.01341    0.01295   1.035 0.300625    
# obrat       -0.06660    0.01935  -3.442 0.000578 ***
#   loanprc     -1.90970    0.45916  -4.159  3.2e-05 ***
#   unem        -0.06755    0.03278  -2.061 0.039315 *  
#   male        -0.07190    0.20704  -0.347 0.728372    
# married      0.50354    0.17808   2.828 0.004691 ** 
#   dep         -0.09577    0.07355  -1.302 0.192849    
# sch          0.03489    0.17894   0.195 0.845390    
# cosign       0.15257    0.45004   0.339 0.734606    
# chist        1.06139    0.17160   6.185  6.2e-10 ***
#   pubrec      -1.34427    0.21793  -6.168  6.9e-10 ***
#   mortlat1    -0.33331    0.46282  -0.720 0.471415    
# mortlat2    -0.92086    0.56942  -1.617 0.105838    
# vr          -0.35086    0.15391  -2.280 0.022631 *  
#   white:obrat  0.01815    0.02079   0.873 0.382587    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 1476.0  on 1970  degrees of freedom
# Residual deviance: 1200.2  on 1954  degrees of freedom
# (18 observations deleted due to missingness)
# AIC: 1234.2
# 
# Number of Fisher Scoring iterations: 5

# white test
coeftest(model5,vcov.=vcovHC)

# 
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept)  4.306527   0.901355  4.7778 1.772e-06 ***
#   white        0.296882   0.858772  0.3457 0.7295644    
# hrat         0.013405   0.014158  0.9468 0.3437269    
# obrat       -0.066604   0.020728 -3.2133 0.0013121 ** 
#   loanprc     -1.909701   0.533546 -3.5793 0.0003446 ***
#   unem        -0.067549   0.035988 -1.8770 0.0605206 .  
# male        -0.071904   0.210842 -0.3410 0.7330802    
# married      0.503536   0.187212  2.6897 0.0071526 ** 
#   dep         -0.095772   0.076030 -1.2597 0.2077873    
# sch          0.034893   0.180807  0.1930 0.8469704    
# cosign       0.152567   0.413003  0.3694 0.7118226    
# chist        1.061385   0.174154  6.0945 1.098e-09 ***
#   pubrec      -1.344267   0.235075 -5.7185 1.075e-08 ***
#   mortlat1    -0.333314   0.540401 -0.6168 0.5373739    
# mortlat2    -0.920857   0.610013 -1.5096 0.1311534    
# vr          -0.350862   0.157302 -2.2305 0.0257141 *  
#   white:obrat  0.018149   0.023603  0.7689 0.4419416    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Interpretations
#a. The coefficient of white in model3 indicates that if a person is white, the odds of approval of loan are higher as compared to non-white. 
#
#b. After adding 14 more variables in model4, in model3 the coefficient on white is 1.40942 & in model4 the coefficient is 0.937764. 
#   The variable is still highly significant in model4
#
#c. After adding interaction variable between white and obrat, the coefficient is 0.296882 but now it is not significant in model5
#
#d. The variable (white x obrat) is a interaction variable. It has affected the model greatly because it provides
#   other obligations for a white person and this interaction improves the model significantly so it is an important variable
#   


########################################### Question 3 ###################################################


# read smoke.csv into context3
context3 <- fread("smoke.csv")
context3$age2 <- context3$age^2
context3$lincome <- log(context3$income)

#model6
model6 <- glm(cigs~educ+age+age2+lincome+restaurn, family=poisson(link="log"),data=context3)

summary(model6)

#summary
# 
# Call:
#   glm(formula = cigs ~ educ + age + age2 + lincome + restaurn, 
#       family = poisson(link = "log"), data = context3)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -6.338  -4.229  -3.280   2.223  13.942  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -8.953e-02  1.881e-01  -0.476    0.634    
# educ        -5.952e-02  4.257e-03 -13.981  < 2e-16 ***
#   age          1.140e-01  4.968e-03  22.943  < 2e-16 ***
#   age2        -1.368e-03  5.696e-05 -24.016  < 2e-16 ***
#   lincome      1.047e-01  2.026e-02   5.168 2.36e-07 ***
#   restaurn    -3.613e-01  3.074e-02 -11.754  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 15821  on 806  degrees of freedom
# Residual deviance: 14755  on 801  degrees of freedom
# AIC: 16238
# 
# Number of Fisher Scoring iterations: 6

# white test
coeftest(model6,vcov.=vcovHC)
# # 
# z test of coefficients:
#   
#   Estimate Std. Error z value  Pr(>|z|)    
# (Intercept) -0.0895322  0.7819305 -0.1145  0.908840    
# educ        -0.0595212  0.0194111 -3.0663  0.002167 ** 
#   age          0.1139858  0.0215662  5.2854 1.254e-07 ***
#   age2        -0.0013679  0.0002485 -5.5045 3.701e-08 ***
#   lincome      0.1047168  0.0840807  1.2454  0.212973    
# restaurn    -0.3613089  0.1386491 -2.6059  0.009163 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#interpretions
#a. Every one year of education is associated with a 5.9521175 % decrease in number of cigs controlling for other variables.
#
#b.   When age = 20
#     log (cigs) =  0.1139858 - (2*0.0013679*20) = 0.0592698 = (5.92%)
#     Rate of change for log(cigs) with respect to age when age is 20 is 5.92698 %
#     (Rate of change of cigs increases by 5.92698 % when the person is age 20)
#     When age = 60
#     log (cigs) =  0.1139858 - (2*0.0013679*60) = -0.0501622 = (-5.01%)
#     Rate of change for log(cigs) with respect to age when age is 20 is -5.01622 %
#     (Rate of change of cigs decreases by 5.01622 % when the person is age 60)


########################################### Question 4 ###################################################



# read hdisease.csv into context4
context4 <- fread("hdisease.csv")
#convert Exang to 1 & 0
context4$exang		<-	as.numeric(context4$exang=="yes")

formula <- hdisease~age+cp+trestbps+thalach+exang
#model 7
model7 <-	evtree(formula,data=context4)
plot(model7)
#model8
model8 <- ctree(formula,data=context4)
plot(model8)
context5 <- fread("hdisease-new.csv")
#convert Exang to 1 & 0
context5$exang		<-	as.numeric(context5$exang=="yes")
context5$hdisease_pred <- predict(model8,context5)

#interpretations
#a. Model 8 is overfitting the data (More decisions in the model) whereas Model 7 is underfitting the data.
#
#b. dset variable is a categorical variable, this variable doesn't really explain heart disease and so 
#   adding this variabe to the model will cause error and overfitting. 

