#Let's install the library "car" so we can use the dataset Prestige
install.packages("car")
library(car)
data(Prestige)
help(Prestige)
library(stargazer)
setwd("/Users/ombelinemussat/Documents/GitHub/StatsI_Fall2024/problemSets/PS04/my_answers")
getwd()

#Question 1

#a)Let's create a new variable 'professional'

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

#b) Let's run a regression with prestige as an outcome and income, professional, and the interaction of the two as predictors
regression <- lm(prestige ~ income + professional + income:professional, data = Prestige)
stargazer(regression, type = "latex")

#Question 2

#a) Let's conduce a hypothesis test to determine whether having these yard signs in a precinct affects vote share
#Let's run a t-test since we are testing an individual coefficient and we want to see the partial effect of this predictor 

#Step 1: Assumptions
# Relationship between predictor variables and outcome variable should be approximately linear
#independent covariates
# Errors are normally distributed with constant variance
# Errors are independent from the covariates 


#Step 2: Hypotheses
#let's call β1 the coefficient for precinct assigned lawn signs 
#H0: β1=0, precinct assigned lan signs would have no effect on the vote share for Cuccinelli
#HA :β1≠0, precinct assigned lan signs have some effect on the vote share for Cuccinelli

#Step 3: Test Satistic
#t = β1hat/Standard Error(β1hat)
t_statistics_1 <- 0.042/0.016
t_statistics_1

#determine the degree of freedom:
#df <- n-k-1
df <- 131-2-1
df

#Step 4: Calculate the p-value
p_value_1 <- 2*pt(abs(t_statistics_1) , df, lower.tail = F)
p_value_1

#Step 5: Conclusions

#b) Let's conduce a hypothesis test to determine whether being next to precincts with these yard signs affects vote share
#Let's run a t-test since we are testing an individual coefficient and we want to see the partial effect of this predictor 

#Step 1: Assumptions
# Relationship between predictor variables and outcome variable should be approximately linear
#independent covariates
# Errors are normally distributed with constant variance
# Errors are independent from the covariates 


#Step 2: Hypotheses
#let's call β2 the coefficient for precinct adjacent to lawn signs 
#H0: β2=0, precinct adjacent to lawn signs would have no effect on the vote share for Cuccinelli
#HA :β2≠0, precinct adjacent to lawn signs have some effect on the vote share for Cuccinelli

#Step 3: Test Satistic
#t = β2hat/Standard Error(β2hat)
t_statistics_2 <- 0.042/0.013
t_statistics_2

#determine the degree of freedom:
#df <- n-k-1
df <- 131-2-1
df

#Step 4: Calculate the p-value
p_value_2 <- 2*pt(abs(t_statistics_2) , df, lower.tail = F)
p_value_2

#Step 5: Conclusions




