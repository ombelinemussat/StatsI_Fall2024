#Question 1: Political Science

#a) Let's calculate the χ2 test statistic by hand χ2 = X (f0 − fe)2 fe

#f_observed= the row count
f_observed11 <- 14
f_observed12 <- 6
f_observed13 <- 7
f_observed21 <- 7
f_observed22 <- 7
f_observed23 <- 1
#f_expected= (Row Total/Grand Total)/Column Total
f_expected11 <- (27/42)*21
f_expected11
f_expected12 <- (27/42)*13
f_expected12
f_expected13 <- (27/42)*8
f_expected13
f_expected21 <- (15/42)*21
f_expected21
f_expected22 <- (15/42)*13
f_expected22
f_expected23 <- (15/42)*8
f_expected23

#χ2 = the sum of (f_observed-f_expected)^2/f_expected
χ2 <- ((f_observed11-f_expected11)^2)/f_expected11+ ((f_observed12-f_expected12)^2)/f_expected12+ ((f_observed13-f_expected13)^2)/f_expected13+ ((f_observed21-f_expected21)^2)/f_expected21+ ((f_observed22-f_expected22)^2)/f_expected22+ ((f_observed23-f_expected23)^2)/f_expected23
χ2

#Calculate by hand with the values we got:
chi_squared <-((14-13.5)^2)/13.5+ ((6-8.36)^2)/8.36+ ((7-5.14)^2)/5.14+ ((7-7.5)^2)/7.5+ ((7-4.64)^2)/4.64+ ((1-2.86)^2)/2.86
chi_squared

#b) Let's calculate the p_value from the t-statistic we just created
#df = (rows-1)(columns-1)
df <- (2-1)*(3-1)
df

p_value <- pchisq(chi_squared, df=2, lower.tail=FALSE)
p_value

#We can conlcude that if α = 0.1, p_value>α, we fail to reject the null -> 

#c) Let's calculate the standardize residuals. First we need to calculate the adjusted residual for each cell.
#z = (f_observed-f_expected)/sqrt(f_expected)


z_11 <- (f_observed11-f_expected11)/(sqrt(f_expected11*(1-(27/42))*(1-(21/42))))
z_11
z_12 <- (f_observed12-f_expected12)/(sqrt(f_expected12*(1-(27/42))*(1-(13/42))))
z_12
z_13 <- (f_observed13-f_expected13)/(sqrt(f_expected13*(1-(27/42))*(1-(8/42))))
z_13
z_21 <- (f_observed21-f_expected21)/(sqrt(f_expected21*(1-(15/42))*(1-(21/42))))
z_21
z_22 <- (f_observed22-f_expected22)/(sqrt(f_expected22*(1-(15/42))*(1-(13/42))))
z_22
z_23 <- (f_observed23-f_expected23)/(sqrt(f_expected23*(1-(15/42))*(1-(8/42))))
z_23

#d)  How might the standardized residuals help you interpret the results?
#the response is on the pdf and LaTeX file 


#QUESTION 2

#a) State a null and alternative (two-tailed) hypothesis.
#Is there any (linear for now) relationship between the two variables 
# Is there an effect of the reservation policy on the number of new or repaired drinking water facilities in the villages.)

#H0: βpolicy = 0 
#If the coefficient βpolicy in the model equals zero, this would mean that means that the reservation policy has no effect on the number of new or repaired drinking water facilities
#Ha: βpolicy ≠ 0
##If the coefficient βpolicy in the model does not equal zero, this would mean that means that the reservation policy does have an effect on the number of new or repaired drinking water facilities

#b) Run a bivariate regression to test this hypothesis

library(tidyverse)
data <- read_csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")


X <- data$reserved
Y <- data$water

#then we create the model 
regression <- lm(Y~X, data)
#We want to see the results of our regression
summary(regression)



#The coefficient estimate for reservation policy is 9.252. 
#it is a positive coefficient, meaning that reservation policies are associated with more repaired drinking facilities 


