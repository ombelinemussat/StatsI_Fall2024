#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd('/Users/ombelinemussat/Documents/GitHub/StatsI_Fall2024/problemSets/PS03/my_answers/')
getwd()

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

#Question 1:

#let's run a regression where voteshare is the outcome variable and difflog is the explanatory variable
regression_q1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(regression_q1)

#Let's make a scatterplot of the two variables and add the regression line
png(file="scatter_plot_voteshare_difflog.png")
plot(inc.sub$difflog, inc.sub$voteshare, 
     xlab = "Difference in Campaign Spending (Incumbent - Challenger) (difflog)", 
     ylab = "Incumbent vote share (voteshare)", 
     main = "Incumbent vote share vs. difference in campaign spending", 
     pch = 19, col = "black", cex = 0.5) 

# Add the regression line using abline
abline(regression_q1, col = "red") 
dev.off()

residuals_q1 <- regression_q1$residuals

#Question 2: 

#let's run a regression where presvote is the outcome variable and difflog is the explanatory variable
regression_q2 <- lm(presvote ~ difflog, data = inc.sub)
summary(regression_q2)

#Let's make a scatterplot of the two variables 
png(file="scatter_plot_presvote_difflog.png")
plot(inc.sub$difflog, inc.sub$presvote, 
     xlab = "Difference in Campaign Spending (Incumbent - Challenger) (difflog)", 
     ylab = "Vote share of the presidential candidate of the incumbent party (presvote)", 
     main = "Vote Share of Incumbent Party Candidate vs. Campaign Spending Difference ", 
     cex.main = 0.95, #we want a smaller size for the title so it fits on the graph
     pch = 19, col = "black", cex = 0.5) 

#Add the regression line using abline
abline(regression_q2, col = "red") 
dev.off()

residuals_q2 <- regression_q2$residuals

#Question 3

#let's run a regression where voteshare is the outcome variable and presvote is the explanatory variable
regression_q3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(regression_q3)

#Let's make a scatterplot of the two variables and add the regression line
png(file="scatter_plot_presvote_voteshare.png")
plot(inc.sub$presvote, inc.sub$voteshare, 
     xlab = "Vote share of the presidential candidate of the incumbent party (presvote)", 
     ylab = "Incumbent vote share (voteshare)", 
     main = " Vote share of the presidential candidate of the incumbenr party 
     vs the vote share from the incumbent's party", 
     cex.main = 0.95,
     pch = 19, col = "black", cex = 0.5)

# Add the regression line using abline
abline(regression_q3, col = "red") 
dev.off()

#Question 4:

regression_q4 <- lm(residuals_q1 ~ residuals_q2)
summary(regression_q4)

#Let's make a scatterplot of the two variables and add the regression line
png(file="scatter_plot_residuals_q2_residuals_q1.png")
plot(residuals_q2, residuals_q1, 
     xlab = "Unexplained Variation in presvote (after accounting for difflog)", 
     ylab = "Unexplained Variation in voteshare (after accounting for difflog)", 
     main = "Relationship between Residuals of presvote and voteshare", 
     pch = 19, col = "black", cex = 0.5) 

# Add the regression line using abline
abline(regression_q4, col = "red") 
dev.off()

#Question 5

regression_q5 <- lm(voteshare ~ difflog + presvote, data= inc.sub)
summary(regression_q5)

