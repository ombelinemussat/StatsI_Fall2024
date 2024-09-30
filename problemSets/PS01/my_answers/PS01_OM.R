
# set wd
setwd("/Users/ombelinemussat/Documents/Github/StatsI_Fall2024/problemSets/PS01")
getwd()

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

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

## Our confidence coefficient is .90
sample_mean <- mean(y) # Point estimate
sample_mean
sample_sd <- sd(y) # Sample standard deviation
sample_sd
standard_error <- sample_sd/sqrt(length(y)) # Standard error
standard_error

#using qt since n<30 so we use the t-distribution
n <- length(y) #sample size
df <- n - 1 #degree of freedom
t90 <- qt((1 - 0.90) / 2, df = df, lower.tail = FALSE)
t90
lower_90 <- sample_mean - (t90 * standard_error) 
upper_90 <- sample_mean + (t90 * standard_error) 
confint90 <- c(lower_90, upper_90)
confint90

#Hypothesis test 

#Step 1: Assumptions 

#Random sampling
#Data is continuous
#Population is distributed normally
#The sample is below 30 so we are using a t-test

#Step 2: Hypotheses

#We have the null hypothesis H0: mu = 100
#The alternative hypothesis is H1: mu > 100
#This is a one-sided test (right-tailed) because we want to test if the mean is greater than the average


#Step 3: Calculate a test statistic
mu_0=100
t_statistic <- (sample_mean - mu_0) / (sample_sd / sqrt(n))
t_statistic

#Step 4: Calculate the p-value 
p_value <- pt(t_statistic, df, lower.tail = FALSE)
p_value 
#Step 5: Draw conclusions 

#error probability:
alpha <- 0.05

#The p_value is 0.7215 and alpha is 0.05, so p_value>alpha
#We fail to reject the null hypothesis 
#We cannot conclude that that the average student has an IQ higher than 100

#####################
# Problem 2
#####################


expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

head(expenditure)

# Get summary statistics for entire dataset
summary_data <- summary(expenditure)
summary_data

#Plot the relationships

# Scatterplot between Y and X1
png(file="scatter_plot_Y_X1.png")
plot(expenditure$X1, expenditure$Y,
     xlab = "Per capita personal income",
     ylab = "Per capita expenditure on shelter/housing assistance",
     main = "Relationship between Y and X1",
     col = "blue")
dev.off()

# Scatterplot between Y and X2
png(file="scatter_plot_Y_X2.png")
plot(expenditure$X2, expenditure$Y,
     xlab = "Number of residents financially insecure (per 100,000)",
     ylab = "Per capita expenditure on shelter/housing assistance",
     main = "Relationship between Y and X2",
     col = "green")
dev.off()

# Scatterplot between Y and X3
png(file="scatter_plot_Y_X3.png")
plot(expenditure$X3, expenditure$Y,
     xlab = "Number of people in urban areas (per thousand)",
     ylab = "Per capita expenditure on shelter/housing assistance",
     main = "Relationship between Y and X3",
     col = "red")
dev.off()

# Scatterplot between X1 and X2
png(file="scatter_plot_X1_X2.png")
plot(expenditure$X1, expenditure$X2,
     xlab = "Per capita personal income",
     ylab = "Number of residents financially insecure (per 100,000)",
     main = "Relationship between X1 and X2",
     col = "purple")
dev.off()

# Scatterplot between X1 and X3
png(file="scatter_plot_X1_X3.png")
plot(expenditure$X1, expenditure$X3,
     xlab = "Per capita personal income",
     ylab = "Number of people in urban areas (per thousand)",
     main = "Relationship between X1 and X3",
     col = "orange")
dev.off()

# Scatterplot between X2 and X3
png(file="scatter_plot_X2_X3.png")
plot(expenditure$X2, expenditure$X3,
     xlab = "Number of residents financially insecure (per 100,000)",
     ylab = "Number of people in urban areas (per thousand)",
     main = "Relationship between X2 and X3",
     col = "brown")
dev.off()

##Relationship between Y and Region
# Boxplot to see which region has the highest per capita expenditure on housing assistance
png(file="box_plot.png")
boxplot(expenditure$Y ~ expenditure$Region,
        main = "Expenditure on Housing Assistance by Region",
        xlab = "Region",
        ylab = "Per capita expenditure on shelter/housing Assistance",
        names=c('Northeast','North Central', 'South', 'West'),
        col = c("green", "blue", "red", "purple"))
dev.off()

#Relationship between Y and X1
# Scatterplot between Y and X1
png(file="scatter_plot_Y_X1_n2.png")
plot(expenditure$X1, expenditure$Y,
     xlab = "Per capita income",
     ylab = "Per capita expenditure on shelter/housing assistance",
     main = "Relationship between Y and X1",
     col = "blue")
dev.off()

#Relationship between Y and X1 with the different Regions
# Scatterplot of Y vs X1, with Region in different colours and symbols
png(file="scatter_plot_Y_X1_Region.png")
plot(expenditure$X1, expenditure$Y,
     xlab = "Per capita income",
     ylab = "Per capita expenditure on shelter/housing assistance",
     main = "Relationship between Y and X1 by Region",
     col = expenditure$Region,    # Display Regions in different colours
     pch = expenditure$Region)   # Display Regions in different symbols
#We need to add a legend
legend("topright",
       legend = c("Northeast", "North Central", "South", "West"),  # Region labels
       col = 1:4, #As we did not specify in the graph which colour to use, it will be the 1st to the 4th
       pch = 1:4) #As we did not specify in the graph which symbol to use, it will be the 1st to the 4th
dev.off()


