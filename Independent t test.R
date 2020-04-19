#Structure of data
head(mtcars)

#Label the am(0,1) variable
mtcars$am = factor(mtcars$am, levels = c(0,1), labels = c("Automatic", "Manual"))

#Check if labeling was correct
head(mtcars)

# Make the mtcars columns easily available
attach(mtcars)

# Calculate mean for each category of cars 
aggregate(mpg~am,data=mtcars,FUN = mean)

# Calculate the range for each category of cars
aggregate(mpg~am,data=mtcars,FUN = range)

# Plot box plots for each category to understand data distribution and identify outliers
boxplot(mpg~am,main = "Achieved mileage of Automatic and
manual cars", xlab = "Type of car (atuomatic/manual)",col =
          (c("green","blue")), ylab = "Miles per gallon")

# Perform shapiro wilk normality test for each category
aggregate(mpg~vs,data=mtcars,FUN = function(x)  shapiro.test(x))

# Extract the p values
aggregate(mpg~vs,data=mtcars,FUN = function(x)  shapiro.test(x)$p.value)

# Check if the variance in the two groups is equal
# If you have not installed car package please install it
#Load the car package that contains the levene test
library(car)
leveneTest(mpg~am, center = mean)

#The variance in the two groups is not equal. We will transform the data to remedy this
#Take the log of mpg data to stabilize variance
log.transformed.mpg = log(mpg)

#Perform a t test assuming equal variance
t.test(log.transformed.mpg~am,var.equal = TRUE)
