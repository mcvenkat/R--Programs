install.packages("alr3")
library(alr3)

install.packages("ISLR")
library(ISLR)
data("Carseats")
str(Carseats)

sales.fit<-lm(Sales ~ Advertising + ShelveLoc,data=Carseats)
summary(sales.fit)
contrasts(Carseats$ShelveLoc)

