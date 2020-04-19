# Load the library for doing association rules
install.packages("arules") 
library(arules)  

# Data preprocessing 
data("AdultUCI") 
AdultUCI[1:2,]  
AdultUCI[["fnlwgt"]] <- NULL 
AdultUCI[["education-num"]] <- NULL  

AdultUCI[[ "age"]] <- ordered(cut(AdultUCI[[ "age"]], c(15,25,45,65,100)), 
                              labels = c("Young", "Middle-aged", "Senior", "Old")) 
AdultUCI[[ "hours-per-week"]] <- ordered(cut(AdultUCI[[ "hours-per-week"]], 
                                             c(0,25,40,60,168)), labels = c("Part-time", "Full-time", "Over-time", "Workaholic")) 
AdultUCI[[ "capital-gain"]] <- ordered(cut(AdultUCI[[ "capital-gain"]], 
                                           c(-Inf,0,median(AdultUCI[[ "capital-gain"]][AdultUCI[[ "capitalgain"]]>0]),Inf)), 
                                       labels = c("None", "Low", "High")) 
AdultUCI[[ "capital-loss"]] <- ordered(cut(AdultUCI[[ "capital-loss"]], 
                                           c(-Inf,0, median(AdultUCI[[ "capital-loss"]][AdultUCI[[ "capitalloss"]]>0]),Inf)), 
                                       labels = c("none", "low", "high"))
# Convert the data into a transactions format
Adult <- as(AdultUCI, "transactions") 
Adult 
# transactions in sparse format with 
# 48842 transactions (rows) and 
# 115 items (columns)  

summary(Adult)  
# Plot frequent item-sets 
itemFrequencyPlot(Adult, support = 0.1, cex.names = 0.8)  

# generate rules 
min_support = 0.01 
confidence = 0.6 
rules <- apriori(Adult, parameter = list(support = min_support, confidence = confidence))

rules 
inspect(rules[100:110, ]) 
# lhs                             rhs                      support     confidence  lift
# {occupation = Farming-fishing} => {sex = Male}        0.02856148  0.9362416   1.4005486
# {occupation = Farming-fishing} => {race = White}      0.02831579  0.9281879   1.0855456
# {occupation = Farming-fishing} => {native-country     0.02671881  0.8758389   0.9759474
#= United-States} 
