# Install the party package
install.packages("party") 
library(party) 
library(ggplot2)  

head(diamonds) 
# We will predict the cut of diamonds using the features available in the 
#diamonds dataset. 
ct = ctree(cut ~ ., data = diamonds) 

plot(ct, main="Conditional Inference Tree") 
# Example output 
# Response:  cut  
# Inputs:  carat, color, clarity, depth, table, price, x, y, z  

# Number of observations:  53940  
#  
# 1) table <= 57; criterion = 1, statistic = 10131.878 
#   2) depth <= 63; criterion = 1, statistic = 8377.279 
#     3) table <= 56.4; criterion = 1, statistic = 226.423 
#       4) z <= 2.64; criterion = 1, statistic = 70.393 
#         5) clarity <= VS1; criterion = 0.989, statistic = 10.48 
#           6) color <= E; criterion = 0.997, statistic = 12.829 
#             7)*  weights = 82  
#           6) color > E  

#Table of prediction errors 
table(predict(ct), diamonds$cut) 
#            Fair  Good Very Good Premium Ideal 
# Fair       1388   171        17       0    14 
# Good        102  2912       499      26    27 
# Very Good    54   998      3334     249   355 
# Premium      44   711      5054   11915  1167 
# Ideal        22   114      3178    1601 19988 
# Estimated class probabilities 
probs = predict(ct, newdata = diamonds, type = "prob") 
probs = do.call(rbind, probs) 
head(probs)