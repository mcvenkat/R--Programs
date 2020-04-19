library(ElemStatLearn)
head(spam) 

# Split dataset in training and testing 
inx = sample(nrow(spam), round(nrow(spam) * 0.8)) 
train = spam[inx,] 
test = spam[-inx,]  

# Fit regression model 
fit = glm(spam ~ ., data = train, family = binomial()) 
summary(fit)  

# Call: 
#   glm(formula = spam ~ ., family = binomial(), data = train) 
#  

# Deviance Residuals:  
#   Min       1Q   Median       3Q      Max   
# -4.5172  -0.2039   0.0000   0.1111   5.4944
# Coefficients: 
# Estimate Std. Error z value Pr(>|z|)     
# (Intercept) -1.511e+00  1.546e-01  -9.772  < 2e-16 *** 
# A.1         -4.546e-01  2.560e-01  -1.776 0.075720 .   
# A.2         -1.630e-01  7.731e-02  -2.108 0.035043 *   
# A.3          1.487e-01  1.261e-01   1.179 0.238591     
# A.4          2.055e+00  1.467e+00   1.401 0.161153     
# A.5          6.165e-01  1.191e-01   5.177 2.25e-07 *** 
# A.6          7.156e-01  2.768e-01   2.585 0.009747 **  
# A.7          2.606e+00  3.917e-01   6.652 2.88e-11 *** 
# A.8          6.750e-01  2.284e-01   2.955 0.003127 **  
# A.9          1.197e+00  3.362e-01   3.559 0.000373 *** 
# Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1  1  

### Make predictions 
preds = predict(fit, test, type = "response") 
preds = ifelse(preds > 0.5, 1, 0) 
tbl = table(target = test$spam, preds) 
tbl 

#         preds 
# target    0   1 
# email   535  23 
# spam     46 316 
sum(diag(tbl)) / sum(tbl) 
# 0.925