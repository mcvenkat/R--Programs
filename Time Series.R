install.packages("forecast")
library("forecast")  

# Read the data 
data = scan('fancy.dat') 
ts_data <- ts(data, frequency = 12, start = c(1987,1)) 
ts_data  
plot.ts(ts_data)

# Fit the ARMA model
fit = auto.arima(ts_data) 
summary(fit) 

# Series: ts_data  
# ARIMA(1,1,1)(0,1,1)[12]                     
#    Coefficients: 
#    ar1     ma1    sma1 
# 0.2401  -0.9013  0.7499 
# s.e.  0.1427   0.0709  0.1790 

#  
# sigma^2 estimated as 15464184:  log likelihood = -693.69 
# AIC = 1395.38   AICc = 1395.98   BIC = 1404.43 

# Training set error measures: 
#                 ME        RMSE      MAE        MPE        MAPE      MASE       ACF1 
# Training set   328.301  3615.374  2171.002  -2.481166  15.97302  0.4905797 -0.02521172
