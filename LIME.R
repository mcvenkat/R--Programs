# required packages
# install vip from github repo: devtools::install_github("koalaverse/vip")
install.packages("lime")
install.packages("vip")
install.packages("pdp")
install.packages("caret")
install.packages("h2o")
install.packages("tidyverse")
install.packages("rsample")
install.packages("gridExtra")
library(lime)       # ML local interpretation
library(vip)        # ML global interpretation
library(pdp)        # ML global interpretation
library(ggplot2)    # visualization pkg leveraged by above packages
library(caret)      # ML model building
library(h2o)        # ML model building

# other useful packages
library(tidyverse)  # Use tibble, dplyr
library(rsample)    # Get HR Data via rsample::attrition
library(gridExtra)  # Plot multiple lime plots on one graph
library(magrittr)
# initialize h2o
h2o.init()
##  Connection successful!
## 
## R is connected to the H2O cluster: 
##     H2O cluster uptime:         3 minutes 17 seconds 
##     H2O cluster timezone:       America/New_York 
##     H2O data parsing timezone:  UTC 
##     H2O cluster version:        3.19.0.4208 
##     H2O cluster version age:    4 months and 6 days !!! 
##     H2O cluster name:           H2O_started_from_R_mdancho_zbl980 
##     H2O cluster total nodes:    1 
##     H2O cluster total memory:   3.50 GB 
##     H2O cluster total cores:    4 
##     H2O cluster allowed cores:  4 
##     H2O cluster healthy:        TRUE 
##     H2O Connection ip:          localhost 
##     H2O Connection port:        54321 
##     H2O Connection proxy:       NA 
##     H2O Internal Security:      FALSE 
##     H2O API Extensions:         Algos, AutoML, Core V3, Core V4 
##     R Version:                  R version 3.4.4 (2018-03-15)
h2o.no_progress()

# create data sets
df <- rsample::attrition %>% 
  mutate_if(is.ordered, factor, ordered = FALSE) %>%
  mutate(Attrition = factor(Attrition, levels = c("Yes", "No")))

index <- 1:5
train_obs <- df[-index, ]
local_obs <- df[index, ]

# create h2o objects for modeling
y <- "Attrition"
x <- setdiff(names(train_obs), y)
train_obs.h2o <- as.h2o(train_obs)
local_obs.h2o <- as.h2o(local_obs)

# Create Random Forest model with ranger via caret
fit.caret <- train(
  Attrition ~ ., 
  data       = train_obs, 
  method     = 'ranger',
  trControl  = trainControl(method = "cv", number = 5, classProbs = TRUE),
  tuneLength = 1,
  importance = 'impurity'
)

# create h2o models
h2o_rf  <- h2o.randomForest(x, y, training_frame = train_obs.h2o)
h2o_glm <- h2o.glm(x, y, training_frame = train_obs.h2o, family = "binomial")
h2o_gbm <- h2o.gbm(x, y, training_frame = train_obs.h2o)

# ranger model --> model type not built in to LIME
fit.ranger <- ranger::ranger(
  Attrition ~ ., 
  data        = train_obs, 
  importance  = 'impurity',
  probability = TRUE
)

vip(fit.ranger) + ggtitle("ranger: RF")

# built-in PDP support in H2O
h2o.partialPlot(h2o_rf, data = train_obs.h2o, cols = "MonthlyIncome")

fit.ranger %>%
  pdp::partial(pred.var = "MonthlyIncome", grid.resolution = 25, ice = TRUE) %>%
  autoplot(rug = TRUE, train = train_obs, alpha = 0.1, center = TRUE)

explainer_caret <- lime::lime(train_obs, fit.caret, n_bins = 5)

class(explainer_caret)
## [1] "data_frame_explainer" "explainer"           
## [3] "list"
summary(explainer_caret)
##                      Length Class  Mode     
## model                24     train  list     
## bin_continuous        1     -none- logical  
## n_bins                1     -none- numeric  
## quantile_bins         1     -none- logical  
## use_density           1     -none- logical  
## feature_type         31     -none- character
## bin_cuts             31     -none- list     
## feature_distribution 31     -none- list

explainer_caret <- lime::lime(train_obs, fit.caret, n_bins = 5)

class(explainer_caret)
## [1] "data_frame_explainer" "explainer"           
## [3] "list"
summary(explainer_caret)
##                      Length Class  Mode     
## model                24     train  list     
## bin_continuous        1     -none- logical  
## n_bins                1     -none- numeric  
## quantile_bins         1     -none- logical  
## use_density           1     -none- logical  
## feature_type         31     -none- character
## bin_cuts             31     -none- list     
## feature_distribution 31     -none- list

explanation_caret <- lime::explain(
  x               = local_obs, 
  explainer       = explainer_caret, 
  n_permutations  = 5000,
  dist_fun        = "gower",
  kernel_width    = .75,
  n_features      = 5, 
  feature_select  = "highest_weights",
  labels          = "Yes"
)

tibble::glimpse(explanation_caret)

plot_features(explanation_caret)

plot_explanations(explanation_caret)

# tune LIME algorithm
explanation_caret <- lime::explain(
  x               = local_obs, 
  explainer       = explainer_caret, 
  n_permutations  = 5000,
  dist_fun        = "manhattan",
  kernel_width    = 3,
  n_features      = 3, 
  feature_select  = "lasso_path",
  labels          = "Yes"
)

plot_features(explanation_caret)

#Supported vs Non-support models

explainer_h2o_rf  <- lime(train_obs, h2o_rf, n_bins = 5)
explainer_h2o_glm <- lime(train_obs, h2o_glm, n_bins = 5)
explainer_h2o_gbm <- lime(train_obs, h2o_gbm, n_bins = 5)

explanation_rf  <- lime::explain(local_obs, 
                                 explainer_h2o_rf, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")
explanation_glm <- lime::explain(local_obs, 
                                 explainer_h2o_glm, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")
explanation_gbm <- lime::explain(local_obs, 
                                 explainer_h2o_gbm, 
                                 n_features      = 5, 
                                 labels          = "Yes", 
                                 kernel_width    = .1, 
                                 feature_select  = "highest_weights")

p1 <- plot_features(explanation_rf,  ncol = 1) + ggtitle("rf")
p2 <- plot_features(explanation_glm, ncol = 1) + ggtitle("glm")
p3 <- plot_features(explanation_gbm, ncol = 1) + ggtitle("gbm")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

explainer_ranger <- lime(train, fit.ranger, n_bins = 5)

# get the model class
class(fit.ranger)
## [1] "ranger"
# need to create custom model_type function
model_type.ranger <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  
  return("classification")
}

model_type(fit.ranger)

# need to create custom predict_model function
predict_model.ranger <- function(x, newdata, ...) {
  # Function performs prediction and returns data frame with Response
  pred <- predict(x, newdata)
  return(as.data.frame(pred$predictions))
}

predict_model(fit.ranger, newdata = local_obs)

explainer_ranger   <- lime(train_obs, fit.ranger, n_bins = 5)

explanation_ranger <- lime::explain(local_obs, 
                                    explainer_ranger, 
                                    n_features   = 5, 
                                    n_labels     = 2, 
                                    kernel_width = .1)

plot_features(explanation_ranger, ncol = 2) + ggtitle("ranger")

