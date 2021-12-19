###Load appropriate packages
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # for printing tables
library(cowplot)       # for side-by-side plots

###Data preparation
##Read in data
test = read_csv("data/clean/test_data.csv")
#Make binary respones (1=Dem, 0=Rep)
test = test %>% 
  mutate(leading_party = as.numeric(leading_party == "Democrat"))

##Load fit objects
#Decision tree
load("results/dtr.Rda")
#Random forest
load("results/rf_fit.Rda")
#Boosting
load("results/gbm_fit.Rda")

###Test set evaluation and comparison
##Misclassification errors
#decision tree
pred_dtr = predict(optimal_tree, newdata = test)
dtr_mis_err = mean(pred_dtr != test$leading_party)
#random forest
pred_rf = predict(rf_fit3, newdata = test)
rf_mis_err = mean(pred_rf != test$leading_party)
#boosting
pred_gbm = predict(gbm_fit_optimal, 
                   newdata = test,
                   n.trees = optimal_num_trees,
                   type = "response")
gbm_mis_err = mean(as.numeric(pred_gbm > 0.5) != test$leading_party)
#table
misclass_errs = tibble(dtr = dtr_mis_err,rf = rf_mis_err, gbm = gbm_mis_err)
#According to misclassification error, the random forest model performs the best