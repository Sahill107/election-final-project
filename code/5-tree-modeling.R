###Load appropriate packages
library(rpart)         # to train decision trees
library(rpart.plot)    # to plot decision trees
library(randomForest)  # random forests
library(gbm)           # boosting
library(tidyverse)     # tidyverse
library(kableExtra)    # for printing tables
library(cowplot)       # for side-by-side plots

###Data Preparation
##Read in data
train = read_csv('data/clean/train_data.csv')
test = read_csv('data/clean/test_data.csv')
##Convert response to binary (Dem = 1, Rep = 0)
train = train %>% 
  mutate(leading_party = as.numeric(leading_party == "Democrat"))
test = test %>% 
  mutate(leading_party = as.numeric(leading_party == "Democrat"))
##Keep only relevant columns
#container for column nails
cols = colnames(train)
train = train[cols[-c(1:14,41:44,55,62)]]

###Decision Tree
###Random Forest
###XGBoost