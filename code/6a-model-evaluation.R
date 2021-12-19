# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
test_data = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/test_data.csv")

# load glm fit object
load("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/glm_fit.Rda")

# load ridge fit object
load("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/ridge_fit.Rda")

# load lasso fit object
load("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/lasso_fit.Rda")

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-covid_test$case_fatality_rate)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = covid_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-covid_test$case_fatality_rate)^2))

# print nice table
tibble(Method = c("Logistic", "Ridge", "Lasso"), `Test RMSE` = c(glm_RMSE, ridge_RMSE, lasso_RMSE)) %>%
  write_tsv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/model-evaluation.tsv")