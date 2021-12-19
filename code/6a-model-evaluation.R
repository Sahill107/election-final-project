# load libraries
library(glmnetUtils)
library(tidyverse)

# load test data
test_data = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/test_data.csv")
test_data = test_data %>% mutate(leading_party = as.numeric(leading_party == "Democrat"))

# load glm fit object
load("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/glm_fit.Rda")

# load ridge fit object
load("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/ridge_fit.Rda")

# load lasso fit object
load("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/lasso_fit.Rda")

# evaluate glm RMSE
glm_predictions = predict(glm_fit, newdata = test_data, type = "response")
glm_RMSE = sqrt(mean((glm_predictions-test_data$leading_party)^2))

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = test_data, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-test_data$leading_party)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = test_data, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-test_data$leading_party)^2))

# print nice table
tibble(Method = c("Logistic", "Ridge", "Lasso"), `Test RMSE` = c(glm_RMSE, ridge_RMSE, lasso_RMSE)) %>%
  write_tsv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/model-evaluation.tsv")