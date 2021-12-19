# load libraries
library(tidyverse)
library(tibble)
library(glmnetUtils)                    # to run ridge and lasso
source("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/code/functions/plot_glmnet.R")            # for lasso/ridge trace plots

# read in the training data and convert response to binary (Dem = 1, Rep = 0)
train_data = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/train_data.csv")
train_data = train_data %>% mutate(leading_party = as.numeric(leading_party == "Democrat"))

# run logistic regression
glm_fit = glm(leading_party ~ . - state - county - fips - total_votes - Democrat - Other - Republican - Green - Libertarian - pct_dem - pct_rep - pct_other - pct_green - pct_libertarian,
              family = "binomial",
              data = train_data)
beta_hat_std = as.data.frame(coef(glm_fit)) %>% rownames_to_column("feature") %>% rename(coefficient = `coef(glm_fit)`)

# save the glm fit object
save(glm_fit, file = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/glm_fit.Rda")

# extract features selected by glm and their coefficients
beta_hat_std[-1,] %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/glm-features-table.tsv")

# run ridge regression
set.seed(2021)
ridge_fit = cv.glmnet(leading_party ~ . - state - county - fips - total_votes - Democrat - Other - Republican - Green - Libertarian - pct_dem - pct_rep - pct_other - pct_green - pct_libertarian,   
                      alpha = 0,                 
                      nfolds = 10,
                      family = "binomial",
                      type.measure = "class",
                      data = train_data)

# save the ridge fit object
save(ridge_fit, file = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/ridge_fit.Rda")

# create ridge CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/ridge-cv-plot.png")
plot(ridge_fit)
dev.off()

# create ridge trace plot
p = plot_glmnet(ridge_fit, train_data, features_to_plot = 6)
ggsave(filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/ridge-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by ridge and their coefficients
beta_hat_std = extract_std_coefs(ridge_fit, train_data)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/ridge-features-table.tsv")

# run lasso regression
set.seed(2021)
lasso_fit = cv.glmnet(leading_party ~ . - state - county - fips - total_votes - Democrat - Other - Republican - Green - Libertarian - pct_dem - pct_rep - pct_other - pct_green - pct_libertarian,   
                      alpha = 1,
                      family = "binomial",
                      type.measure = "class",
                      data = train_data)

# save the lasso fit object
save(lasso_fit, file = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/lasso_fit.Rda")

# create lasso CV plot
png(width = 6, 
    height = 4,
    res = 300,
    units = "in", 
    filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/lasso-cv-plot.png")
plot(lasso_fit)
dev.off()

# create lasso trace plot
p = plot_glmnet(lasso_fit, train_data, features_to_plot = 6)
ggsave(filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/lasso-trace-plot.png", 
       plot = p, 
       device = "png", 
       width = 6, 
       height = 4)

# extract features selected by lasso and their coefficients
beta_hat_std = extract_std_coefs(lasso_fit, train_data)
beta_hat_std %>%
  filter(coefficient != 0) %>%
  arrange(desc(abs(coefficient))) %>% 
  write_tsv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/lasso-features-table.tsv")
