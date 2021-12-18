library(tidyverse)

# read in the cleaned data
master_data = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/master_data.csv")

# split into train and test (set seed here if applicable)
# seed set for reproducibility (DO NOT CHANGE)
set.seed(2021) 
n = nrow(master_data)
train_samples = sample(1:n, round(0.8*n))
train_data = master_data[train_samples,]
test_data = master_data[-train_samples,]

# save the train and test data
write_csv(x = train_data, file = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/train_data.csv")
write_csv(x = test_data, file = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/test_data.csv")