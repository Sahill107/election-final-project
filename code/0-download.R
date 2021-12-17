# load libraries
library(tidyverse)

# download presidential election data (source: MIT Election Lab)
election_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/countypres_2000-2020.csv")

# download education, population, unemployment/income, and poverty data (source: USDA Economic Research Service)
education_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/Education.csv")
population_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/PopulationEstimates.csv")
unemployment_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/Unemployment.csv")
poverty_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/PovertyEstimates.csv")

# download COVID and mask-use data (source: NYT)
COVID_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/us-covid-counties-2020.csv")
masks_raw = read_csv("/Users/sahill/OneDrive - PennO365/STAT 471/stat-471-final-project/data/raw/mask-use-by-county.csv")
