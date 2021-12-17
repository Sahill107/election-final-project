# load libraries
library(lubridate)
library(tidyverse)

str(election_raw)
# clean election data
election = election_raw %>% 
  na.omit() %>%
  filter(year == 2020) %>%
  filter(mode == "TOTAL") %>%
  select(-candidate, -office, -mode, -version) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>% 
  mutate(pct_dem = DEMOCRAT/totalvotes, pct_rep = REPUBLICAN/totalvotes, pct_other = OTHER/totalvotes)

# clean
