# load libraries
library(lubridate)
library(tidyverse)
library(janitor)

# clean election data
election = election_raw %>%
  na.omit() %>%
  filter(year == 2020) %>%
  filter(mode == "TOTAL") %>%
  select(-candidate, -office, -mode, -version,-year,-state_po) %>%
  pivot_wider(names_from = party, values_from = candidatevotes) %>%
  rename(
    democrat = DEMOCRAT,
    libertarian = LIBERTARIAN,
    republican = REPUBLICAN,
    green = GREEN,
    other = OTHER,
    total_votes = totalvotes
  ) %>%
  mutate(
    pct_dem = democrat / total_votes,
    pct_rep = republican / total_votes,
    pct_other = other / total_votes,
    pct_green = green / total_votes,
    pct_libertarian = libertarian / total_votes
  ) %>%
  rename(fips = county_fips)
partytallies = election %>%
  select(democrat, other, republican, green, libertarian)
election = cbind(election, leading_party = colnames(partytallies)[apply(partytallies, 1, which.max)])

# clean county health rankings data
county_health = county_health_raw %>% row_to_names(row_number = 1)
county_health = county_health[-1, ]
CH_fips = county_health %>% select(fipscode)
county_health = cbind(CH_fips, county_health[, grepl("rawvalue", names(county_health))])
county_health = county_health[, apply(county_health, 2, function(x)
  ! any(is.na(x)))]
county_health = county_health[, order(colnames(county_health))]
county_health = county_health %>%
  select(-v051_rawvalue, -v069_rawvalue) %>%
  rename(
    fips = fipscode,
    poor_fair_health = v002_rawvalue,
    adult_smoking = v009_rawvalue,
    adult_obesity = v011_rawvalue,
    poor_physical_health_days = v036_rawvalue,
    poor_mental_health_days = v042_rawvalue,
    excessive_drinking = v049_rawvalue,
    pct_below_18 = v052_rawvalue,
    pct_above_65 = v053_rawvalue,
    pct_nonhispanic_black = v054_rawvalue,
    pct_native_american = v055_rawvalue,
    pct_hispanic = v056_rawvalue,
    pct_females = v057_rawvalue,
    pct_nonproficient_english = v059_rawvalue,
    diabetes_prevalence = v060_rawvalue,
    physical_inactivity = v070_rawvalue,
    pct_pacific_islander = v080_rawvalue,
    pct_asian = v081_rawvalue,
    pct_nonhispanic_white = v126_rawvalue,
    severe_housing_issues = v136_rawvalue,
    food_insecurity = v139_rawvalue,
    social_associations = v140_rawvalue,
    insufficient_sleep = v143_rawvalue,
    frequent_physical_distress = v144_rawvalue,
    frequent_mental_distress = v145_rawvalue,
    homeownership = v153_rawvalue,
    traffic_volume = v156_rawvalue
  )
county_health = subset(county_health,!(endsWith(fips, '000')))
county_health = sapply(county_health, as.numeric)
county_health[,-1] = scale(county_health[,-1])

# clean COVID data
str(COVID_raw)
COVID = COVID_raw %>% select(
  fips = `FIPS Code`,
  total_deaths = `Total deaths`,
  covid_deaths = `COVID-19 Deaths`,
  urban_rural_desc = `Urban Rural Description`
)
COVID = unique(COVID)

# clean mask data
masks = masks_raw %>% rename(
  fips = COUNTYFP,
  never = NEVER,
  rarely = RARELY,
  sometimes = SOMETIMES,
  frequently = FREQUENTLY,
  always = ALWAYS
)
masks = sapply(masks, as.numeric)
masks[,-1] = scale(masks[,-1])

# clean education data
education = cbind(education_raw[, 1], education_raw[, 44:47])
education = education %>%
  rename(
    fips = `FIPS Code`,
    pct_less_than_high_school = `Percent of adults with less than a high school diploma, 2015-19`,
    pct_high_school_diploma = `Percent of adults with a high school diploma only, 2015-19`,
    pct_college_associates = `Percent of adults completing some college or associate's degree, 2015-19`,
    pct_bachelors_or_higher = `Percent of adults with a bachelor's degree or higher, 2015-19`
  )
education = subset(education,!(endsWith(fips, '000')))
education = sapply(education, as.numeric)
education[,-1] = scale(education[,-1])

# clean population data
population = population_raw[-c(1, 2, 3, 4), ]
population = population %>% mutate_all( ~ gsub("Population ", "", .)) %>%
  select(fips = FIPStxt,
         year = Attribute,
         population = Value) %>%
  filter(year == 2020) %>%
  select(-year)
population = subset(population,!(endsWith(fips, '000')))
population = sapply(population, as.numeric)
population[,-1] = scale(population[,-1])

# clean unemployment data
unemployment = unemployment_raw %>%
  filter(str_detect(
    Attribute,
    "Median_Household_Income_2019|Unemployment_rate_2020"
  ))
unemployment = unemployment[-c(1, 2), ]
unemployment = unemployment %>% pivot_wider(names_from = Attribute,
                                            values_from = Value) %>%
  select(
    fips = FIPS_Code,
    unemployment_rate = Unemployment_rate_2020,
    median_household_income = Median_Household_Income_2019
  )
unemployment = subset(unemployment,!(fips %% 1000 == 0))
unemployment = sapply(unemployment, as.numeric)
unemployment[,-1] = scale(unemployment[,-1])

# clean poverty data
poverty = poverty_raw %>% pivot_wider(names_from = Attribute,
                                      values_from = Value) %>%
  select(fips = FIPStxt, poverty = POVALL_2019)
poverty = subset(poverty,!(fips %% 1000 == 0))
poverty[,-1] = scale(poverty[,-1])

# inner join based on "fips"
election = as.data.frame(election)
county_health = as.data.frame(county_health)
COVID = as.data.frame(COVID)
masks = as.data.frame(masks)
education = as.data.frame(education)
population = as.data.frame(population)
unemployment = as.data.frame(unemployment)
poverty = as.data.frame(poverty)
master_data = inner_join(x = election, y = county_health, by = "fips") %>%
  inner_join(COVID, by = "fips") %>%
  inner_join(masks, by = "fips") %>%
  inner_join(education, by = "fips") %>%
  inner_join(population, by = "fips") %>%
  inner_join(unemployment, by = "fips") %>%
  inner_join(poverty, by = "fips")

# write clean data to file 
write_csv(x = master_data, file = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/master_data.csv")