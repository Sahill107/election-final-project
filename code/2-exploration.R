
# load libraries
library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(flextable)                      # creating contingency tables
library(corrplot)                       # creating correlation matrices
library(tidyverse)
library(tidyquant)

# read in the cleaned data
master_data = read_csv(
  "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/data/clean/master_data.csv"
)

# create plot of # of counties for each party
p = master_data %>%
  ggplot(aes(x = leading_party, fill = leading_party)) +
  stat_count(width = 0.5) +
  labs(x = "Political Party",
       y = "Number of Counties") +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) +
  theme_bw() +
  theme(legend.position = "none")

# save the plot
ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/reponse-plot.png",
  plot = p,
  device = "png",
  width = 5,
  height = 3
)

# create plot of # of total individual votes for each party
sum_votes = cbind(as.data.frame(c(
  sum(master_data$Democrat),
  sum(master_data$Republican),
  sum(master_data$Other, na.rm = TRUE)
)),
c("Democrat", "Republican", "Other"))
colnames(sum_votes) = c("Counts", "Party")
p = sum_votes %>%
  ggplot(aes(x = Party, y = Counts, fill = Party)) +
  labs(x = "Political Party",
       y = "Number of Votes") +
  geom_bar(stat = "identity") +
  scale_x_discrete(limits = c("Democrat", "Republican", "Other")) +
  scale_y_continuous(label = comma) +
  scale_fill_manual(
    breaks = c("Democrat", "Republican", "Other"),
    values = c("blue", "red", "grey")
  ) +
  theme_bw() +
  theme(legend.position = "none")

# save the plot
ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/indiv-vote-plot.png",
  plot = p,
  device = "png",
  width = 5,
  height = 3
)

# examine top 10 democratic, republican, and other counties as well as civically engaged counties
master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_dem)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/top-10-dems-data.tsv"
  )

master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_rep)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/top-10-reps-data.tsv"
  )

master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_other)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/top-10-other-data.tsv"
  )

master_data %>%
  select(county,
         state,
         pct_dem,
         pct_rep,
         pct_other,
         pct_voters,
         total_votes) %>%
  arrange(desc(pct_voters)) %>%
  rename(
    "County" = county,
    "State" = state,
    "% Democrat" = pct_dem,
    "% Republican" = pct_rep,
    "% Other" = pct_other,
    "% Voted of Population" = pct_voters,
    "Total Votes" = total_votes
  ) %>%
  head(10) %>%
  write_tsv(
    "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/top-10-voters-data.tsv"
  )

# create a map of election parties across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    election %>%
      rename(
        region = state,
        subregion = county,
        `Political Party` = leading_party
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Political Party`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_manual(breaks = c("Democrat", "Republican"),
                    values = c("blue", "red")) +
  theme_void()

ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/election-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of health ratings across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    county_health %>%
      rename(
        region = state,
        subregion = county,
        `Health Rating` = poor_fair_health
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Health Rating`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void()

ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/health-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of education categories across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    education %>%
      rename(region = state,
             subregion = county) %>%
      mutate(
        region = str_to_lower(region),
        subregion = str_to_lower(subregion),
        pct_greater_than_HSdiploma = pct_college_associates + pct_bachelors_or_higher
      ) %>%
      rename(`% of Population with an Associates Degree or More` = pct_greater_than_HSdiploma),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `% of Population with an Associates Degree or More`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void()

ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/education-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of poverty categories across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    poverty %>%
      rename(
        region = state,
        subregion = county,
        `Log Poverty Rating` = log_poverty_rating
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Log Poverty Rating`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "green", high = "red") +
  theme_void()

ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/poverty-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of unemployment values across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    unemployment %>%
      rename(region = state,
             subregion = county) %>%
      mutate(
        region = str_to_lower(region),
        subregion = str_to_lower(subregion),
        log_unemployment_rate = log10(unemployment_rate)
      ) %>%
      rename(`Log Unemployment Rate` = log_unemployment_rate),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `Log Unemployment Rate`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "red", high = "green") +
  theme_void()

ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/unemployment-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

# create a map of COVID values across the U.S.
p = map_data("county") %>%
  as_tibble() %>%
  left_join(
    COVID %>%
      rename(
        region = state,
        subregion = county,
        `% of Total Deaths that are COVID Deaths` = pct_covid_deaths
      ) %>%
      mutate(region = str_to_lower(region),
             subregion = str_to_lower(subregion)),
    by = c("region", "subregion")
  ) %>%
  ggplot() +
  geom_polygon(
    data = map_data("state"),
    aes(x = long, y = lat, group = group),
    color = "black",
    fill = NA,
    size = 1,
    alpha = .3
  ) +
  geom_polygon(
    aes(
      x = long,
      y = lat,
      group = group,
      fill = `% of Total Deaths that are COVID Deaths`
    ),
    color = "darkblue",
    size = .1
  ) +
  scale_fill_gradient(low = "green", high = "red") +
  theme_void()

ggsave(
  filename = "/Users/sahill/OneDrive - PennO365/STAT 471/election-final-project/results/covid-map.png",
  plot = p,
  device = "png",
  width = 7,
  height = 4
)

###Explore associations (train data only)
##read in data
master_data = read_csv('data/clean/train_data.csv')
##dataset contains mixed data types - create lists of categorical vs. numerical columns
#named vector containing dtypes
dtypes = sapply(colnames(master_data), function(x) class(master_data[[x]]))
x_cats = dtypes[dtypes=='character'] %>%
  names()
x_cats = x_cats[-3]
x_cts = dtypes[dtypes=='numeric'] %>%
  names()
y = master_data$leading_party

##categorical variables
#create 2-way contingency table
desc_cont = proc_freq(master_data,"leading_party","urban_rural_desc")
v2_desc_cont = table(master_data$leading_party, master_data$urban_rural_desc)
#save as image
save_as_image(desc_cont, path = "results/desc-cont.png")

#balloonplot
lpd <- master_data %>%
  # Convert to long format
  as_tibble() %>%
  group_by(leading_party, urban_rural_desc) %>%
  summarise(n())

lpd_sp <- ggplot(lpd, aes(x = leading_party, y = urban_rural_desc)) +
  geom_point(aes(size = `n()`), shape = 21, colour = "black", fill = "cornsilk") +
  scale_size_area(max_size = 20, guide = FALSE) +
  geom_text(aes(label = `n()`),
    vjust = 3.2,
    colour = "grey60",
    size = 4
  ) +
  labs(x = 'Leading Party',
       y = 'Urban/Rural Description',
       title = 'County Type vs. Leading Party')
#save as image
ggsave(
  filename = "results/urb-rural-lp.png",
  plot = lpd_sp,
  device = "png",
  width = 7,
  height = 9
)

#barplot
desc_bar = master_data %>%
  ggplot() +
  aes(x = urban_rural_desc, fill = leading_party) +
  geom_bar(position='fill')
#save as image 
ggsave(
  filename = "results/desc-bar.png",
  plot = desc_bar,
  device = "png",
  width = 9,
  height = 9
)

#chi2 test of independence
test <- chisq.test(master_data$leading_party, master_data$urban_rural_desc)
#save results
test_results = capture.output(print(test))
writeLines(test_results, con = file("results/chi2_output.txt"))

##numerical variables
#drop numerical predictors relating to num. of Dem./Rep. votes
x_cts = x_cts[-1:-12]

#correlation matrix using corrplot
nums = master_data %>% select(x_cts)
corrMatrix = corrplot(cor(nums))
#focus on strongest correlations
correlations = cor(nums) %>%
  apply(1, round, digits=2)

correlations[upper.tri(correlations)] <- NA # erase the upper triangle
diag(correlations) <- NA 

correlations %>%
  as.data.frame() %>%
  rownames_to_column("var") %>%
  flextable::flextable() %>%
  flextable::bg(j = 2:ncol(correlations), 
                bg = function(x){
                  out <- rep("transparent", length(x))
                  out[x < -0.7 | x > 0.7] <- "light blue"
                  out
                })
#save as image


