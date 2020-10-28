
# Loading in necessary libraries

library(tidyverse)

# Loading in raw data

polls_past <- read_csv("raw-data/pollavg_1968-2016.csv")
polls_2020 <- read_csv("raw-data/president_polls.csv")
past_elections <- read_csv("raw-data/popvote_1948-2016.csv")
econ <- read_csv("raw-data/econ.csv")

# Cleaning for Past Polls:
# 1) Using only polls after the convention
# 2) Averaging for all candidates per year (by party)

polls_clean <- polls_past %>% 
  filter(before_convention == FALSE) %>% 
  group_by(year, party) %>% 
  summarize(average_poll = mean(avg_support), .groups = "drop")

# write_csv(polls_clean, "data/pollavg_1968-2016_clean.csv")

# Cleaning for 2020 Polls:
# 1) Removing state polls
# 2) Cleaning up dates
# 3) Using only polls after the convention (After September)
# 4) Averaging for all candidates (by party)
# 5) Selecting the democrat and republican parties and renaming them

polls_2020_clean <- polls_2020 %>% 
  filter(is.na(state)) %>% 
  mutate(start_date = as.Date(end_date, "%m/%d/%y")) %>% 
  filter(start_date >= "2020-09-01") %>% 
  group_by(candidate_party) %>% 
  summarize(average_poll = mean(pct), .groups = "drop") %>% 
  filter(candidate_party %in% c("DEM", "REP")) %>% 
  mutate(candidate_party = case_when(
    candidate_party == "DEM" ~ "democrat",
    candidate_party == "REP" ~ "republican"
  )) %>% 
  rename(party = candidate_party)

# write_csv(polls_2020_clean, "data/president_polls_clean.csv")

# Cleaning for past elections:
# 1) Add in previous year's pv2p for each party using lag. Removing 1948 data
# 2) Selecting year, party, winner, pv2p, last_pv2p, incumbent, incumbent_party

past_elections_clean <- past_elections %>% 
  group_by(party) %>% 
  mutate(last_pv2p = lag(pv2p, order_by = year)) %>% 
  ungroup() %>% 
  drop_na(last_pv2p) %>% 
  select(year, party, winner, pv2p, last_pv2p, incumbent, incumbent_party)

# write_csv(past_elections_clean, "data/popvote_1948-2016_clean.csv")  

# NOTE: Get Q3 GDP data: https://www.bea.gov/data/gdp/gross-domestic-product

# Cleaning for economic data:
# 1) Selecting year, quarter, GDP_growth_qt
# 2) Dropping NAs
# 3) Filtering for election year and removing Q4 data
# 4) Averaging GDP per year

econ_clean <- econ %>% 
  select(year, quarter, GDP_growth_qt) %>% 
  drop_na() %>% 
  filter(year %% 4 == 0,
         quarter != 4) %>% 
  group_by(year) %>% 
  summarize(average_gdp = mean(GDP_growth_qt), .groups = "drop")
  
# write_csv(econ_clean, "data/econ_clean.csv")  
