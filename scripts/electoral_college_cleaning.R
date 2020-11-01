
# Loading in necessary libraries

library(tidyverse)

# Loading in raw data

polls_past_state <- read_csv("raw-data/pollavg_bystate_1968-2016.csv")
polls_2020 <- read_csv("raw-data/president_polls.csv")
past_elections <- read_csv("raw-data/popvote_1948-2016.csv")
past_elections_state <- read_csv("raw-data/popvote_bystate_1948-2016.csv")
ec <- read_csv("raw-data/ec_1952-2020.csv")

# Cleaning for Past State Polls:
# 1) Using only polls after the convention
# 2) Removing split states
# 3) Averaging for all candidates per year (by party and state)
# 4) Removing years where there were not many state polls

polls_past_state_clean <- polls_past_state %>% 
  filter(before_convention == FALSE,
         !state %in% c("ME-1", "ME-2", "NE-1", "NE-2", "NE-3")) %>% 
  group_by(year, state, party) %>% 
  summarize(average_poll = mean(avg_poll), .groups = "drop") %>% 
  filter(year >= 1988)

# write_csv(polls_past_state_clean, "data/pollavg_bystate_1968-2016_clean.csv")

# Cleaning for 2020 Polls:
# 1) Removing national polls and split states
# 2) Cleaning up dates
# 3) Using only polls after the convention (After September)
# 4) Averaging for all candidates (by party and state)
# 5) Selecting the democrat and republican parties and renaming them

polls_2020_state_clean <- polls_2020 %>% 
  filter(!is.na(state),
         !state %in% c("Maine CD-1", "Maine CD-2", "Nebraska CD-1", "Nebraska CD-2")) %>% 
  mutate(start_date = as.Date(end_date, "%m/%d/%y")) %>% 
  filter(start_date >= "2020-09-01") %>% 
  group_by(candidate_party, state) %>% 
  summarize(average_poll = mean(pct), .groups = "drop") %>% 
  filter(candidate_party %in% c("DEM", "REP")) %>% 
  mutate(candidate_party = case_when(
    candidate_party == "DEM" ~ "democrat",
    candidate_party == "REP" ~ "republican"
  )) %>% 
  rename(party = candidate_party)

# write_csv(polls_2020_state_clean, "data/president_polls_state_clean.csv")

# Cleaning for past elections:
# 1) Pivoting R_pv2p and D_pv2p and fixing party names
# 2) Add in previous year's pv2p for each party and state using lag. Removing NAs
# 3) Joining in national data to get needed predictors
# 4) Filtering for year >= 1988 b/c of limited state polls before then

past_elections_state_clean <- past_elections_state %>% 
  select(-c(total, D, R)) %>% 
  pivot_longer(R_pv2p:D_pv2p, names_to = "party", values_to = "pv2p") %>% 
  mutate(party = case_when(
    party == "D_pv2p" ~ "democrat",
    party == "R_pv2p" ~ "republican"
  )) %>% 
  group_by(state, party) %>% 
  mutate(last_pv2p = lag(pv2p, order_by = year)) %>% 
  ungroup() %>% 
  drop_na(last_pv2p) %>% 
  left_join(past_elections %>% 
              select(year, party, winner, incumbent, incumbent_party), by = c("year", "party")) %>% 
  filter(year >= 1988)

# write_csv(past_elections_state_clean, "data/popvote_bystate_1948-2016_clean.csv")  

# Presidential Job Approval Data Cleaning is located in the national_model_data_cleaning.R file
