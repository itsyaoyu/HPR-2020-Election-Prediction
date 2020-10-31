
# Loading in necessary libraries

library(tidyverse)
library(caret)
library(jtools)
library(huxtable)
library(scales)

# Reading in the data

polls_2020 <- read_csv("data/president_polls_state_clean.csv")
polls_past_state <- read_csv("data/pollavg_bystate_1968-2016_clean.csv")
past_elections_state <- read_csv("data/popvote_bystate_1948-2016_clean.csv")
econ <- read_csv("data/econ_clean.csv")
ec <- read_csv("data/ec_2020.csv")

# Setting seed for replicability

set.seed(1347)

# Joining data for model 1988-2016
# Choosing to drop NAs for the few years that are missing data.
# This shouldn't affect the models too much.

full_data <- past_elections_state %>% 
  left_join(polls_past_state, by = c("state", "year", "party")) %>% 
  drop_na() %>% 
  left_join(econ %>% mutate(year = year + 1), by = "year") %>% 
  group_by(state, party) %>% 
  group_nest() %>% 
  mutate(data = map(data, ~unnest(., cols = c())))

# Training models 
# lm(pv2p ~ average_poll + incumbent_party*average_gdp, data = full_data)

models <- full_data %>% 
  mutate(model = map(data, ~train(pv2p ~ average_poll + incumbent_party*average_gdp, 
                                  data = .x, method = "lm", trControl = trainControl(method = "LOOCV")))) %>% 
  select(-data)

model_results <- models %>% 
  mutate(r_squared = map_dbl(model, ~summary(.x)$r.squared)) %>%
  mutate(r_squared_loocv = map_dbl(model, ~.x$results[,3])) %>%
  mutate(rmse = map_dbl(model, ~.x$results[,2]))

# Joining data for 2020 prediction

data_2020 <- polls_2020 %>% 
  left_join(past_elections_state %>% 
              filter(year == 2016) %>% 
              select(state, party, pv2p), 
            by = c("state", "party")) %>% 
  rename(last_pv2p = pv2p) %>% 
  mutate(incumbent = case_when(
           party == "republican" ~ TRUE,
           party == "democrat" ~ FALSE
         ),
         incumbent_party = incumbent,
         average_gdp = econ %>% 
           filter(year == 2019) %>% 
           pull(average_gdp))




# Predicting 2020

final_models <- full_data %>% 
  mutate(model = map(data, ~lm(pv2p ~ average_poll + last_pv2p + incumbent_party*average_gdp + party, 
                               data = .x))) %>% 
  select(-data) 

pred_2020 <- data_2020 %>% 
  mutate(state_temp = state) %>% 
  group_by(state_temp) %>% 
  group_nest() %>% 
  mutate(data = map(data, ~unnest(., cols = c()))) %>% 
  left_join(final_models, by = c("state_temp" = "state")) %>% 
  mutate(pred = map_dbl(.x = model, .y = data, ~predict(object = .x, newdata = as.data.frame(.y))))


