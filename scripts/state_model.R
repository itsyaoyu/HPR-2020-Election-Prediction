
# Loading in necessary libraries

library(tidyverse)
library(caret)
library(jtools)
library(huxtable)
library(scales)
library(statebins)

# Reading in the data

polls_2020 <- read_csv("data/president_polls_state_clean.csv")
polls_past_state <- read_csv("data/pollavg_bystate_1968-2016_clean.csv")
past_elections_state <- read_csv("data/popvote_bystate_1948-2016_clean.csv")
ec <- read_csv("data/ec_2020.csv")
econ <- read_csv("data/econ_clean.csv")
job_approval_gallup <- read_csv("data/approval_gallup_1941-2020_clean.csv")

# Setting seed for replicability

set.seed(1347)

# Joining data for model 1988-2016
# Choosing to drop NAs for the few years that are missing data.
# This shouldn't affect the models too much.

full_data <- past_elections_state %>% 
  left_join(polls_past_state, by = c("state", "year", "party")) %>% 
  drop_na() %>% 
  left_join(econ %>% mutate(year = year + 1), by = "year") %>% 
  left_join(job_approval_gallup, by = "year") %>% 
  group_by(state) %>% 
  group_nest() %>% 
  mutate(data = map(data, ~unnest(., cols = c())))

# Training models 
# lm(pv2p ~ average_poll + incumbent_party*average_gdp, data = full_data)

#models <- full_data %>% 
#  mutate(model = map(data, ~train(pv2p ~ average_poll + incumbent_party*job_approval + party, 
#                                  data = .x, method = "lm", trControl = trainControl(method = "LOOCV")))) %>% 
#  select(-data)

#model_results <- models %>% 
#  mutate(r_squared = map_dbl(model, ~summary(.x)$r.squared)) %>%
#  mutate(r_squared_loocv = map_dbl(model, ~.x$results[,3])) %>%
#  mutate(rmse = map_dbl(model, ~.x$results[,2]))

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
           pull(average_gdp),
         year = 2020) %>% 
  left_join(job_approval_gallup, by = "year")




# Predicting 2020

final_models <- full_data %>% 
  mutate(model = map(data, ~lm(pv2p ~ average_poll + incumbent_party*job_approval, data = .x))) %>% 
  select(-data) 

pred_2020 <- data_2020 %>%
  mutate(party_temp = party) %>% 
  group_by(state, party_temp) %>% 
  nest() %>% 
  mutate(data = map(data, ~unnest(., cols = c()))) %>% 
  left_join(final_models, by = "state") %>% 
  mutate(pred = map_dbl(.x = model, .y = data, ~predict(object = .x, newdata = as.data.frame(.y)))) %>% 
  select(state, party_temp, pred)

# Scaling the predictions and calculating winner for each state

pred_2020_scaled <- pred_2020 %>% 
  pivot_wider(names_from = party_temp, values_from = pred) %>% 
  mutate(total = democrat + republican) %>% 
  mutate(democrat = (democrat / total) * 100,
         republican = (republican / total) * 100,
         winner = ifelse(republican > democrat, "Trump", "Biden"),
         win_margin = republican - democrat,
         win_margin_group = case_when(
           win_margin >= 5 ~ "Strong Trump",
           win_margin >= 2 ~ "Lean Trump",
           win_margin <= -5 ~ "Strong Biden",
           win_margin <= -2 ~ "Lean Biden",
           TRUE ~ "Toss-Up"
         )) %>% 
  select(state, winner, win_margin, win_margin_group)

pred_2020_scaled_plot <- pred_2020_scaled %>% 
  ggplot(aes(state = state, 
             fill = win_margin_group, 
             name = "Predicted Win Margin")) +
  geom_statebins(border_col = "black", border_size = 1/sqrt(pi)) + 
  theme_statebins() +
  scale_fill_manual(values = c("#619CFF", "#C3D7F7", "#BABABA", "#FACECA", "#F8766D"),
                    breaks = c("Strong Biden", "Lean Biden", "Toss-Up", "Lean Trump", "Strong Trump")) +
#  scale_fill_manual(values = c("#0145ED", "#4CB2F9", "#a396c0", "#FA7988", "#EB001C"),
#                    breaks = c("Strong Biden", "Lean Biden", "Toss-Up", "Lean Trump", "Strong Trump")) +
  labs(title = "2020 Presidential Election Prediction Map",
       fill = "")

# Saving plot as image (uncomment to save)

# png("graphics/pred_2020_scaled_plot.png", units="in", width=7, height=5, res=300)
# print(pred_2020_scaled_plot)
# dev.off()

# Creating the electoral college bar

ec_plot_data <- pred_2020_scaled %>% 
  mutate(state = ifelse(state == "District of Columbia", "D.C.", state)) %>% 
  left_join(ec, by = "state") %>% 
  group_by(win_margin_group) %>% 
  summarize(total = sum(electors), .groups = "drop")

# Plot of the ec numbers (bar)

ec_plot <- ec_plot_data %>% 
  ggplot(aes(x = "2020", y = total, fill = fct_relevel(win_margin_group, "Strong Trump", "Lean Trump", "Toss-Up", "Lean Biden", "Strong Biden"), label = total)) +
  geom_col(show.legend = FALSE, width = 0.25) + 
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_flip() + 
  theme_void() + 
  labs(fill = "") +
  scale_fill_manual(values = c("#619CFF", "#C3D7F7", "#BABABA", "#FACECA", "#F8766D"),
                    breaks = c("Strong Biden", "Lean Biden", "Toss-Up", "Lean Trump", "Strong Trump"))

# Saving plot as image

# png("graphics/pred_2020_ec_bar.png", units="in", width=6, height=1.5, res=100)
# print(ec_plot)
# dev.off()
