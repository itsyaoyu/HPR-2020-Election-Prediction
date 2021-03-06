
# Loading in necessary libraries

library(tidyverse)
library(caret)
library(jtools)
library(huxtable)
library(scales)

# Reading in the data

polls_past <- read_csv("data/pollavg_1968-2016_clean.csv")
polls_2020 <- read_csv("data/president_polls_clean.csv")
past_elections <- read_csv("data/popvote_1948-2016_clean.csv")
job_approval_gallup <- read_csv("data/approval_gallup_1941-2020_clean.csv")

# Setting seed for replicability

set.seed(1347)

# Joining data for model 1968-2016

full_data <- polls_past %>% 
  inner_join(past_elections, by = c("year", "party")) %>% 
  inner_join(job_approval_gallup, by = "year")

# Training models
# 1) pv2p ~ average_poll + party
# 2) pv2p ~ average_poll + last_pv2p + party
# 3) pv2p ~ average_poll + last_pv2p + job_approval + party
# 4) pv2p ~ average_poll + last_pv2p + incumbent_party*job_approval + party
# 5) pv2p ~ average_poll + incumbent_party*job_approval

model_1 <- train(pv2p ~ average_poll + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_2 <- train(pv2p ~ average_poll + last_pv2p + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_3 <- train(pv2p ~ average_poll + last_pv2p + job_approval + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_4 <- train(pv2p ~ average_poll + last_pv2p + incumbent_party*job_approval + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_5 <- train(pv2p ~ average_poll + incumbent_party*job_approval, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

# Table of loocv results

models <- tibble(model = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"))

loocv_results <- rbind(model_1$results, model_2$results, model_3$results, model_4$results, model_5$results)

loocv_results_table <- models %>% 
  cbind(loocv_results) %>% 
  tibble()

# Table Presentation of models

model_outputs <- export_summs(model_1$finalModel, model_2$finalModel, model_3$finalModel, 
                              model_4$finalModel, model_5$finalModel,
             coefs = c("Intercept" = "(Intercept)",
                       "Average Poll" = "average_poll",
                       "Republican" = "partyrepublican",
                       "Last pv2p" = "last_pv2p",
                       "Average Job Approval" = "job_approval",
                       "Incumbent Party" = "incumbent_partyTRUE",
                       "Incumbent Party:Average Job Approval" = "`incumbent_partyTRUE:job_approval`"),
             statistics = c(N = "nobs",
                            R2 = "r.squared",
                            R2.adj = "adj.r.squared",
                            sigma = "sigma"))

# quick_pdf(model_outputs, file = "graphics/national_models.pdf")

# Joining data for 2020 prediction

data_2020 <- polls_2020 %>% 
  mutate(last_pv2p = past_elections %>% filter(year == 2016) %>% pull(pv2p),
         incumbent = case_when(
           party == "republican" ~ TRUE,
           party == "democrat" ~ FALSE
         ),
         incumbent_party = incumbent,
         job_approval = job_approval_gallup %>% filter(year == 2020) %>% pull(job_approval))

# Predicting 2020 using model 5

final_model <- lm(pv2p ~ average_poll + incumbent_party*job_approval, data = full_data)

pred_2020 <- predict.lm(object = final_model, newdata = data_2020, se.fit=TRUE, interval="confidence", level=0.95)

# Simulating 10000 draws to get predictive interval

sim_2020 <- tibble(id = as.numeric(1:20000),
                   candidate = rep(c("Biden", "Trump"), 10000),
                   pred_fit = rep(pred_2020$fit[,1], 10000),
                   pred_se = rep(pred_2020$se.fit, 10000)) %>% 
  mutate(pred_prob = map_dbl(.x = pred_fit, .y = pred_se, ~rnorm(n = 1, mean = .x, sd = .y))) %>% 
  mutate(id = case_when(
    id %% 2 == 1 ~ id,
    id %% 2 == 0 ~ id - 1))

# Plot 10000 draws (OLD: un-normalized)

sim_2020_plot <- sim_2020 %>% 
  ggplot(aes(x = pred_prob, color = fct_relevel(candidate, "Trump", "Biden"), 
             fill = fct_relevel(candidate, "Trump", "Biden"))) +
  geom_density(alpha = 0.2) +
  annotate(geom = 'text', x = pred_2020$fit[1,1], y = 0.2, label = 'Biden') +
  annotate(geom = 'text', x = pred_2020$fit[2,1], y = 0.2, label = 'Trump') +
  theme_classic() +
  labs(
    title = "Two-Party Popular Vote Predictive Interval",
    subtitle = "results are from 10,000 simulations of our model",
    x = "Two-Party Popular Vote",
    y = "Density" ) + 
  scale_x_continuous(breaks = seq(46, 60, by = 2), labels = percent_format(accuracy = 1, scale = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values=c("#619CFF", "#F8766D"), breaks = c("Biden", "Trump")) +
  scale_fill_manual(values=c("#619CFF", "#F8766D"), breaks = c("Biden", "Trump")) +
  theme(legend.position = "none")

# Saving plot as image (uncomment to save)

# png("graphics/sim_2020_plot.png", units="in", width=7, height=5, res=300)
# print(sim_2020_plot)
# dev.off()

# Finding the number of times that Trump has a greater two-paty popular vote

trump_wins <- sim_2020 %>% 
  select(id, candidate, pred_prob) %>% 
  pivot_wider(names_from = "candidate", values_from = "pred_prob") %>% 
  mutate(trump_win = Trump > Biden) %>% 
  summarize(trump_wins = sum(trump_win))

# Normalize simulations plot (NEW)

sim_2020_normalized <- sim_2020 %>%
  pivot_wider(id_cols = "id", names_from = "candidate", values_from = "pred_prob") %>%
  mutate(total = Biden + Trump,
         Biden_pv2p = Biden / total * 100,
         Trump_pv2p = Trump / total * 100) %>%
  select(id, Biden_pv2p, Trump_pv2p) %>%
  pivot_longer(cols = c("Biden_pv2p", "Trump_pv2p"), names_to = "candidate1") %>%
  mutate(pred_prob = value,
         candidate = ifelse(candidate1 == "Biden_pv2p", "Biden", "Trump")) %>%
  select(id, candidate, pred_prob)

# Normalized model mean

sim_2020_normalized_mean <- sim_2020_normalized %>% 
  group_by(candidate) %>% 
  summarize(avg = mean(pred_prob))

sim_2020_normalized_plot <- sim_2020_normalized %>% 
  ggplot(aes(x = pred_prob, color = fct_relevel(candidate, "Trump", "Biden"), 
             fill = fct_relevel(candidate, "Trump", "Biden"))) +
  geom_density(alpha = 0.2) +
  annotate(geom = 'text', x = sim_2020_normalized_mean$avg[1], y = 0.2, label = 'Biden') +
  annotate(geom = 'text', x = sim_2020_normalized_mean$avg[2], y = 0.2, label = 'Trump') +
  theme_classic() +
  labs(
    title = "Two-Party Popular Vote Predictive Interval",
    subtitle = "results are from 10,000 simulations of our model",
    x = "Two-Party Popular Vote",
    y = "" ) + 
  scale_x_continuous(breaks = seq(46, 60, by = 2), labels = percent_format(accuracy = 1, scale = 1)) +
  scale_color_manual(values=c("#619CFF", "#F8766D"), breaks = c("Biden", "Trump")) +
  scale_fill_manual(values=c("#619CFF", "#F8766D"), breaks = c("Biden", "Trump")) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Saving plot as image (uncomment to save)

# png("graphics/sim_2020_normalized_plot.png", units="in", width=7, height=5, res=300)
# print(sim_2020_normalized_plot)
# dev.off()
