
# Loading in necessary libraries

library(tidyverse)
library(caret)
library(jtools)
library(huxtable)

# Reading in the data

polls_past <- read_csv("data/pollavg_1968-2016_clean.csv")
polls_2020 <- read_csv("data/president_polls_clean.csv")
past_elections <- read_csv("data/popvote_1948-2016_clean.csv")
econ <- read_csv("data/econ_clean.csv")

# Joining data for model 1968-2016

full_data <- polls_past %>% 
  inner_join(past_elections, by = c("year", "party")) %>% 
  inner_join(econ, by = "year")

# Training models
# 1) pv2p ~ average_poll + party
# 2) pv2p ~ average_poll + last_pv2p + party
# 3) pv2p ~ average_poll + last_pv2p + average_gdp + party
# 4) pv2p ~ average_poll + last_pv2p + incumbent_party*average_gdp + party

model_1 <- train(pv2p ~ average_poll + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_2 <- train(pv2p ~ average_poll + last_pv2p + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_3 <- train(pv2p ~ average_poll + last_pv2p + average_gdp + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_4 <- train(pv2p ~ average_poll + last_pv2p + incumbent_party*average_gdp + party, 
                 data = full_data, method = "lm", trControl = trainControl(method = "LOOCV"))

model_5 <- train(pv2p ~ average_poll + incumbent_party*average_gdp, 
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
                       "Average GDP Growth" = "average_gdp",
                       "Incumbent Party" = "incumbent_partyTRUE",
                       "Incumbent Party:Average GDP Growth" = "`incumbent_partyTRUE:average_gdp`"),
             statistics = c(N = "nobs",
                            R2 = "r.squared",
                            R2.adj = "adj.r.squared",
                            sigma = "sigma"))

# quick_pdf(model_outputs, file = "graphics/national_models.pdf")

# Joining data for 2020 prediction

data_2020 <- polls_2020 %>% 
  mutate(last_pv2p = past_elections %>% filter(year == 2016) %>% pull(last_pv2p),
         incumbent = case_when(
           party == "republican" ~ TRUE,
           party == "democrat" ~ FALSE
         ),
         incumbent_party = incumbent,
         average_gdp = econ %>% filter(year == 2020) %>% pull(average_gdp))

# Predicting 2020 using model 5

final_model <- lm(pv2p ~ average_poll + incumbent_party*average_gdp, data = full_data)

pred_2020 <- predict.lm(object = final_model, newdata = data_2020, se.fit=TRUE, interval="confidence", level=0.95)
