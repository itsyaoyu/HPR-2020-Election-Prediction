
# Loading in necessary libraries

library(tidyverse)

# Reading in the data

polls_2020 <- read_csv("raw-data/president_polls.csv")
polls_past <- read_csv("raw-data/pollavg_1968-2016.csv")
past_elections <- read_csv("raw-data/popvote_1948-2016.csv")