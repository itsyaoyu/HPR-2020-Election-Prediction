
# Loading in necessary libraries

library(tidyverse)

# Reading in the data

polls_2020 <- read_csv("raw-data/president_polls.csv")
polls_past <- read_csv("raw-data/pollavg_1968-2016.csv")
polls_past_state <- read_csv("raw-data/pollavg_bystate_1968-2016.csv")
past_elections <- read_csv("raw-data/popvote_1948-2016.csv")
past_elections_state <- read_csv("raw-data/popvote_bystate_1948-2016.csv")
electoral_college <- read_csv("raw-data/ec_1952-2020.csv")