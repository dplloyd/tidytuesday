library(tidytuesdayR)
library(tidyverse)
library(visdat)

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load(2021, week = 52)

starbucks <- tuesdata$starbucks

# Getting a sense of the data
glimpse(starbucks)
vis_dat(starbucks)
anyNA(starbucks)

starbucks <- starbucks %>%  mutate(caffeine_mg_per_ml = caffeine_mg / serv_size_m_l)

starbucks %>% 
  select(size,serv_size_m_l, caffeine_mg,calories, product_name) %>% 
  ggplot() +
  geom_point(aes(y=calories, x = caffeine_mg, colour = size) )



