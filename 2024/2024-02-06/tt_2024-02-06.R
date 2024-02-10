library(tidyverse)
library(ggtext)


heritage <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-02-06/heritage.csv')

glimpse(heritage)

heritage2 <- heritage |> 
  pivot_longer(c(`2004`,`2022`), names_to = "year", values_to = 'n_sites')

heritage2 |> 
ggplot() + 
  geom_col( aes(x = country, y = n_sites, fill = year), position = "dodge2") +
  theme_minimal()
  

