library(tidyverse)

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

glimpse(big_tech_companies)

glimpse(big_tech_stock_prices)


ggplot(big_tech_stock_prices) +
  geom_line(aes(date,adj_close, colour = stock_symbol)) +
  theme_bw()





