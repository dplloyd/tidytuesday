library(tidyverse)

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

glimpse(big_tech_companies)

glimpse(big_tech_stock_prices)

crash_period <- tibble(start = as.Date("2020-02-20"), end =  as.Date("2020-04-07"))


dfplot <- big_tech_stock_prices %>% 
  #filter(date >= as.Date("2018-01-01")) %>% 
  group_by(stock_symbol) %>% 
  mutate(index_adjclose = 100*(adj_close/adj_close[date==crash_period$start] ) )

ggplot() +
  geom_line(
    data = dfplot,
    aes(date, adj_close, group = stock_symbol),
    colour = "gray80",
    alpha = 0.5
  ) +
  geom_line(
    data = dfplot %>% filter(stock_symbol == "NFLX"),
    aes(date, adj_close, group = stock_symbol),
    colour = "#E50914"
  )+ 
  geom_segment(data = crash_period, aes(
    x = start,
    xend = start,
    y = 0,
    yend = 600
  ),colour = "black",
  linetype = "dashed",
  linewidth = 0.5) +
  
 labs( title = "Netflix's adjusted closing price soared after the 2020 stock market crash compared to other major tech companies",
       subtitle = 'Adjusted stock closing stock price, <i> 2020 - 2021 </i>',
       caption = 'Data: Yahoo Finance (via Kaggle)',
       x = '**Date**', 
       y = '**Adjusted Closing Price** (USD)') +

theme_bw(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.title = ggtext::element_textbox_simple(
      color = "white", fill = "#E50914", 
      padding = margin(8, 4, 8, 4), margin = margin(b = 5), lineheight= .9
    ),
    plot.caption = ggtext::element_markdown(),
    plot.subtitle = ggtext::element_markdown(),
    axis.title.x = ggtext::element_markdown(),
    axis.title.y = ggtext::element_markdown()
  ) +
  annotate("text", label = "Onset of 2020 market crash", x = crash_period$start-800, y = 600,)

 ggsave("2023/2023-02-07/tt_2023_02-07.png",bg = "white")


