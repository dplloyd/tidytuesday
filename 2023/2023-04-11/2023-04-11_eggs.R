library(tidyverse)
library(ggtext)
library(sysfonts)



eggproduction  <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/egg-production.csv')
cagefreepercentages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-11/cage-free-percentages.csv')


font_add_google("Open Sans","opensans")

eggproduction %>% 
  group_by(observed_month, prod_type) %>% 
  summarise(n_eggs = sum(n_eggs),
            n_hens = sum(n_hens)) %>% 
  ggplot() +
  geom_line(aes(observed_month,n_hens,colour = prod_type) )


eggproduction %>% 
  ggplot() +
  geom_col(aes(observed_month,n_eggs) ) +
  facet_grid(prod_process ~ prod_type)


eggproduction <-   eggproduction %>% 
  group_by(prod_type,prod_process) %>% 
  mutate(index_hens = 100*n_hens / n_hens[observed_month==min(observed_month)]) %>% 
filter(observed_month >= as.Date("2016-08-31") )



eggproduction %>% 
  ggplot() +
  geom_line(aes(observed_month,index_hens )) +
  facet_grid(prod_process ~ prod_type) +
  
  expand_limits(y=0)


eggproduction %>% filter(prod_type == "table eggs") %>% 
  ggplot(aes(observed_month,index_hens, colour = prod_process )) +
  geom_line(linewidth = 1) +
  
  scale_y_continuous(name = "**Index** (100 = 31 Aug 2016)")  +
  scale_x_continuous(name = "Time") +
  theme_minimal() +
  
  labs(title = "The number of cage free (non-organic) hens has \nalmost quadrupled since 2016.",
       caption = "Data: The Humane League's US Egg Production dataset by Samara Mendez.",
       subtitle = "Index of the number table-egg laying hens, by production type.") +
  
  theme(plot.background = element_rect(fill = "#F0EAD6" ),
        panel.background = element_rect(fill = "#F0EAD6" ),
        panel.grid = element_blank(),
        axis.title.y = element_markdown(angle = 90),
        plot.caption.position = "plot") +
  
  scale_fill_manual(values = c("#7C8867","#e4acd0", "#bface4"), aesthetics = "colour",name = "")



ggsave("2023-04-11_eggs.png", height = 5)


