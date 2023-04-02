library(tidyverse)
library(ggtext)



babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')


glimpse(babynames)


# Number of unique names
unique_names <- babynames %>% group_by(year) %>% 
  summarise(n_distinct = n_distinct(name))

ggplot() +
   geom_col(data = unique_names, aes(x = year, y = n_distinct))


# Most common starting letter
babynames <- babynames %>% mutate(start_letter = substring(name,1,1))

letter_count <- babynames %>% 
  group_by(year, start_letter) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(prop = n / sum(n))



title <- "Since 1960, New Zealand babies have increasingly been given names beginning with the letter <span style='color:seagreen'>**A**</span>."
subtitle <- "<span style='color:gray50'>Proportion of babies born by the starting letter of their name</span>"


ggplot() +
  
  geom_tile(data = letter_count, aes(y = start_letter, x = year, fill = prop)) +
  theme_minimal(base_size = 12)




ggplot() +
  
  geom_line(data = letter_count, aes(group = start_letter, x = year, y = prop), colour = "gray80") +
  geom_line(data = letter_count %>% filter(start_letter == "A"), aes(group = start_letter, x = year, y = prop), colour = "seagreen", lwd = 1) +
  
  scale_y_continuous(labels = scales::percent ) +
  
  labs(title = title, 
       subtitle = subtitle,
       y= "**Proportion** of babies born (%)",
       x = "**Year**") +
  
  theme_minimal(base_size = 15) +
  
  theme(panel.grid.major  = element_blank(),
        panel.grid.minor =  element_blank(),
        plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        axis.title.y  = element_markdown(),
        axis.title.x = element_markdown(),
        plot.title.position = "plot")


ggsave("2022/2022-03-22/tt_2022-03-22.png", bg = "white")

