library(tidyverse)

scurvy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-25/scurvy.csv')


glimpse(scurvy)

scurvy_long <- scurvy |> 
pivot_longer(cols = gum_rot_d6:lassitude_d6, values_to = "score", names_to = "ailment")

# Convert scores into ordered factors
# 

scurvy_long$score <- forcats::as_factor(scurvy_long$score)


scurvy_long |> 
  ggplot() +
  geom_bar(aes(ailment, fill = score)) +
  facet_grid(~treatment)
