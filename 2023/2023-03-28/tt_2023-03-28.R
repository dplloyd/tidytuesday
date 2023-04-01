library(tidyverse)
library(maps)
library(sf)
library(rnaturalearth)

# Read data
transitions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/transitions.csv')
timezones <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezones.csv')
timezone_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/timezone_countries.csv')
countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-28/countries.csv')


glimpse(transitions)
glimpse(timezones)
glimpse(timezone_countries)
glimpse(countries)

## Get map things set up
timezones <- timezones %>% 
  separate(zone, into = c("place1","place2"))

world_map <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_set_crs(4326)
glimpse(world)


timezones <- st_as_sf(x = timezones, coords = c("longitude","latitude")) %>% 
  st_set_crs(4326)


## Count the number of unique timezones per country
timezone_countries <- left_join(timezone_countries, countries)

plot_title <- "Locations of timezones stored in the IANA database"

p <- ggplot() +
  
  geom_sf(data = world_map, fill = "#8EDEC5") +
  
  geom_sf(data = timezones, aes(colour = place1)) +
  
  scale_colour_discrete(name = "Location") +
  
  labs(title = plot_title ) +
  
  theme_minimal(base_size = 10) +
  theme(
    panel.grid = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    plot.background = element_rect(fill = "#FEFEFE", colour = "#FEFEFE"),
    panel.background = element_rect(fill = "#FEFEFE", colour = "#FEFEFE")
    
    
  ) 
p



  

