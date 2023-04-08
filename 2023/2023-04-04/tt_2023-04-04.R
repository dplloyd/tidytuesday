library(tidyverse)
library(reactable)
library(reactablefmtr)
library(ggsankey)

football <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-04-04/soccer21-22.csv')

glimpse(football)

# Will create a summary table using reactable.


football_long <- football %>% pivot_longer(cols = c("AwayTeam","HomeTeam"), names_to = "home_away", values_to = "team") %>% 
  mutate(goals_fulltime = case_when(home_away == "AwayTeam" ~ FTAG,
                                     home_away == "HomeTeam" ~ FTHG),
         goals_against_fulltime = case_when(home_away == "AwayTeam" ~ FTHG,
                                            home_away == "HomeTeam" ~ FTAG),
         WLD = case_when(home_away == "AwayTeam" & FTHG > FTAG ~ "Loss",
                         home_away == "AwayTeam" & FTHG < FTAG ~ "Win",
                         home_away == "HomeTeam" & FTHG < FTAG ~ "Loss",
                         home_away == "HomeTeam" & FTHG > FTAG ~ "Win",
                         FTHG == FTAG ~ "Draw"))

goals <- football_long %>% 
  group_by(team) %>% 
  summarise(goals = sum(goals_fulltime),
            goals_against = sum(goals_against_fulltime)) %>% 
  mutate(goal_difference = goals - goals_against
         )

WLD_count <- football_long %>% 
  group_by(team, WLD) %>% 
   count() %>% 
  pivot_wider(names_from = WLD, values_from = n)

points_earned <- football_long %>% 
  group_by(team, WLD) %>% 
  summarise(WLD_count = n()) %>% 
  mutate(total_points = case_when(WLD == "Win" ~ WLD_count * 3,
                                  WLD == "Loss" ~ 0,
                                  WLD == "Draw" ~ WLD_count)) %>% 
  group_by(team) %>% 
  summarise(total_points = sum(total_points))


# Join together



# Sparkline data
WLD_ts <- football_long %>% 
  mutate(WLD_num  = case_when(WLD=="Win" ~ 1,
                              WLD=="Loss" ~ -1,
                              WLD=="Draw"~ 0)) %>% 
  group_by(team) %>% 
  summarise(WLD_time  = list(tail(WLD_num)))
  
epl <- left_join(goals,WLD_count) %>% 
  left_join(points_earned) %>% 
  left_join(WLD_ts) %>% 
  select(team, Win, Draw, Loss, goals, goals_against, goal_difference, total_points,WLD_time) %>% 
  arrange(desc(total_points))

# Build reactable - doesn't quite work as intended for the moment
reactable(epl,
          columns = list(
            WLD_time = colDef(cell = react_sparkbar(epl, min_value = -1, max_value = 1)
          )))



## Trying a bump sankey plot showing the total number of points cumulatively 
# after certain number of matches played


football_long <- football_long %>% 
  mutate(points_won  =  case_when(WLD == "Win" ~   3,
                                  WLD == "Loss" ~ 0,
                                  WLD == "Draw" ~ 1),
         date = as.Date(Date,"%d/%m/%Y" ) )


points_ts <- football_long %>% 
  group_by(team) %>% 
  summarise(cum_points = cumsum(points_won)) %>% 
  mutate(matches_played = rep(seq(1,38))) %>% 
  mutate(fill = case_when(team == "Man City" ~ "#6CABDD",
                          TRUE ~ "grey82"),
         colour = case_when(team == "Man City"~ "#FFC659",
                            TRUE ~ "grey20"))

title_text <- "Share of points among EPL teams in the 2021-22 season"
tag_text <- "Manchester City reached the top of the table after 15 matches, \nand remained at that position for the remaining matches played"


ggplot(points_ts)+
  
  ggsankey::geom_sankey_bump(aes(x = matches_played, node = team, 
                                 value = cum_points,
                                 fill = fill,
                                 colour = colour),
                             type = "alluvial",
                             space = 0) +

  labs(title = title_text,
       subtitle = tag_text,
       caption = "**Data**: Premier League Match Data 2021-2022 via Evan Gower on Kaggle") +
  
  ylab("EPL points won") +
  xlab("Matches played") +
  
  theme_minimal() +
  
  scale_fill_identity(aesthetics = c("fill","colour")) +
  
  
  theme(plot.background = element_rect(fill = "#FEFEFE"),
        plot.subtitle = element_text(colour = "grey52"),
        plot.title = element_text(face = "bold"),
        plot.caption = ggtext::element_markdown()
        ) 

ggsave("tt_2022-04-22.png")

