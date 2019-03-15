
library(tidyverse)

# import csv
bg <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-12/board_games.csv")


# Inspect the data
glimpse(bg)


# Get a running total of board games
running_total <-
  bg %>%
  group_by(year_published) %>%
  summarise(counts = n()) %>%
  mutate(running_total = cumsum(counts))


# plot the running total by year
running_total %>%
  ggplot(aes(x = year_published, y = running_total)) +
  geom_line(color = 'deepskyblue4') +
  geom_area(fill = 'deepskyblue4', alpha = .1) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_y_continuous(labels = comma) +
  geom_vline(xintercept=2000, linetype = "dotted") +
  labs( title = "The Number of Board Games Published Since 1950",
        subtitle = "Based on a running total of board games",
        x = "Year",
        y = "No. of Board\nGames") +
  annotate("text", x = 1967, y = 9500, 
           label = "There has been a significant increase\nin the number of published board games\nafter 2000",
           hjust = 0, color = 'deepskyblue4') +
  theme_classic() +
  theme(axis.title.y = element_text(size = 10, vjust = 1, hjust = 0, angle = 0),
       axis.title.x = element_text(size = 10, hjust = 0)) 

