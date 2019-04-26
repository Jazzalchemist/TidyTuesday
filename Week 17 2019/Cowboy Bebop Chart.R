# Load packages
library(tidyverse)
library(lubridate)
library(paletteer)


# Import data
anime <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv") %>% 
  mutate(start_date = ymd(as.character(start_date))) %>% 
  mutate(start_year = year(start_date))


# Inspect data
View(anime)


# Filter all series that aired in 1998. Remove all films.
anime_filtered <- 
  anime %>% 
  select(title_english, type, score, start_date, start_year, status) %>%
  filter(status == "Finished Airing" & 
           type == "TV" & 
           start_year == 1998 &
           !is.na(title_english)) %>% 
  arrange(desc(score)) %>% 
  distinct(title_english, .keep_all = TRUE)


# Set formatting for chart
my_font <- 'Quicksand'
my_background <- 'antiquewhite'
my_palette <- c('#F8AFA8','#74A089')
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  plot.title = element_text(face = 'bold', size = 20),
                  plot.subtitle = element_text(size = 14),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_line(color = 'grey75'),
                  panel.grid.minor.x = element_line(color = 'grey75'),
                  legend.position = 'none',
                  plot.caption = element_text(size = 8),
                  axis.ticks = element_blank(),
                  axis.text.y =  element_blank())


# Plot top 20 to see where Cowboy Bebop places
anime_filtered %>% 
  top_n(20, score) %>% 
  mutate(title_english = fct_reorder(title_english, score)) %>% 
  ggplot(aes(x = title_english, y = score)) +
  geom_col() +
  coord_flip()
