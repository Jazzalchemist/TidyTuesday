# Load libraries
library(tidyverse)
library(extrafont)
library(paletteer)
library(ggthemes)
library(gganimate)

# Import data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") %>% 
  filter(!is.na(lat),!is.na(long),!is.na(mass)) 


# Data wrangling
anim <- meteorites %>% 
  select(year, long, lat, mass) %>%
  mutate(year = as.integer(year)) %>% 
  filter(year > 1900, year < 2020, long < 300)#, fall == "Fell")

# Set theme
my_background <- 'grey89'
my_textcolour <- "grey19"
my_font <- 'Century Gothic'
my_theme <- theme(text = element_text(family = my_font),
                  plot.title = element_text(face = 'bold', size = 20),
                  plot.background = element_rect(fill = my_background),
                  plot.subtitle = element_text(size = 50, colour = my_textcolour),
                  plot.caption = element_text(size = 8, hjust = 1.15, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, colour = my_background),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text = element_blank(),
                  legend.position="none")

theme_set(theme_light() + my_theme)


ggplot() + 
  borders("world", colour = "#353535", fill = "white") +
  geom_point(data = anim, aes(x = long, y = lat, size = mass), alpha = 0.6) +
  labs(title = "Meteorite Landings",
       x = "",
       y = "")


p <- ggplot() + 
  borders("world", colour = "#353535", fill = "white") +
  geom_point(data = anim, aes(x = long, y = lat, size = mass), colour = "red", alpha = 0.6) +
  labs(title = "Meteorite Landings",
       x = "",
       y = "",
       subtitle = "{frame_time}") +
  transition_time(year) +
  shadow_mark(past = TRUE) +
  ease_aes("sine-in-out")

# Create animation
animate(p, fps = 15, type = "cairo", width = 800, height = 500)
