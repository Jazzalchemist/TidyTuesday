# Load libraries
library(tidyverse)
library(extrafont)
library(paletteer)
library(ggthemes)
library(gganimate)

# Import data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") 

# Data wrangling
anim <- meteorites %>% 
  select(year, long, lat, mass) %>%
  mutate(year = as.integer(year)) %>%
  filter(!is.na(lat),
         !is.na(long),
         !is.na(mass), 
         year > 1900,
         year < 2020, 
         long < 300)

# Set theme
map_background <- '#3F4248'
map_colour <- "grey90"
map_textcolour <- "white"
map_dot <- "#D8986A"
map_font <- 'Century Gothic'
map_theme <- theme(text = element_text(family = map_font),
                  plot.title = element_text(size = 20, colour = map_textcolour),
                  plot.background = element_rect(fill = map_background),
                  plot.subtitle = element_text(face = 'bold', size = 50, colour = map_textcolour),
                  plot.caption = element_text(size = 14, colour = map_textcolour),
                  panel.background = element_rect(fill = map_background, colour = map_background),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  legend.position="none")

theme_set(theme_light() + map_theme)

# Plot data
p <- ggplot() + 
  borders("world", colour = map_colour, fill = map_colour, alpha = 0.1) +
  geom_point(data = anim, aes(x = long, y = lat, size = mass), colour = map_dot) +
  labs(title = "Meteorite Landings",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: NASA",
       subtitle = "{frame_time}") +
  transition_time(year) +
  shadow_mark(past = TRUE) +
  enter_fade() +
  exit_shrink() +
  ease_aes("linear") +
  shadow_mark(colour = map_dot, alpha = 0.4)

# Create animation
animate(p, fps = 8, type = "cairo", width = 800, height = 500)

# Create GIF
anim_save("meteorite_animated.gif", p, width = 800, height = 500)
