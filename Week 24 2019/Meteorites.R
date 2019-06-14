# Load libraries
library(tidyverse)
library(extrafont)
library(gganimate)

# Import data
meteorites <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv") 

# Data wrangling
anim <- meteorites %>% 
  select(year, long, lat, mass, fall) %>%
  mutate(year = as.integer(year)) %>%
  rename("Fall" = fall) %>% 
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
                  legend.title = element_text(face = 'bold', size = 12, colour = map_textcolour),
                  legend.text = element_text(size = 10, colour = map_textcolour),
                  legend.title.align = 1,
                  legend.direction="horizontal",
                  legend.justification=c(1,0), 
                  legend.position="top",
                  legend.background = element_rect(fill = map_background),
                  legend.key = element_rect(fill = map_background))

theme_set(theme_light() + map_theme)

pal <- c("Found" ="#69C6CD", "Fell" = "#D8986A")

# Plot data
p <- ggplot() + 
  borders("world", colour = map_colour, fill = map_colour, alpha = 0.05) +
  geom_point(data = anim, aes(x = long, y = lat, colour = Fall, size = mass/1000)) +
  scale_colour_manual(values = pal) +
  scale_size_continuous(name = "Mass (kgs)") +
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
  shadow_mark(colour = pal, alpha = 0.6)

# Create GIF
anim_save("meteorite_animated.gif", p, fps = 8, type = "cairo", width = 800, height = 500)
