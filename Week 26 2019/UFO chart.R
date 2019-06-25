# Load libraries
library(tidyverse)
library(ggimage)
library(gganimate)
library(lubridate)
library(scales)
library(extrafont)


# Import data
ufo_sightings <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv")

# Inspect data
glimpse(ufo_sightings)

# Data wrangling
ufo_sightings$date_time <- mdy_hm(as.character(ufo_sightings$date_time))
ufo_sightings <- ufo_sightings %>% 
  mutate(year = year(date_time)) %>%
  filter(!is.na(latitude), !is.na(longitude))
  
ufo_area <- ufo_sightings %>% 
  group_by(year) %>% 
  summarise(total=n())

ufo_map <- ufo_sightings %>% 
  mutate(year = as.integer(year))

# Set theme
map_background <- 'black'
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
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank())

theme_set(theme_light() + map_theme)

image = "./ufo.png"

# Plot sightings by shape over time
ggplot(ufo_area, aes(year, total)) +
  geom_area(fill = "white", colour = "white", alpha = 0.5) +
  transition_reveal(year)

# Plot map
p <- ggplot(ufo_map, aes(x = longitude, y = latitude)) + 
  borders("world", colour = "white", fill = map_colour, alpha = 0.1) +
  geom_image(aes(image = image), size=.05) +
  labs(title = "UFO Sightings",
       caption = "Visualisation: @JaredBraggins | Data Source: NUFORC",
       subtitle = "{frame_time}") +
  transition_time(year) +
  shadow_mark(past = TRUE) +
  enter_fade() +
  exit_shrink() +
  ease_aes("linear")


# Create GIF
anim_save("ufo.gif", p, fps = 8, type = "cairo", width = 800, height = 500)
