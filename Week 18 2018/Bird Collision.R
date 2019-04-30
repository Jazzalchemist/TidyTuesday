# Load libraries
library(tidyverse)
library(lubridate)
library(ggridges)

# Import data
bird_collisions <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv") %>% 
  mutate(date = ymd(as.character(date))) %>% 
  mutate(year = year(date))

# Inspect data
head(bird_collisions)


# Set theme
# my_font <- 'Sans Serif'
my_background <- '#FFFAF0'
my_textcolour <- "grey29"
my_axiscolour <- "grey15" 
my_theme <- theme(#text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 16),
                  plot.subtitle = element_text(size = 14, colour = my_textcolour),
                  plot.caption = element_text(size = 10, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_text(size = 10, colour=my_axiscolour, vjust = 0.5, angle = 0),
                  axis.text.y = element_text(size = 8, colour= my_axiscolour),
                  axis.text.x = element_text(size = 8, colour= my_axiscolour),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(colour = "black", size = 0.5),
                  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                  legend.position="none")


theme_set(theme_light() + my_theme)

# Chart data
bird_collisions %>% 
  ggplot(aes(year, family, group = family)) +
  geom_density_ridges(aes(fill = family, color = family, scale = 2, alpha = 0.5)) + 
  labs(title = "Chicago Bird Collisions",
       subtitle = "1978-2016",
       caption = "",
       y = "Bird Family",
       x = "")

ggsave('Bird Collision.png', dpi = 'retina')
