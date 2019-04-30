# Load libraries
library(tidyverse)
library(lubridate)
library(ggridges)

# Import data
bird_collisions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv") %>% 
  mutate(year = year(date))

# Inspect data
head(bird_collisions)

# Set theme
my_font <- 'Century Gothic'
my_background <- '#FFFAF0'
my_textcolour <- "grey29"
my_axiscolour <- "black" 
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 14),
                  plot.subtitle = element_text(size = 12, colour = my_textcolour),
                  plot.caption = element_text(size = 5, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_text(size = 8, colour=my_axiscolour, face="bold", vjust = 0.9),
                  axis.text.y = element_text(size = 6, colour= my_axiscolour),
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
       caption = "Source: Winger BM, Weeks BC, Farnsworth A, Jones AW, Hennen M, Willard DE (2019)\n
       Nocturnal flight-calling behaviour predicts vulnerability to artificial light in migratory birds.\n
       Proceedings of the Royal Society B 286(1900): 20190364. https://doi.org/10.1098/rspb.2019.0364",
       y = "Bird Family",
       x = "")

ggsave('Bird Collision.png', dpi = 'retina')
