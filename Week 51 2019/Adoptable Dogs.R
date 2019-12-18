
## Tidy Tuesday Week 51 - Adoptable Dogs
## Data Source: Petfinder

#Load packages
library(tidyverse)
library(usmap)
library(paletteer)
library(extrafont)

#Import data
dog_moves <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')

#Inpect data
head(dog_moves)

#Wrangle data
dog_filtered <- dog_moves %>% 
  filter(inUS == TRUE & !is.na(total)) %>% 
  replace_na(list(imported = 0, exported = 0)) %>% 
  rename(state = location)

# Set theme
my_font <- 'Century Gothic'
my_background <- 'gray95'
my_textcolour <- "gray17"
my_axiscolour <- "black" 

theme_set(theme_light() + my_theme)

#Plot map
plot_usmap(data = dog_filtered, values = "total", size = 0.2) + 
  scale_fill_paletteer_c(name = "No. of Dogs", "scico", "tokyo", -1) +
  theme(text = element_text(family = my_font),
        rect = element_rect(fill = my_background),
        plot.background = element_rect(fill = my_background, color = NA),
        plot.title = element_text(face = 'bold', size = 16, colour = my_textcolour),
        plot.subtitle = element_text(size = 14, colour = my_textcolour),
        plot.caption = element_text(size = 10, colour = my_textcolour),
        panel.background = element_rect(fill = my_background, color = NA),
        panel.border = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank())+
  labs(title = "New York had the highest number of adoptable dogs",
       subtitle = "As of September 20, 2019",
       caption = "Visualisation: @JaredBraggins | Source: PetFinder")

ggsave('Adoptable Dogs.png', type = "cairo")
