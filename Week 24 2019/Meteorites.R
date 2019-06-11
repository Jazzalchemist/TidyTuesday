# Load libraries
library(tidyverse)
library(extrafont)
library(paletteer)
library(ggthemes)
library(gganimate)

# Import data
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

# Inspect data
View(meteorites)

# Load world map
world <- ggplot() +
  borders("world", colour = "#353535", fill = "#353535") +
  theme_map()
