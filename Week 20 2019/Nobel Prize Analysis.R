# Load packages
library(tidyverse)
library(ggmap)
library(extrafont)


# Load data
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")


# View the data
View(nobel_winners)


# Summarise data
nobel_filtered <- nobel_winners %>% 
  count(birth_country, sort = TRUE) %>% 
  filter(!is.na(birth_country))


# Load world map
map.world <- map_data('world') %>% 
  filter(region != "Antarctica")

map_data('world') %>%
  group_by(region) %>%
  summarise()


# Join datasets
country_join <- left_join(map.world, nobel_filtered, by = c('region' = 'birth_country'))
  
# Set theme
my_background <- 'white'
my_textcolour <- "grey19"
my_font <- 'Century Gothic'
my_theme <- theme(text = element_text(family = my_font),
                  plot.title = element_text(face = 'bold', size = 16),
                  plot.background = element_rect(fill = my_background),
                  plot.subtitle = element_text(size = 14, colour = my_textcolour),
                  plot.caption = element_text(size = 8, hjust = 1.15, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, colour = my_background),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text = element_blank())


theme_set(theme_light() + my_theme)


# Plot map
ggplot(country_join, aes( x = long, y = lat, group = group )) +
  geom_point(aes(colour = "black")) +
  geom_polygon(aes(fill = n)) +
  labs(title = "Nobel Prize Winners By Country of Origin",
       subtitle = "1901 - 2016",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: ") +
  guides(
    fill = guide_legend(title = "# of Winners"))

