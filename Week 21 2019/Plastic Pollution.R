# Load libraries
library(tidyverse)
library(extrafont)
library(paletteer)
library(ggalt)


# Import data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")


# Rename columns and remove nulls
colnames(coast_vs_waste)[1] <- c("Country")
colnames(coast_vs_waste)[4] <- c("Mismanaged_Waste")
colnames(coast_vs_waste)[6] <- c("Population")

coast_filtered <- na.omit(coast_vs_waste)


# View data
View(coast_filtered)


# Load world map
map.world <- map_data('world') %>% 
  filter(region != "Antarctica")


# Join datasets
country_join <- left_join(map.world, coast_filtered, by = c('region' = 'Country'))


# Set theme
my_background <- 'White'
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
                  axis.text = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.background = element_rect(fill = my_background),
                  legend.key = element_rect(fill = my_background),
                  legend.title.align = 1)

theme_set(theme_light() + my_theme)
world_map <- borders("world", colour = "white", size = 0.05)


# Plot area map
ggplot(country_join, aes( x = long, y = lat)) +
  geom_polygon(aes(fill = Mismanaged_Waste, group = group )) +
  #scale_fill_gradientn(colours = c('#9CDA3B','#24858C', '#31668E')) +
  labs(title = "Coastal Plastic Mismanagement by Country",
       subtitle = "In 2010",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: Our World in Data",
       fill = "Waste (Tonnes)") +
  world_map +
  scale_fill_paletteer_c("ggthemes", "Temperature Diverging", 1,
                         breaks = c(2500000, 5000000, 7500000, 10000000))


# Calculate top ten most wasteful countries
top_ten <- coast_filtered %>% 
  filter(rank(desc(Mismanaged_Waste))<=10)
top_ten


# Join datasets
top_join <- left_join(map.world, top_ten, by = c('region' = 'Country')) %>% 
  na.omit(top_ten) %>% 
  unique(c('Mismanaged_Waste', 'long', 'lat', 'region'))
  

# Plot bubble map
ggplot() +
  geom_cartogram(
    data = map.world, map = map.world,
    aes(x = long, y = lat, map_id = region),
    color = "#113c7a", fill = "#113c7a", size = 0.125
  ) +
  geom_point(
    data = top_join, aes(long, lat, size = Mismanaged_waste), fill = "#ffe923",
    shape = 21, alpha = 0.8, stroke = 0.25, color = "#ffe923"
  ) +
  coord_proj("+proj=robin") +
  scale_size_area(name = "Number of Nobel Laureates", breaks = c(10, 50, 100, 200), max_size = 30, labels = scales::comma) +
  labs(
    x = NULL, y = NULL,
    title = "Nobel Winners by country",
    subtitle = "Size of bubble indicates number of Nobel lauretes",
    caption = "Source: Kaggle"
  )
