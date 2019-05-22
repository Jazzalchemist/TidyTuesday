# Load libraries
library(tidyverse)
library(extrafont)
library(paletteer)

# Import data
coast_vs_waste <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-21/coastal-population-vs-mismanaged-plastic.csv")

# Rename columns, remove nulls and create waste per person
colnames(coast_vs_waste)[1] <- c("Country")
colnames(coast_vs_waste)[4] <- c("Mismanaged_Waste")
colnames(coast_vs_waste)[5] <- c("Coastal_Population")
colnames(coast_vs_waste)[6] <- c("Population")

coast_filtered <- na.omit(coast_vs_waste) %>% 
  mutate(waste_per_person = (Mismanaged_Waste/Coastal_Population)*1000)
    
# View data
View(coast_filtered)

# Update name for USA
coast_filtered <- coast_filtered %>% 
  mutate(Country = str_replace_all(Country, c("United States" = "USA")))

# Load world map, filter out Antarctica
map.world <- map_data('world') %>% 
  filter(region != "Antarctica")

# Join datasets
country_join <- left_join(map.world, coast_filtered, by = c('region' = 'Country'))

# Set theme
my_background <- 'White'
my_textcolour <- "grey19"
my_font <- 'Century Gothic'
my_theme <- theme(text = element_text(family = my_font),
                  plot.title = element_text(face = 'bold', size = 14),
                  plot.background = element_rect(fill = my_background),
                  plot.subtitle = element_text(size = 12, colour = my_textcolour),
                  plot.caption = element_text(size = 8, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, colour = my_background),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "bottom",
                  legend.background = element_rect(fill = my_background),
                  legend.key = element_rect(fill = my_background),
                  legend.title.align = 1)

theme_set(theme_light() + my_theme)

# Plot map
ggplot(country_join, aes( x = long, y = lat)) +
  geom_polygon(aes(fill = waste_per_person, group = group )) +
  labs(title = "Plastic Mismanagement by Coastal Populations",
       subtitle = "In 2010",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: Our World in Data",
       fill = "Waste per person (kg)") +
  scale_fill_paletteer_c("ggthemes", "Temperature Diverging", 1,
                         breaks = c(25, 50, 75, 100))

ggsave(file = "coastal_plastic.png", height = 5, width = 8, units = 'in', device = "png", type = "cairo")
