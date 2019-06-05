# Load packages
library(tidyverse)
library(extrafont)

# Import data
wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")

# Inspect data 
View(wine_ratings)

#Summarise data
top_variety <- wine_ratings %>% 
  filter(country == "Italy") %>%
  filter(variety %in% c("Zinfandel", "Syrah", "Red Blend", "Tempranillo", "Pinot Noir",
                        "Merlot", "Malbec", "Cabernet Sauvignon")) %>% 
  group_by(variety) %>% 
  summarise(rating = round(mean(points, na.rm = TRUE),1),
            n = n())

# Set theme
my_background <- '#EAD7B7'
my_textcolour <- "black"
mysubtitlecolour = "grey9"
my_chartcolour <- "#800000"
my_font <- 'Bodoni MT'
my_theme <- theme(text = element_text(family = my_font),
                  plot.title = element_text(face = 'bold', size = 16),
                  plot.background = element_rect(fill = my_background),
                  plot.subtitle = element_text(size = 14, colour = mysubtitlecolour),
                  plot.caption = element_text(size = 10, colour = mysubtitlecolour),
                  panel.background = element_rect(fill = my_background, colour = my_background),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.text.y = element_text(size = 14, colour = my_textcolour),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = "none")

theme_set(theme_light() + my_theme)


# Plot data
top_variety %>% 
  mutate(variety = fct_reorder(variety, rating)) %>% 
  ggplot(aes(variety, rating, label = rating)) +
  geom_segment(aes(y = 80, xend = variety, yend = rating)) +
  geom_point(size = 9.5, colour = my_chartcolour) +
  geom_text(colour = "white", size = 3) +
  coord_flip() +
  scale_y_continuous(limits = c(80,90)) +
  labs(title = "Top Rated Italian Red Wines",
       subtitle = "Based on average rating per variety",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: Kaggle")

ggsave(file = "Italian Wines.png",  dpi = 600)
