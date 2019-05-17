# Load packages
library(tidyverse)
library(extrafont)

# Import data
student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")


# Inspect data
View(student_ratio)

# Group countries and calculate mean
ratio_grouped <-student_ratio %>%
  filter(indicator == "Tertiary Education") %>% 
  group_by(country) %>%
  mutate(mean = mean(student_ratio, na.rm = TRUE))


# Load world map
map.world <- map_data('world') %>% 
  filter(region != "Antarctica")

map_data('world') %>%
  group_by(region) %>%
  summarise()


# Join datasets
country_join <- left_join(map.world, ratio_grouped, by = c('region' = 'country'))


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


# Plot map and save image
ggplot(data = country_join, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mean)) +
  scale_fill_gradientn(colours = c('#F9E53F', '#7FD157','#2A8A8C','#404E88', '#461863', '#462255')) +
  labs(title = "Student to Teacher Ratio in Tertiary Education",
       subtitle = "2012 - 2018",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: UNESCO Institute of Statistics") +
  guides(
    fill = guide_legend(title = "Ratio"))

ggsave('Student Ratios.png', device = "png", type = "cairo")

 
