# Load packages
library(tidyverse)
library(rvest)
library(magrittr)
library(ggmap)
library(stringr)

# Import data
student_ratio <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# Inspect data
View(student_ratio)

# Group countries and calculate mean
ratio_grouped <-student_ratio %>%
  group_by(country) %>%
  mutate(mean = mean(student_ratio, na.rm = TRUE))

ratio_grouped

map.world <- map_data('world')

map_data('world') %>% group_by(region) %>% summarise() %>% print(n = Inf)

# Join datasets
country_join <- left_join(map.world, ratio_grouped, by = c('region' = 'country'))

View(country_join)

map_data(country_join) %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = mean))
