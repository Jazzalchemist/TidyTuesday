## Tidy Tuesday Week 5 2020 - San Francisco Trees
## Data Source: data.sfgov.org

#Load packages
library(tidyverse)
library(lubridate)
library(stringr)
library(patchwork)
library(extrafont)
library(ggfittext)

#Import data
sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


#Wrangle data
name = data.frame(str_split(sf_trees$species, ":: ", simplify = TRUE),
                  stringsAsFactors = FALSE)

colnames(name) = c("species", "name")
name$name[name$name == ""] = "Unknown"

sf_trees$name = name$name

sf_trees2 <- sf_trees %>% 
  mutate(date = case_when(
    is.na(date) ~ as.Date("1954-12-31", origin = "1970-01-01"),
    TRUE ~ date
  ),
  yr = year(date)) %>% 
  filter(between(yr, 1960, 2019)) %>% 
  mutate(decade = floor(yr / 10) * 10) %>% 
  mutate(decade_text = factor(case_when(
    decade == 1950 ~ "1950s",
    decade == 1960 ~ "1960s",
    decade == 1970 ~ "1970s",
    decade == 1980 ~ "1980s", 
    decade == 1990 ~ "1990s",
    decade == 2000 ~ "2000s",
    decade == 2010 ~ "2010s",
    decade == 2020 ~ "2020s",
    TRUE ~ "Rest"
  ))) %>% 
  mutate(decade_text = fct_reorder(decade_text, decade)) %>% 
  filter(latitude < 45 & latitude > 37.6)

sf_trees_top10 <- sf_trees2 %>% 
  group_by(decade_text, name) %>%
  filter(name != "Unknown") %>%
  summarise(count = n()) %>%
  top_n(10, count) %>%
  ungroup()

#Set theme
my_font <- 'Century Gothic'
my_background <- '#363732'
my_textcolour <- "white"
my_axiscolour <- "white" 
my_fill <- "#C6D4FF"
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 14, colour = my_textcolour),
                  plot.subtitle = element_text(size = 12, colour = my_textcolour),
                  plot.caption = element_text(size = 9, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_text(size = 8, colour= my_textcolour),
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank(),
                  #axis.line = element_blank(),
                  legend.position="none")

theme_set(theme_light() + my_theme)

#Chart each decade
sf_80s <- sf_trees_top9 %>%
  filter(decade_text == "1980s") %>% 
  mutate(name = fct_reorder(name, count)) %>%
  ggplot(aes(name, count)) +
  geom_col(fill = my_fill) +
  geom_bar_text(place = "right", size = 9, contrast = TRUE) +
  coord_flip() +
  ylim(0,500) +
  labs(title = "1980s")

sf_90s <- sf_trees_top9 %>%
  filter(decade_text == "1990s") %>% 
  mutate(name = fct_reorder(name, count)) %>%
  ggplot(aes(name, count)) +
  geom_col(fill = my_fill) +
  geom_bar_text(place = "right", size = 9, contrast = TRUE) +
  coord_flip() +
  ylim(0,1500) +
  labs(title = "1990s")

sf_00s <- sf_trees_top9 %>%
  filter(decade_text == "2000s") %>% 
  mutate(name = fct_reorder(name, count)) %>%
  ggplot(aes(name, count)) +
  geom_col(fill = my_fill) +
  geom_bar_text(place = "right", size = 9, contrast = TRUE) +
  coord_flip() +
  ylim(0,2000) +
  labs(title = "2000s")

sf_10s <- sf_trees_top9 %>%
  filter(decade_text == "2010s") %>% 
  mutate(name = fct_reorder(name, count)) %>%
  ggplot(aes(name, count)) +
  geom_col(fill = my_fill) +
  geom_bar_text(place = "right", size = 9, reflow = TRUE, contrast = TRUE) +
  coord_flip() +
  ylim(0,1500) +
  labs(title = "2010s")

patchwork <-  (sf_80s | sf_90s | sf_00s | sf_10s)

patchwork + plot_annotation(
  title = 'Most Common Tree Species in San Francisco',
  subtitle = 'Based on the decade in which they were planted',
  caption = 'Visualisation: @JaredBraggins | Data Source: DataSF'
)

ggsave("sf_trees.png", dpi = 500, width = 13, height = 5)
