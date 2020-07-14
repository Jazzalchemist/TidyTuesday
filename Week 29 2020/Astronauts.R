## Tidy Tuesday Week 29 - Astronauts
## Data Source: Mariya Stavnichuk and Tatsuya Corlett
## https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-07-14/readme.md

#Load packages
library(tidyverse)
library(extrafont)

#Import data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

#Inspect data
view(astronauts)

#Wrangle data
astro <- astronauts %>% 
  filter(between(year_of_mission, 1960, 2019)) %>% 
  mutate(decade = floor(year_of_mission / 10) * 10) %>% 
  mutate(decade_text = factor(case_when(
    decade == 1960 ~ "1960s",
    decade == 1970 ~ "1970s",
    decade == 1980 ~ "1980s", 
    decade == 1990 ~ "1990s",
    decade == 2000 ~ "2000s",
    decade == 2010 ~ "2010s",
    decade == 2020 ~ "2020s",
    TRUE ~ "Rest"
  ))) %>% 
  mutate(decade_text = fct_reorder(decade_text, decade))

#Group data by gender
astro_decade <- astro %>% 
  select(decade_text, sex) %>% 
  group_by(decade_text, sex) %>%
  summarise(count = n()) %>%
  top_n(5, count) %>%
  ungroup()

#Set theme
my_font <- 'Century Gothic'
my_background <- "#0b3d91"
my_textcolour <- "white"
my_axiscolour <- "white"
my_plotcolour <- "black"
my_fill <- "white"
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 14, colour = my_textcolour),
                  plot.subtitle = element_text(size = 11, colour = my_textcolour),
                  plot.caption = element_text(size = 9, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_line(colour = my_axiscolour, size = 0.05),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_line(colour = my_axiscolour, size = 0.05),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_text(size = 11, colour= my_textcolour),
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank(),
                  strip.text.x = element_text(size=11, colour = my_textcolour),
                  strip.background = element_rect(colour= my_background, 
                                                  fill= my_background),
                  legend.position="none")

theme_set(theme_light() + my_theme)

#Plot data
a_chart <- astro_decade %>%
  mutate(sex = fct_reorder(sex, count)) %>%
  mutate(highlight_flag = ifelse(sex == 'male', T, F)) %>%
  ggplot(aes(sex, count, fill = highlight_flag)) +
  scale_fill_manual(values = c('white', 
                               '#fc3d21')) +
  geom_col() +
  geom_bar_text(place = "right", size = 9, contrast = TRUE) +
  coord_flip() +
  ylim(0,500) +
  labs( title = "Astronaut Gender",
        subtitle = "Based on the decade in which their mission occurred\n",
        caption = '\nVisualisation: @JaredBraggins | Data Source: Mariya Stavnichuk and Tatsuya Corlett\n') +
  facet_wrap(~decade_text, ncol = 2)


#Export plot
ggsave("Astronauts.png", a_chart, dpi = 500, width = 20, height = 12, unit = "cm")
