# Load packages
library(tidyverse)
library(lubridate)
library(extrafont)


getwd()
# Load data
nobel_winners <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv") %>% 
  mutate(birth_year = year(birth_date)) %>% 
  mutate(birth_date = ymd(as.character(birth_date)))


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
my_background <- 'grey89'
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
                  legend.background = element_rect(fill = my_background),
                  legend.key = element_rect(fill = my_background),
                  legend.title.align = 1)

theme_set(theme_light() + my_theme)


# Plot map
ggplot(country_join, aes( x = long, y = lat, group = group )) +
  geom_polygon(aes(fill = n)) +
  scale_fill_gradientn(colours = c('#9CDA3B','#24858C', '#31668E')) +
  labs(title = "Nobel Prize Winners by Country of Origin",
       subtitle = "1901 - 2016",
       x = "",
       y = "",
       caption = "Visualisation: @JaredBraggins | Data Source: Harvard Dataverse") +
  guides(
    fill = guide_legend(title = "# of Winners"))

ggsave('Nobel Map.png', device = "png", type = "cairo")

# Calculate prize winners' age
winner_age <- nobel_winners %>% 
  select(full_name, birth_year, gender, prize_year, category) %>% 
  mutate(age = as.numeric(prize_year - birth_year)) %>% 
  filter(!is.na(birth_year))


# Boxplot of prize winner age/gender
winner_age %>% 
  ggplot(aes(category, age, fill = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c('#D4855A', '#6C4E97')) +
  labs(x = "Category", 
       y = "Age",
       title = "Nobel Prize Winners by Age and Gender",
       caption = "Visualisation: @JaredBraggins | Data Source: Harvard Dataverse") +
  theme(axis.line = element_line(colour = "black"),
        axis.text = element_text(),
        axis.ticks = element_line(colour = "black"))

ggsave('Nobel Boxplot.png', device = "png", type = "cairo")
