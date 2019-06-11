# Load libraries
library(tidyverse)
library(ggridges)
library(extrafont)
library(paletteer)

# Import data
ramen_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-04/ramen_ratings.csv")

# Inspect data
View(ramen_ratings)

# Figure out the top ten countries
ramen_top10 <- ramen_ratings %>%
  group_by(country) %>%
  tally() %>%
  arrange(-n) %>%
  head(10) %>%
  inner_join(ramen_ratings)

# Set theme
my_font <- 'Century Gothic'
my_background <- '#FAECA5'
my_textcolour <- "grey19"
my_axiscolour <- "black" 
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 18),
                  plot.subtitle = element_text(size = 14, colour = my_textcolour),
                  plot.caption = element_text(size = 11, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 14, hjust=-0.008),
                  axis.text.y = element_text(size = 12, colour= my_axiscolour),
                  axis.text.x = element_text(size = 12, colour= my_axiscolour),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(colour = "black", size = 0.5),
                  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                  legend.position="none")

pal <- paletteer_c(scico, "berlin", n = 10)
theme_set(theme_light() + my_theme)

# Plot ridgeline chart
ramen_top10 %>% 
  ggplot(aes(stars, country)) +
  geom_density_ridges(scale = 2, aes(fill = country), alpha = 0.5, color = "#0000001A") +
  scale_fill_manual(values = pal) +
  labs(title = "Ramen Ratings",
       subtitle = "For the top ten countries with most observations in the dataset", 
       caption = "Source: The Ramen Rater",
       x = "Rating")

ggsave("ramen_rating.png", width = 12, height = 6)

