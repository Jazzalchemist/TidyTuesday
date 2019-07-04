# Load libraries
library(tidyverse)
library(ggfittext)
library(extrafont)
library(scales)
library(grid)
library(ggpubr)

# Import data
media_franchises <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-02/media_franchises.csv")

# Inspect data
glimpse(media_franchises)

# Data wrangling
top5 <- media_franchises %>%
  filter(original_media == "Film")

top5 <- top5 %>% 
  group_by(franchise) %>%
  summarise(total_revenue = sum(revenue)) %>%
  top_n(5) %>%
  mutate(franchise = fct_reorder(franchise, total_revenue))


# Set theme
my_font <- 'Century Gothic'
my_background <- 'black'
my_titlecolour <- "#EFCD01"
my_textcolour <- "white"
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 22, colour = my_titlecolour),
                  plot.subtitle = element_text(size = 16, colour = my_textcolour),
                  plot.caption = element_text(size = 14, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_text(size = 12, colour= my_textcolour),
                  axis.text.x = element_text(size = 12, colour = my_textcolour),
                  axis.ticks = element_blank(),
                  axis.line.x = element_line(colour = "white", size = 0.5, linetype = "solid"),
                  legend.position="none")

theme_set(theme_light() + my_theme)

# Load images
img <- png::readPNG("RedFive_X-wing_SWB.png")
rast <- grid::rasterGrob(img, interpolate = T)

# Plot top five film franchises
top5 %>% 
  mutate(highlight_flag = ifelse(franchise == "Star Wars",T,F)) %>%
  ggplot(aes(franchise, total_revenue)) +
  annotation_custom(rast, ymin = 40, ymax = 60, xmin = -2) +
  geom_segment(aes(y = 0, xend = franchise, yend = total_revenue), colour = "grey50") +
  geom_point(aes(colour = highlight_flag), size = 8) +
  scale_fill_manual(values = c('grey65', 'skyblue4')) +
  scale_y_continuous(labels = scales::dollar_format(prefix="$", suffix = "bn")) +
  coord_flip() +
  labs(title = "Star Wars For the Win",
       subtitle = "Based on the Top Five Film Franchises by Revenue (Billions)",
       caption = "\nVisualisation: @JaredBraggins | Data Source: Wikipedia")
