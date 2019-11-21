# Load libraries
library(tidyverse)
library(ggridges)
library(extrafont)
library(paletteer)

# Load data
tuesdata <- tidytuesdayR::tt_load("2019-11-19")
tuesdata <- tidytuesdayR::tt_load(2019, week = 47)

nz_bird <- tuesdata$nz_bird

# Inspect data
View(nz_bird)

# Wrangle data
nz_bird <- na.omit(nz_bird)

nz_bird_top10 <- nz_bird %>%
  filter(vote_rank == 'vote_1') %>%
  group_by(bird_breed) %>%
  tally() %>%
  arrange(-n) %>%
  head(5) %>%
  inner_join(nz_bird)

# Set theme
my_font <- 'Century Gothic'
my_background <- 'gray95'
my_textcolour <- "gray17"
my_axiscolour <- "black" 
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 20, colour = my_textcolour),
                  plot.subtitle = element_text(size = 15, colour = my_textcolour),
                  plot.caption = element_text(size = 11, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 14, hjust=-0.008, colour= my_axiscolour),
                  axis.text.y = element_text(size = 12, colour= my_axiscolour),
                  axis.text.x = element_text(size = 12, colour= my_axiscolour),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(colour = "black", size = 0.5),
                  axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
                  legend.position="none")

pal <- paletteer_c(viridis, "viridis", n = 5)
theme_set(theme_light() + my_theme)

# Plot ridgeline chart
nz_bird_top10 %>% 
  ggplot(aes(date, bird_breed)) +
  geom_density_ridges(scale = 1.3, aes(fill = bird_breed), alpha = 0.5, color = "gray95") +
  scale_fill_manual(values = pal) +
  labs(title = "Bird of the Year 2019",
       subtitle = "Top five birds voted for during the competition", 
       caption = "Visualisation: @JaredBraggins | Source: Forest & Bird",
       x = "Date")

#Save png
ggsave("nz_bird_2019.png", dpi = 700, width = 12, height = 6)
