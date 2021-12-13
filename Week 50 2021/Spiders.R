## Tidy Tuesday Week 50 - Spider Species
## Data Source: World Spiders Database

# Load packages
library(tidyverse)
library(ggstream)
library(here)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-12-07')
tuesdata <- tidytuesdayR::tt_load(2021, week = 50)

spiders <- tuesdata$spiders

# Inspect data
glimpse(spiders)

# Filter for NZ spiders
NZ_spiders <- spiders %>%
  filter(distribution == "New Zealand") %>%
  group_by(family, year) %>%
  summarise(genus_count = n_distinct(genus), spider_count=n()) %>%
  ungroup() %>%
  top_n(n=12)

# Set theme
font_family1 <- "Century Gothic"
font_family2 <- 'Assiduous'
background <- "#424B54"
text_colour <- "white"
axis_colour <- "#EFF2F1"
segment_colour <- "white"
plot_colour <- "gray"
theme_style <- theme(text = element_text(family = font_family1),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(family = font_family2, size = 40, colour = text_colour),
                     plot.subtitle = element_text(family = font_family1, size = 15, colour = text_colour),
                     plot.caption = element_text(family = font_family1, size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 2, 2, 2), "cm"),
                     axis.title= element_blank(),
                     axis.text.x = element_text(family = font_family1, size = 12, colour= text_colour),
                     axis.text.y = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position="top",
                     legend.title = element_blank(),
                     legend.text = element_text(colour = text_colour, size = 10))

theme_set(theme_classic() + theme_style)

# Set colour palette
cols <- c("#9e0142", "#d53e4f", "#f46d43", "#fdae61", "#fee08b",
          "#e6f598", "#abdda4", "#66c2a5", "#3288bd", "#5e4fa2")

# Plot data
ggplot(NZ_spiders, aes(year, spider_count, fill = family)) +
  geom_stream(bw = 0.75) +
  scale_fill_manual(name = "family", values = cols) +
  geom_vline(data = tibble(x = c(1960, seq(1960, 1990, by = 10), 1990)),
             aes(xintercept = x), color = axis_colour, size = .1) +
  scale_x_continuous(breaks = seq(1960, 1990, 10)) +
  labs(title = "New Zealand Spiders",
       subtitle = "The top ten families based on the number of identified spiders by year\n",
       caption = "\nVisualisation: @JaredBraggins | Source: World Spiders Database") +
  guides(fill = guide_legend(nrow = 1))

# Export plot
ggsave("NZ Spiders.png", width = 15, height = 9)
