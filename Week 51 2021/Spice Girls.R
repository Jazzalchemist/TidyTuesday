## Tidy Tuesday Week 51 - Spice Girls
## Data Source: Spotify & Genius
# Idea: Graphic equalizer for each album looking at audio features

# Load packages
library(tidyverse)
library(here)
library(scales)
library(lubridate)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-12-14')
tuesdata <- tidytuesdayR::tt_load(2021, week = 51)

studio_album_tracks <- tuesdata$studio_album_tracks
lyrics <- tuesdata$lyrics

# Inspect data
glimpse(studio_album_tracks)

# Find mean of each feature
avg_df <- studio_album_tracks %>%
  group_by(album_name) %>%
  summarise(danceability = round(mean(danceability),2),
            valence = round(mean(valence),2),
            energy = round(mean(energy),2),
            liveness = round(mean(liveness),2),
            speechiness = round(mean(speechiness),2),
            acousticness = round(mean(acousticness),2))

# Pivot data and add duplicate rows based on mean
album_df <- avg_df %>%
  pivot_longer(cols = c(danceability, valence, energy, liveness, speechiness, acousticness),
               names_to = "feature",
               values_to = "value") %>%
  group_by(album_name, feature, value) %>%
  slice(rep(1:n(), each = (value * 10))) %>%
  mutate(n = 1,
         row_count = row_number())

# Add buckets for each feature
album_bucket <- album_df %>%
  mutate(feature_bucket = case_when(
    row_count == 1 ~ "0.1-0.3",
    row_count == 2 ~ "0.1-0.3",
    row_count == 3 ~ "0.1-0.3",
    row_count == 4 ~ "0.4-0.6",
    row_count == 5 ~ "0.4-0.6",
    row_count == 6 ~ "0.4-0.6",
    row_count == 7 ~ "0.7-0.9",
    row_count == 8 ~ "0.7-0.9",
    row_count == 9 ~ "0.7-0.9",
    row_count == 10 ~ "0.9-1")) %>%
  mutate(album_order = case_when(
    album_name == "Spice" ~ 1,
    album_name == "Spiceworld" ~ 2,
    album_name == "Forever" ~ 3)) %>%
    mutate(album_name_new = case_when(
      album_name == "Spice" ~ "1. Spice (1996)",
      album_name == "Spiceworld" ~ "2. Spiceworld (1997)",
      album_name == "Forever" ~ "3. Forever (2000)",
  ))

# Reorder data
album_bucket$album_name <- factor(album_bucket$album_name) %>%
  fct_reorder(album_bucket$album_order)

album_bucket$feature_bucket <- factor(album_bucket$feature_bucket) %>%
  fct_reorder(album_bucket$row_count, .desc = TRUE)

# Set theme
font_family1 <- "Century Gothic"
font_family2 <- "Bebas Neue"
background <- "#1E1E24"
text_colour <- "white"
axis_colour <- "white"
theme_style <- theme(text = element_text(family = font_family1),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(family = font_family2, size = 40, face = "bold", colour = text_colour),
                     plot.subtitle = element_text(family = font_family1, size = 12, colour = text_colour),
                     plot.caption = element_text(family = font_family1, size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     panel.spacing = unit(2, "lines"),
                     axis.title = element_blank(),
                     axis.text.x = element_text(family = font_family1, size = 10, colour= text_colour),
                     axis.text.y = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position ="none",
                     strip.text = element_text(colour = text_colour, size = 12, face = "bold", hjust = 0),
                     strip.background = element_rect(fill = background, colour = background))

theme_set(theme_classic() + theme_style)

# Assign Palette
cols = c("#6A6A99", "#FFB3BD", "#C4DA2D")

# Plot data
ggplot(album_bucket, aes(feature, n, fill = feature_bucket)) +
  geom_col(color = background, size = 1.75) +
  scale_fill_manual(values = cols) +

  # Add custom y-axis labels
  annotate("label", x = .5, y = 10, size = 3, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  "1") +
  annotate("label", x = .5, y = 5, size = 3, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  "0.5") +
  annotate("label", x = .5, y = 0, size = 3, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  "0") +

  # Facet and add labels
  facet_wrap(~ album_name_new, ncol = 1, scales='free_x') +
  labs(title = "SPICE GIRLS ALBUMS",
       subtitle = "Visualising average values per audio feature\n",
       caption = "\n\nVisualisation: @JaredBraggins | Data Source: Spotify & Genius")

# Export plot
ggsave("Spice Girls.png", width = 210, height = 297,  unit = "mm")
