## Tidy Tuesday Week 48 - Dr Who
## Data Source: datardis package

# Load packages
library(tidyverse)
library(here)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-11-23')
tuesdata <- tidytuesdayR::tt_load(2021, week = 48)

eps <- tuesdata$episodes

# Inspect data
glimpse(eps)

# Wrangle data
e_df <- eps %>%
  filter(era == "revived" & !is.na(rating)) %>%
  group_by(season_number) %>%
  mutate(season_year = case_when(
    season_number == 1 ~ 2005,
    season_number == 2 ~ 2006,
    season_number == 3 ~ 2007,
    season_number == 4 ~ 2008,
    season_number == 5 ~ 2010,
    season_number == 6 ~ 2011,
    season_number == 7 ~ 2012,
    season_number == 8 ~ 2014,
    season_number == 9 ~ 2015,
    season_number == 10 ~ 2017,
    season_number == 11 ~ 2018,
    season_number == 12 ~ 2020,
    season_number == 13 ~ 2021,
  ),
  showrunner = case_when(
    season_number >= 1 &  season_number < 6 ~ "Russell T Davies",
    season_number >= 6 &  season_number < 11 ~ "Steven Moffat",
    season_number >= 11 ~ "Chris Chibnall",
  ),
  min_rating = round(min(rating),1),
  max_rating = round(max(rating),1)) %>%
  ungroup() %>%
  mutate(mean_rating = round(mean(rating),1)) %>%
  group_by(showrunner) %>%
  mutate(sr_rating = round(mean(rating),1)) %>%
  ungroup()

# Set theme
point_size <- 8
font_family1 <- "Century Gothic"
font_family2 <- 'Assiduous'
background <- "#545454"
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
                     axis.title = element_text(family = font_family, size = 16, face = "bold",
                                               hjust = 0.03, colour= text_colour),
                     axis.text.x= element_text(family = font_family, size = 12, colour= text_colour),
                     axis.text.y = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     legend.position="top")

theme_set(theme_classic() + theme_style)

# Plot data
ggplot(e_df) +
  # X-axis
  geom_hline(data = e_df, aes(yintercept = mean_rating),
             colour = axis_colour,size = .5, linetype = 'dotted') +

  # Segments between points
  geom_segment(aes(x = season_year, xend = season_year,
                   y = min_rating, yend = max_rating),
               colour = segment_colour, size = .5, color="#56615c") +

  # Russell T Davies
  geom_segment(aes(x = 2005,xend = 2010, y = 93, yend = 93),
               size = .5, color = segment_colour, linetype = 1) +
  geom_segment(aes(x = 2005,xend = 2005, y = 93, yend = 92),
               size = .5, color = segment_colour, linetype = 1) +
  geom_segment(aes(x = 2010,xend = 2010, y = 93, yend = 92),
               size = .5, color = segment_colour, linetype = 1) +
  annotate("label", x = 2007.5, y = 93, size = 3.5, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  "Russell T Davies\nFive Seasons\nAverage rating: 85.4") +

  # Steven Moffat
  geom_segment(aes(x = 2011, xend = 2017, y = 93, yend = 93),
               size = .5, color = segment_colour, linetype = 1) +
  geom_segment(aes(x = 2011,xend = 2011, y = 93, yend = 92),
               size = .5, color = segment_colour, linetype = 1) +
  geom_segment(aes(x = 2017,xend = 2017, y = 93, yend = 92),
               size = .5, color = segment_colour, linetype = 1) +
  annotate("label", x = 2014, y = 93, size = 3.5, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  "Steven Moffat\nFive Seasons\nAverage rating: 84.1") +

  # Chris Chibnall
  geom_segment(aes(x = 2018,xend = 2021, y = 93, yend = 93),
               size = .5, color = segment_colour, linetype = 1) +
  geom_segment(aes(x = 2018,xend = 2018, y = 93, yend = 92),
               size = .5, color = segment_colour, linetype = 1) +
  geom_segment(aes(x = 2021,xend = 2021, y = 93, yend = 92),
               size = .5, color = segment_colour, linetype = 1) +
  annotate("label", x = 2019.5, y = 93, size = 3.5, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  "Chris Chibnall\nThree Seasons\nAverage rating: 80.2") +

  # Average rating label
  annotate("label", x = 2020.7, y = 84.7, size = 4, label.size=NA,
           color = text_colour, family = font_family1, fill = background,
           label =  paste("Average rating: ", e_df$mean_rating)) +

  # Min rating points
  geom_point(aes(x = season_year, y = min_rating),
             color = "#E2294F", size = point_size) +
  geom_text(data = e_df, aes(x = season_year, y = min_rating, label = min_rating),
            colour = "white", size = 3.5) +

  # Max rating points
  geom_point(aes(x = season_year, y = max_rating),
             color = "#77ACA2", size = point_size) +
  geom_text(data = e_df, aes(x = season_year, y = max_rating, label = max_rating),
            colour = "white", size = 3.5) +

  labs(title = "Dr Who Show Runners",
       subtitle = "Exploring the best and worst ratings for each showrunner from the revived era of Dr Who\n",
       y = "",
       x = "SEASONS",
       caption = "\nVisualisation: @JaredBraggins | Source: datatardis package")

# Export plot
ggsave("Dr Who.png", width = 15, height = 9)
