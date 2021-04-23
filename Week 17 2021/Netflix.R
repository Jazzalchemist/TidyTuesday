## Tidy Tuesday Week 17 - Netflix Analysis
## Data Source: Kaggle

# Load packages
library(tidyverse)
library(ggstream)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-04-20')
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix <- tuesdata$netflix

# Inspect data
head(netflix)

## Wrangle data
# Add 'added year' column
n_year <- netflix %>%
  rowwise() %>%
  mutate(added_year = as.numeric(strsplit(date_added, split=",")[[1]][2]))

# Separate genres and pivot into one column, summarize based on added_year
n_genre <- n_year %>%
  mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>%
  unnest(listed_in) %>%
  filter(type == "TV Show") %>%
  count(listed_in, added_year) %>%
  select(added_year, listed_in, n)

# find top 5 genres by count of shows
n_top5 <- n_year %>%
  mutate(listed_in = strsplit(as.character(listed_in), ", ")) %>%
  unnest(listed_in) %>%
  filter(type == "TV Show") %>%
  group_by(listed_in) %>%
  ungroup() %>%
  count(listed_in) %>%
  top_n(n=5) 

# join datasets
n_join <- n_genre %>%
  inner_join(n_top5, by = "listed_in")

# Set theme
font_family1 <- 'Century Gothic'
font_family2 <- 'Roadgeek 2005 Series 4B'
background <- "#000000"
text_colour1 <- "white"
text_colour2 <- "black"
axis_colour <- "white"
theme_style <- theme(text = element_text(family = font_family1),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(size = 20, colour = text_colour1, family = font_family2),
                     plot.subtitle = element_text(size = 16, colour = text_colour1),
                     plot.caption = element_text(size = 12, colour = text_colour1, margin=margin(60,0,0,0)),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title = element_blank(),
                     axis.text.x = element_text(size = 14, colour= text_colour1),
                     axis.text.y = element_blank(),
                     axis.line = element_blank(),
                     axis.ticks = element_blank(),
                     legend.text = element_text(size = 10, colour= text_colour1),
                     legend.title = element_blank(),
                     legend.box = "horizontal",
                     legend.position="top",
                     legend.justification = "left")

theme_set(theme_classic() + theme_style)

# Set colour palette
cols <- c("#C39549", "#E60A14", "#10264D", "#5C2858", "#FEFFFE")

# Plot data
ggplot(n_join, aes(added_year, n.x, fill = listed_in)) +
  geom_stream() +
  scale_fill_manual(name = "listed_in", values = cols) +
  geom_vline(data = tibble(x = c(2013, seq(2013, 2021, by = 2), 2021)),
             aes(xintercept = x), color = axis_colour, size = .1) +
  scale_x_continuous(breaks = seq(2013, 2021, 2)) +
  labs(title = "Popular TV Show Genres on Netflix",
       subtitle = "The top five television genres 2013-2021\n",
       caption = "Visualisation: @JaredBraggins | Source: Kaggle") +
  guides(fill = guide_legend(nrow = 1))

# Export plot
ggsave("Netflix.png", dpi = 700, width = 15, height = 9)
