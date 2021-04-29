## Tidy Tuesday Week 18 - CEO Departures
## Data Source: Gentry et al.

# Load packages
library(tidyverse)
library(here)
library(treemapify)
library(extrafont)
library(patchwork)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-04-27')
tuesdata <- tidytuesdayR::tt_load(2021, week = 18)

departures <- tuesdata$departures

# Inspect data
View(departures)

## Investigate Data
# Max/min years for context. This got me thinking about grouping by decade
departures %>% 
  summarize(min_delay = min(fyear), max_delay = max(fyear))

## Wrangle data
# Add decade column
dep_decade <- departures %>% 
  mutate(decade = case_when(fyear >= 1990 & fyear < 2000 ~ "1990s",
                            fyear >= 2000 & fyear < 2010 ~ "2000s",
                            fyear >= 2010 ~ "2010s")) %>% 
  filter(!is.na(decade))

# Count codes by decade and calculate percentages
departures_count <- dep_decade %>%
  mutate(departure_code = case_when(departure_code %in% c(1,2) ~ 'Health',
                                    departure_code %in% c(3,4) ~ 'Dismissed',
                                    departure_code == 5 ~ 'Retired',
                                    departure_code == 6 ~ 'New Venture',
                                    departure_code == 7 ~ 'Merger',
                                    TRUE ~ 'Other')) %>% 
  group_by(decade, departure_code) %>% 
  summarise(.groups = 'drop',
            count = n()) %>% 
  group_by(decade) %>% 
  mutate(pct = paste0(round(100 * count/sum(count), 0), "%"))

# Add highlight column and text
departures_count <- departures_count %>% 
  mutate(highlight = 
           ifelse(decade == "1990s" & departure_code == "Retired", 6,
                       ifelse(decade == "2000s" & departure_code == "Dismissed", 1, 
                                     ifelse(decade == "2010s" & departure_code == "Other", 5, 0))),
         text = 
           ifelse(decade == "1990s" & departure_code == "Retired", 
                       "Of all departure reasons, Retired\nwas the largest at 49%\n",
                       ifelse(decade == "2000s" & departure_code == "Dismissed", 
                              "Retired would remain the highest,\nbut Dismissed saw an increase of 8%\non the previous decade to reach 22%", 
                              ifelse(decade == "2010s" & departure_code == "Other", 
                                     "There was a significant increase in the\nOther category, which now represents\n39% of departure reasons", ""))))

# Set theme
font_family1 <- 'Century Gothic'
font_family2 <- 'Times New Roman'
background <- "#0E131F"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family1),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(family = font_family2, face = 'bold', size = 26, colour = text_colour),
                  plot.subtitle = element_text(size = 14, colour = text_colour),
                  plot.caption = element_text(size = 10, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.text = element_text(size = 8, colour= text_colour),
                  legend.title = element_blank(),
                  strip.background = element_blank(),
                  strip.text = element_blank(),
                  legend.position="none",
                  panel.spacing = unit(2, "lines"))

theme_set(theme_classic() + theme_style)

#Set colour palettes
cols <- c("#605B56", "#4ECDC4", "#F7FFF7", "#FF6B6B", "#FFE66D", "#59656F")

cols2 <- c(`1` = "#605B56", `2` = "#4ECDC4", `3` = "#F7FFF7", 
          `4` = "#BF4E30", `5` = "#EE6352", `6` = "#59656F")

## Plot data
# Labels
p1 <- ggplot(departures_count, aes(area = count, fill = departure_code)) +
  geom_text(data = departures_count, aes(y = 0.023, x = 0, 
                                         label = decade), family = font_family2, 
            size = 12, color = text_colour) +
  geom_text(data = departures_count, aes(y = 0.016, x = 0, 
                                         label = text), family = font_family1, 
            size = 3.5, color = text_colour) +
  facet_wrap(~decade, ncol = 1) +
  scale_y_continuous(limits = c(0, 0.025), breaks = seq(0, 0.025, 0.005))

# Faceted Treemaps
p2 <- ggplot(departures_count, aes(area = count, 
                                   fill = factor(highlight),
                                   label = paste(departure_code, pct, sep="\n"))) +
  geom_treemap(layout = "squarified") +
  geom_treemap_text(
    colour = "white",
    place = "topleft",
    family = font_family2,
    size = 15) +
  scale_fill_manual(values = cols2) +
  facet_wrap(~decade, ncol = 1) +
  guides(fill = guide_legend(nrow = 1))

final <- p1 + p2 + plot_layout(widths = c(1, 1.2)) +
  plot_annotation(title = "CEO Departures",
                  subtitle = "Exploring departure reasons across three decades\n",
                  caption = "\nVisualisation: @JaredBraggins | Source: Gentry et al. (2021)")

# Export plot
ggsave("CEO Departures.png", dpi = 320, height = 11, width = 8)
