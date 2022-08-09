## Tidy Tuesday Week 31 - Oregon Spotted Frog
## Data Source: usgs.gov

# Load packages
library(tidyverse)
library(here)
library(lubridate)
library(ggstream)

# Import data
tuesdata <- tidytuesdayR::tt_load('2022-08-02')
tuesdata <- tidytuesdayR::tt_load(2022, week = 31)

frogs <- tuesdata$frogs

# Inspect data
glimpse(frogs)

# Tidy data
frogs_df <- frogs %>% 
  mutate(Date = mdy(SurveyDate),
         MonthNo = month(Date),
         Year = year(Date),
         Month = month(Date, label = TRUE, abbr = FALSE))

# Group data
frog_groups <- frogs_df %>% 
  group_by(Date, Structure, HabType) %>% 
  summarise(FrogCount=n()) %>% 
  ungroup()

# Set theme
background <- "#D3D3D3"
text_colour <- "#171614"
axis_colour <- "#171614"
theme_style <- theme(rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(size = 20, face = "bold", colour = text_colour),
                     plot.subtitle = element_text(size = 12, colour = text_colour),
                     plot.caption = element_text(size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.minor.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.ticks.x = element_blank(),
                     panel.spacing = unit(2, "lines"),
                     axis.title = element_blank(),
                     axis.text = element_text( size = 10, colour= text_colour),
                     axis.line = element_blank(),
                     legend.title = element_blank(),
                     legend.position ="top",
                     strip.text = element_text(colour = text_colour, size = 12, face = "bold", hjust = 0),
                     strip.background = element_rect(fill = background, colour = background))

theme_set(theme_classic() + theme_style)

# Assign Palette
cols = c("#16BAC5", "#DBF4A7", "#FF7F11", "#607196", "#CD4631")

# Plot data
frog_groups %>% 
  ggplot(aes(Date, FrogCount, fill = Structure)) +
  geom_stream(extra_span = 0.013, n_grid = 4000, bw = .78) +
  facet_wrap(~HabType, ncol = 1) +
  scale_fill_manual(name = "structure", values = cols) +
  scale_x_date(breaks = as.Date(c("2018-10-01", "2018-11-01")),
               labels = c("October", "November")) +
  geom_vline(xintercept = as.numeric(frog_groups$Date[c(24, 82)]), color = axis_colour, size = .1) +
  labs(title = "Oregon Spotted Frogs",
       subtitle = "Volume of detected frogs by habitat structure during the late season at Crane Prairie Reservoir",
       caption = "Visualisation: @JaredBraggins | Source: USGS") 

# Export plot
ggsave("Oregon Spotted Frog.png", dpi = 350, width = 9, height = 12)
