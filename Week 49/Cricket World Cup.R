## Tidy Tuesday Week 49 - World Cup Cricket
## Data Source: ESPN Cricinfo

# Load packages
library(tidyverse)
library(here)
library(scales)
library(lubridate)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
tuesdata <- tidytuesdayR::tt_load(2021, week = 49)

matches <- tuesdata$matches

# Inspect data
glimpse(matches)

# Wrangle data
NZ_df <- matches %>%

  # Pivot data
  pivot_longer(c(team1, team2), names_to = "teams", values_to = "Teams") %>%

  # Tidy column headers and add result column
  mutate(score = case_when(teams == "team1" ~ score_team1,
                      TRUE ~ score_team2),
         home_away = case_when(teams == "team1" ~ team1_away_or_home,
                           TRUE ~ team2_home_away),
         result = case_when(winner == "New Zealand" ~ "Won",
                          TRUE ~ "Lost"),

  # Add year and data columns
         year = year(parse_date(match_date, format = "%b %d, %Y")),
         date = parse_date(match_date, format = "%b %d, %Y")) %>%

  # Select needed columns and filter for New Zealand
  select(Teams, score, year, date, home_away, winner, result) %>%
  filter(Teams == "New Zealand" & !is.na(year)) %>%

  # Add blank rows and rank column
  group_split(year) %>%
  map_dfr(~ .x %>%
            add_row(score = 0,  Teams = "New Zealand",  .before = 1)) %>%
  fill(year, .direction = "up")%>%
  mutate(rank = row_number(),

         result_filtered = case_when(result == "Lost" ~ "Lost",
                            TRUE ~ "Won"))

# Label data
Label_df <- NZ_df %>%
  group_by(year) %>%
  summarize(start=min(rank)+1, end=max(rank)) %>%
  rowwise() %>%
  mutate(title=mean(c(start, end)))

#Set theme
font_family <- 'Century Gothic'
background <- "#464655"
text_colour <- "white"
axis_colour <- "white"
theme_style <- theme(text = element_text(family = font_family),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(size = 30, colour = text_colour, hjust = 0.5),
                     plot.subtitle = element_text(size = 16, colour = text_colour, hjust = 0.5),
                     plot.caption = element_text(size = 12, colour = text_colour, margin=margin(60,0,0,0), hjust = 0.5),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.line = element_blank(),
                     legend.text = element_text(size = 12, colour= text_colour),
                     legend.title = element_blank(),
                     legend.box = "horizontal",
                     legend.position="bottom",
                     legend.justification = "center")

theme_set(theme_classic() + theme_style)

# Plot data
ggplot(NZ_df, aes(x = rank, y = score)) +

  # Year labels
  geom_segment(data=Label_df, aes(x = start, y = -20, xend = end, yend = -20),
               colour = text_colour, size=0.6, inherit.aes = FALSE) +
  geom_text(data = Label_df, aes(x = title, y = -80, label = year),
            size = 3, family = font_family, parse = TRUE, color = text_colour) +

  # Bar chart
  geom_bar(data = NZ_df, aes(x = rank, y = score, fill = factor(result_filtered)),
           stat="identity") +
  scale_fill_grey(start = .15, end = .85) +
  ylim(-500, 400) +
  coord_polar(start = 0) +

  # Title
  geom_text(x = 350, y = -580,
            label = "Black Caps",
            family = font_family,
            size = 10,
            lineheight = 0.87,
            color = text_colour) +

  # Subtitle
  geom_text(x = 350, y = -460,
            label = "ICC Cricket World Cup\nODI Results By Year",
            family = font_family,
            size = 5,
            lineheight = 0.87,
            color = text_colour) +

  # Annotations
  ## 2005
  geom_text(x = 223, y = 440,
            label = "397",
            family = font_family,
            size = 4,
            lineheight = 0.87,
            color = text_colour) +

  ## 2004
  geom_text(x = 209, y = 385,
            label = "347",
            family = font_family,
            size = 4,
            lineheight = 0.87,
            color = text_colour) +

  ## 1999
  geom_text(x = 82, y = 390,
            label = "349",
            family = font_family,
            size = 4,
            lineheight = 0.87,
            color = text_colour) +


  # Caption
  labs(caption = "\nVisualisation: @JaredBraggins | Source: ESPN Cricinfo")

# Export plot
ggsave("Black Caps.png", width = 10.3, height = 12, limitsize = F)
