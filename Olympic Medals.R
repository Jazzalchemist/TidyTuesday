## Tidy Tuesday Week 31 - Olympic Medals
## Data Source: Kaggle

#Load packages
library(tidyverse)
library(here)
library(lubridate)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load('2021-07-27')
tuesdata <- tidytuesdayR::tt_load(2021, week = 31)

olympics <- tuesdata$olympics

#Inspect data
View(olympics)

#Filter down to NZ teams only
NZ_medals <- olympics %>% 
  filter(team == "New Zealand" & !is.na(medal))
 
#group by year, sport and medal
NZ_group <- NZ_medals %>% 
  select(year, medal) %>% 
  group_by(year, medal) %>% 
  count(medal)

#Order medals
NZ_group$facet = factor(NZ_group$medal, levels = c("Gold", "Silver", "Bronze"))

#Set theme
font_family <- "Century Gothic"
background <- "#040F0F"
text_colour <- "White"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(face = 'bold', size = 20, colour = text_colour),
                  plot.subtitle = element_text(size = 16, colour = text_colour),
                  plot.caption = element_text(size = 12, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  plot.margin = unit(c(1, 2, 1, 2), "cm"),
                  panel.spacing = unit(2, "lines"),
                  panel.border = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_text(colour = text_colour, size = 12),
                  axis.ticks = element_line(colour = axis_colour),
                  axis.line.y = element_blank(),
                  axis.line.x = element_line(colour = axis_colour, size = 0.5),
                  strip.background = element_blank(),
                  strip.text = element_text(size = 14, face = 'italic', colour= text_colour),
                  legend.position="none")

theme_set(theme_classic() + theme_style)

#Add colour palette
cols <- c("#AD8A56", "#AF9500","#D7D7D7")

#Plot data
ggplot(data = NZ_group) +
  geom_col(aes(year, n, fill = medal)) + 
  facet_grid(~facet) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = cols) + 
  labs(title = "New Zealand Olympic Medals",
       subtitle = "1920 - 2016",
       caption = "\nVisualisation: @JaredBraggins | Source: Kaggle")

#Export plot
ggsave("NZ_Medals.png", dpi = 320, width = 15, height = 9)
