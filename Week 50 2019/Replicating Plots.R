## Tidy Tuesday Week 50 - Replicating Plots with R
## Data Source: Open Data Philly

#Load packages
library(tidyverse)
library(ggridges)
library(scales)
library(patchwork)
library(ggfittext)
library(lubridate)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load("2019-12-10")
tuesdata <- tidytuesdayR::tt_load(2019, week = 50)

diseases <- tuesdata$diseases

#Inspect data
head(diseases)

#Wrangle data
diseases_filtered <- diseases %>%
  select(disease, year, count, weeks_reporting, population) %>% 
  filter(disease == "Measles") %>%
  group_by(disease, year) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>% 
  filter(!is.na(rate)) 


df <- diseases_filtered %>% 
  select(year)

View(df)

#Set theme
my_font <- 'Century Gothic'
my_background <- "#2A324B"
my_textcolour <- "#E1E5EE"
my_axiscolour <- "white"
my_plotcolour <- "#F7C59F"
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 16, colour = my_textcolour),
                  plot.subtitle = element_text(size = 12, colour = my_textcolour),
                  plot.caption = element_text(size = 13, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_text(size = 12, colour= my_axiscolour),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.text = element_text(size = 8, colour= my_textcolour),
                  legend.title = element_text(size = 10, colour= my_textcolour),
                  legend.position="left")

theme_set(theme_classic() + my_theme)

#Plot timeline
timeline_plot <-
  ggplot(diseases_filtered,aes(x=year,y=0, col=disease))+ 
  geom_hline(yintercept=0, color = my_axiscolour, size=0.3) + 
  geom_segment(aes(y = 0.47, x = 1963, yend=0, xend= 1963), colour = my_axiscolour, size=0.2) +
  geom_segment(aes(y = 0.27, x = 1936, yend=0, xend= 1936), colour = my_axiscolour, size=0.2) +
  geom_segment(aes(y = 0.37, x = 1997, yend=0, xend= 1997), colour = my_axiscolour, size=0.2) +
  geom_point(aes(y=0, size=rate), colour = my_plotcolour) + 
  annotate(geom = "text", x = 1963, y = 0.5, label = "The measles vaccine was first introduced in 1963", 
           size = 3, hjust = 0.2, colour = my_textcolour) +
  annotate(geom = "text", x = 1936, y = 0.3, label = "The highest rate of measles occurred in 1936", 
           size = 3, hjust = 0.2, colour = my_textcolour) +
  annotate(geom = "text", x = 1997, y = 0.42, label = "There was a signficant increase\nin the measles rate in 1997", 
           size = 3, hjust = 0.5, colour = my_textcolour) +
  labs(title = "Measles Rates in the United States",
       subtitle = "1928 - 2002")

ggsave("measles_timeline.png", timeline_plot, dpi = 700, width = 25, height = 10, unit = "cm")
