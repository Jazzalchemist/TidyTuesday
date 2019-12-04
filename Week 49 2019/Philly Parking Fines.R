## Tidy Tuesday Week 49 - Philadelphia Parking Violations
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
tuesdata <- tidytuesdayR::tt_load("2019-12-03")
tuesdata <- tidytuesdayR::tt_load(2019, week = 49)

tickets <- tuesdata$tickets

#Inspect data
head(tickets)

#Strip time from date column
tickets <- tickets %>% 
  mutate(date_formatted = as.Date(tickets$issue_datetime, format="%d %m %Y"))

#Wrangle top ten violations data
tickets_top10 <- tickets %>% 
  select(violation_desc, fine) %>%
  group_by(violation_desc) %>% 
  summarise(total=n(), total = sum(fine)) %>% 
  top_n(n=10, total)

tickets_top10$violation_desc <- factor(tickets_top10$violation_desc) %>%
  fct_reorder(tickets2$total)

#Wrangle ridgeline chart data
tickets_line <- tickets %>% 
  filter(!is.na(issuing_agency)) %>% 
  select(issuing_agency, fine, date_formatted) %>%
  group_by(issuing_agency, day=floor_date(date_formatted, "day")) %>% 
  summarise(total = sum(fine))

# Set theme
my_font <- 'Century Gothic'
my_background <- 'gray95'
my_textcolour <- "gray17"
my_axiscolour <- "black" 
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 18, colour = my_textcolour),
                  plot.subtitle = element_text(size = 15, colour = my_textcolour),
                  plot.caption = element_text(size = 13, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_text(size = 12, colour= my_axiscolour),
                  axis.text.x = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.position="none")

theme_set(theme_light() + my_theme)

#Plot top ten data
p1 <- tickets_top10 %>% 
  ggplot(aes(violation_desc, total, label = dollar(total), fill = total)) +
  scale_fill_gradient(low = "#80ADA0", high = "#326273") +
  geom_col() +
  geom_bar_text(place = "right", contrast = TRUE) + 
  coord_flip() +
  labs(title = "Top Ten Parking Violations in Philadelphia",
       subtitle = "Total Sum of Fine in 2017")

#Plot issuing agency data
p2 <- tickets_line %>% 
  ggplot(aes(day, issuing_agency)) +
  geom_density_ridges(scale = 1.8, aes(fill = issuing_agency), alpha = 0.5, color = "gray95") +
  scale_fill_manual(values = c("#2E4057", "#92140C", "#66717E", "#627264", "#80A1C1", 
                               "#16302B", "#85B79D", "#D16014", "#2D232E", "#474448")) +
  labs(title = "Fine Amount By Issuing Agency",
       subtitle = "Total Sum of Fine By Day", 
       caption = "Visualisation: @JaredBraggins | Source: Open Data Philly",
       x = "Date") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size = 14, hjust=-0.008, colour= my_axiscolour),
        axis.text.y = element_text(size = 12, colour= my_axiscolour),
        axis.text.x = element_text(size = 12, colour= my_axiscolour),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_line(colour = "black", size = 0.5),
        axis.line.x = element_line(colour = "black", size = 0.5, linetype = "solid"),
        legend.position="none")

(p1/p2) + plot_layout(guides = 'collect')

ggsave("philly_fines.png", dpi = 700, width = 10, height = 12)

