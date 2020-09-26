## Tidy Tuesday Week 39 - Himalayan Climbing Expeditions
## Data Source: The Himalayan Database

#Load packages
library(tidyverse)
library(scales)
library(patchwork)
library(ggfittext)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load('2020-09-22')
tuesdata <- tidytuesdayR::tt_load(2020, week = 39)

expeditions <- tuesdata$expeditions

#Inspect data
View(expeditions)

#Pivot cols
e_pivot <- expeditions %>% 
  select(expedition_id, peak_name, termination_reason, members, 
         member_deaths, hired_staff, hired_staff_deaths) %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "Category", 
               values_to = "Number")

#Sum top ten peaks by number of climbers
top_ten <- e_pivot %>% 
  filter(Category %in% c("members", "hired_staff")) %>% 
  group_by(expedition_id, peak_name) %>%
  ungroup() %>% 
  group_by(peak_name) %>%
  summarise(Climbers = sum(Number)) %>%
  top_n(n=10, Climbers)

#Sum successful climbers
success <- e_pivot %>% 
  filter(Category %in% c("members", "hired_staff"),
         grepl("Success",termination_reason)) %>% 
  group_by(expedition_id, peak_name) %>%
  ungroup() %>% 
  group_by(peak_name) %>%
  summarise(Successful = sum(Number))

#Sum deaths
deaths <- e_pivot %>% 
  filter(Category %in% c("member_deaths", "hired_staff_deaths")) %>% 
  group_by(expedition_id, peak_name) %>%
  ungroup() %>% 
  group_by(peak_name) %>%
  summarise(Deaths = sum(Number))

#join tables
e_data <- inner_join(top_ten, success, by="peak_name") %>% 
  inner_join(., deaths, by='peak_name') %>% 
  mutate(Success_Rate = Successful/Climbers,
         Death_Rate = Deaths/Climbers,
    peak_name = fct_reorder(peak_name, Climbers, .desc=F))

#Set theme
font_family <- 'Century Gothic'
background <- "#3D3638"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(face = 'bold', size = 20, colour = text_colour),
                     plot.subtitle = element_text(size = 14, colour = text_colour),
                     plot.caption = element_text(size = 10, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     axis.title.y = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.y = element_text(size = 12, colour = text_colour),
                     axis.text.x = element_blank(),
                     axis.ticks = element_blank(),
                     axis.line = element_blank(),
                     legend.position="none")

theme_set(theme_classic() + theme_style)

#Plot total climbers
p1 <- e_data %>%
  mutate(highlight = ifelse(peak_name == "Everest",T,F)) %>%
  ggplot(aes(x = peak_name, 
             y = Climbers, 
             fill = highlight,
             label = comma(Climbers))) +
  geom_col() +
  geom_bar_text(place = "right", 
                contrast = TRUE,
                fullheight = FALSE,
                size = 10) +
  geom_segment(aes(x = 10.8, 
                   xend = 10.8, 
                   y = 0, 
                   yend = max(Climbers)), 
               size = .4, 
               color = axis_colour, 
               linetype = 1) +
  scale_y_continuous() +
  scale_fill_manual(values = c("#F2CC8F", "#C99E57")) +
  coord_flip() +
  labs(title = "Top Ten Summits by Number of Climbers") +
  theme(plot.title = element_text(size = 14, colour = "#F2CC8F", hjust = .2))

#Plot Successful Climbers
p2 <- e_data %>%
  mutate(highlight = ifelse(peak_name == "Ama Dablam",T,F)) %>%
  ggplot(aes(x = peak_name, 
             y = Success_Rate, 
             fill = highlight,
             label = percent(Success_Rate, accuracy = .1))) +
  geom_col() +
  geom_bar_text(place = "right", 
                contrast = TRUE,
                fullheight = FALSE,
                size = 10) + 
  geom_segment(aes(x = 10.8, 
                   xend = 10.8, 
                   y = 0, 
                   yend = max(Success_Rate)), 
               size = .4, 
               color = axis_colour, 
               linetype = 1) +
  scale_fill_manual(values = c("#81B29A", "#327B58")) +
  scale_y_continuous(limits = c(0, 0.8)) +
  coord_flip() +
  labs(title = "Success Rate") +
  theme(plot.title = element_text(size = 14, colour = "#81B29A", hjust = .07),
        axis.text.y = element_blank())
        
#Plot Climber Deaths
p3 <- e_data %>%
  mutate(highlight = ifelse(peak_name == "Annapurna I",T,F)) %>%
  ggplot(aes(x = peak_name, 
             y = Death_Rate, 
             fill = highlight,
             label = percent(Death_Rate, accuracy = .1))) +
  geom_col() +
  geom_bar_text(place = "right", 
                contrast = TRUE, 
                fullheight = FALSE,
                size = 10) + 
  geom_segment(aes(x = 10.8, 
                   xend = 10.8, 
                   y = 0, 
                   yend = .2), 
               size = .4, 
               color = axis_colour, 
               linetype = 1) +
  annotate("text", x = 1, 
           y = 0.10, 
           size = 3, 
           hjust = "left", 
           color = text_colour, 
           family = font_family,
           label = "*Please note: The scale for Death\nRate does not match Success Rate") +
  scale_fill_manual(values = c("#E07A5F", "#9B351A")) +
  scale_y_continuous(limits = c(0, 0.2)) +
  coord_flip() +
  labs(title = "Death Rate*") +
  theme(plot.title = element_text(size = 14, colour = "#E07A5F", hjust = .06), 
        axis.text.y = element_blank())

p1 + p2 + p3 + plot_layout(widths = c(1.2, 1, 1)) +
  plot_annotation(title = "The Most Climbed Summits in the Himalayas",
                  caption = "Visualisation: @JaredBraggins | Source: The Himalayan Database")

#Export
ggsave("Himalayan Climbers.png", width = 20, height = 9)
