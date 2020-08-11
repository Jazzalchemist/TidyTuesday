## Tidy Tuesday Week 32 - European Energy
## Data Source: Eurostat Energy

#Load packages
library(tidyverse)
library(ggpubr)
library(patchwork)
library(extrafont)

#Import data
energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')

#Inspect data
View(energy_types)

#Wrangle data
energy_2016 <- energy_types  %>% 
  filter(level != "Level 2") %>% 
  group_by(type) %>% 
  summarise("Sum" = sum(`2016`, na.rm = TRUE)) %>% 
  mutate("Percent" = round(Sum/sum(Sum) *100,1)) %>% 
  mutate("Year" = 2016) %>% 
  mutate("Energy Type" = case_when(
    type == "Conventional thermal" ~ "Conventional Thermal",
    type == "Nuclear" ~ "Nuclear",
    TRUE ~ "Renewable")) 

energy_2016 <- energy_2016 %>% 
  group_by(`Energy Type`) %>% 
  summarise(Percent = sum(Percent))

energy_2017 <- energy_types  %>% 
  filter(level != "Level 2") %>% 
  group_by(type) %>% 
  summarise("Sum" = sum(`2017`, na.rm = TRUE)) %>% 
  mutate("Percent" = round(Sum/sum(Sum) *100,1)) %>%
  mutate("Year" = 2017) %>% 
  mutate("Energy Type" = case_when(
    type == "Conventional thermal" ~ "Conventional Thermal",
    type == "Nuclear" ~ "Nuclear",
    TRUE ~ "Renewable")) 
energy_2017 <- energy_2017 %>% 
  group_by(`Energy Type`) %>% 
  summarise(Percent = sum(Percent))

energy_2018 <- energy_types  %>% 
  filter(level != "Level 2") %>% 
  group_by(type) %>% 
  summarise("Sum" = sum(`2018`, na.rm = TRUE)) %>% 
  mutate("Percent" = round(Sum/sum(Sum) *100,1)) %>%
  mutate("Year" = 2018) %>% 
  mutate("Energy Type" = case_when(
    type == "Conventional thermal" ~ "Conventional Thermal",
    type == "Nuclear" ~ "Nuclear",
    TRUE ~ "Renewable")) 
energy_2018 <- energy_2018 %>% 
  group_by(`Energy Type`) %>% 
  summarise(Percent = sum(Percent))

#Create Chart Labels
labs_2016 <- paste0(energy_2016$`Energy Type`, "\n(", energy_2016$Percent, "%)")
labs_2017 <- paste0(energy_2017$`Energy Type`, "\n(", energy_2017$Percent, "%)")
labs_2018 <- paste0(energy_2018$`Energy Type`, "\n(", energy_2018$Percent, "%)")


#Set theme
font_family <- 'Century Gothic'
background <- "#FFFBFF"
text_colour <- "#090C02"
axis_colour <- "black"
plot_colour <- "white"
renewable <- "#86BA90"
nuclear <- "#CDCDCD"
conventional <- "#CDCDCD"
theme_style <- theme(text = element_text(family = font_family),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(face = 'bold', size = 16, colour = text_colour),
                  plot.subtitle = element_text(size = 12, colour = text_colour),
                  plot.caption = element_text(size = 13, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  strip.text.x = element_text(size=12),
                  strip.background = element_blank(),
                  legend.position="none")

theme_set(theme_classic() + theme_style)

#Plot data
p1 <- ggpie(energy_2016, "Percent", label = labs_2016,
            title = "Renewable Energy\nGeneration\nin Europe", subtitle = "2016 -2018",
            lab.pos = "in", lab.font = 2, 
            fill = "Energy Type", color = "white",
            font.family = font_family, font.main = c(12, "bold"), legend = "none",
            palette = c(conventional, nuclear, renewable))

p2 <- ggpie(energy_2017, "Percent", label = labs_2017,
            lab.pos = "in", lab.font = 2, fill = "Energy Type", color = "white",
            font.family = font_family, font.main = "bold", legend = "none",
            palette = c(conventional, nuclear, renewable))

p3 <- ggpie(energy_2018, "Percent", label = labs_2018,
            caption = "Visualisation: @JaredBraggins | Source: Eurostat Energy",lab.pos = "in", 
            lab.font = 2, fill = "Energy Type", color = "white",
            font.family = font_family, font.main = "bold", font.caption = 8, legend = "none",
            palette = c(conventional, nuclear, renewable))

plot(p1 + p2 + p3)

#Export plot
ggsave("Renewable Energy.pdf", dpi = 700, width = 16, height = 8, unit = "cm")
