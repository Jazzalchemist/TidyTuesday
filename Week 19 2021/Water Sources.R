## Tidy Tuesday Week 19 - Water Access Points
## Data Source: Water Point Data Exchange

#Load packages
library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load('2021-05-04')
tuesdata <- tidytuesdayR::tt_load(2021, week = 19)

water <- tuesdata$water

worldtilegrid <- read.csv("worldtilegrid.csv")

#Inspect data
head(water)

#Wrangle data
water2 <- water %>% 
  select(country_name, lat_deg, lon_deg, water_source, status_id) %>% 
  mutate(source = case_when(str_detect(water_source, "Spring") ~ "Spring",
                                   str_detect(water_source, "Well") ~ "Well",
                                   str_detect(water_source, "Borehole") ~ "Borehole",
                                   str_detect(water_source, "Surface Water") ~ "Surface",
                                   str_detect(water_source, "Piped Water") ~ "Piped",
                                   T ~ "Unknown")) %>% 
  select(country_name, lat_deg, lon_deg, source) %>% 
  rename(name = country_name) %>% 
  count(name, source) %>%
  select(name, source, n)

map_join <- water2 %>% 
  left_join(worldtilegrid, by = "name") %>% 
  filter(region == "Africa") %>% 
  select(name, source, alpha.3, x, y, n)

#Calculate max per source
max_source <- map_join %>% 
  group_by(source) %>%
  summarise(Value = max(n))

#Create locations in the facet for text
S1_text <- data.frame(x = 11.9, y = 16.8, name = "Nigeria", source = "Borehole")
S2_text <- data.frame(x = 12, y = 17, name = "Sierra Leone", source = "Piped")
S3_text <- data.frame(x = 18, y = 12.5, name = "Uganda", source = "Spring")
S4_text <- data.frame(x = 19, y = 19, name = "Swaziland", source = "Surface")
S5_text <- data.frame(x = 14, y = 12, name = "Uganda", source = "Unknown")
S6_text <- data.frame(x = 12, y = 17, name = "Sierra Leone", source = "Well")

#Add coordinates for leader lines
S1_curve <- data.frame(x = 12.5, y = 17, xend = 13, yend = 16.2, name = "Nigeria", source = "Borehole")
S2_curve <- data.frame(x = 11.5, y = 16.2, xend = 11.8, yend = 13.5, name = "Sierra Leone", source = "Piped")
S3_curve <- data.frame(x = 17.4, y = 12.8, xend = 16.3, yend = 14.8, name = "Uganda", source = "Spring")
S4_curve <- data.frame(x = 18.1, y = 19, xend = 17.2, yend = 19.5, name = "Swaziland", source = "Surface")
S5_curve <- data.frame(x = 14.2, y = 12.8, xend = 15.8, yend = 15.4, name = "Uganda", source = "Unknown")
S6_curve <- data.frame(x = 11.5, y = 16.2, xend = 11.8, yend = 13.5, name = "Sierra Leone", source = "Well")

#Set theme
font_family1 <- 'Century Gothic'
font_family2 <- "Georgia"
background <- "#F4F4ED"
text_colour1 <- "black"
text_colour2 <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family1),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(face = 'bold', size = 18, colour = text_colour1),
                     plot.subtitle = element_text(size = 12, colour = text_colour1),
                     plot.caption = element_text(size = 10, colour = text_colour1),
                     panel.background = element_rect(fill = background, color = NA),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     axis.line = element_blank(),
                     legend.text = element_text(size = 8, colour= text_colour1),
                     legend.title = element_text(size = 10, colour= text_colour1),
                     strip.background = element_blank(),
                     strip.text = element_text(size = 14, face = c('italic'), colour= text_colour1, family = font_family2),
                     legend.position = "none")

theme_set(theme_classic() + theme_style)

#Set colour palettes
cols <- c("#F06449", "#4B3F72", "#F6AE2D", "#43AA8B", "#19647E", "#55868C")

#Plot data
p1 <- ggplot(map_join) + 
  geom_rect(aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1, alpha = n, fill = source),
            size = 0.4, colour = "black") +
  scale_y_reverse() +
  scale_fill_manual(values = cols) +
  scale_alpha(range = c(0.4, 0.8)) +
  geom_text(aes(x = x, y = y, label = alpha.3), 
            color = text_colour1, nudge_x = 0.5, nudge_y = -0.5, size = 3) +
  facet_wrap(. ~ source) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(title = "Water Sources in African Countries",
       subtitle = "The maps below show the various sources for water collection in Africa. Additionally, the nations with the highest number of water sources per category have been labelled.\n",
       caption = "Visualisation: @JaredBraggins | Source: Water Point Data Exchange")

#Add text to facet
p1 <- p1 +
  geom_label(data = S1_text, aes(x = x, y = y), label = 'atop(bold("Nigeria"),"74,884")',
             size = 3.5, color = text_colour, hjust = "center", label.size = NA, fill = background, parse = TRUE) +
  geom_label(data = S2_text, aes(x = x, y = y), label = 'atop(bold("Sierra Leone"),"3,072")',
             size = 3.5, color = text_colour, hjust = "center", label.size = NA, fill = background, parse = TRUE) +
  geom_label(data = S3_text, aes(x = x, y = y), label = 'atop(bold("Uganda"),"28,362")',
             size = 3.5, color = text_colour, hjust = "center", label.size = NA, fill = background, parse = TRUE) +
  geom_label(data = S4_text, aes(x = x, y = y), label = 'atop(bold("Swaziland"),"10,793")',
             size = 3.5, color = text_colour, hjust = "center", label.size = NA, fill = background, parse = TRUE) +
  geom_label(data = S5_text, aes(x = x, y = y), label = 'atop(bold("Uganda"),"34,286")',
             size = 3.5, color = text_colour, hjust = "center", label.size = NA, fill = background, parse = TRUE) +
  geom_label(data = S6_text, aes(x = x, y = y), label = 'atop(bold("Sierra Leone"),"33,600")',
             size = 3.5, color = text_colour, hjust = "center", label.size = NA, fill = background, parse = TRUE)

#Add leader lines
p1 <- p1 +
  geom_curve(data = S1_curve, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "black", curvature = 0.3) +
  geom_curve(data = S2_curve, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "black", curvature = -0.3) +
  geom_curve(data = S3_curve, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "black", curvature = 0.3) +
  geom_curve(data = S4_curve, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "black", curvature = -0.3) +
  geom_curve(data = S5_curve, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "black", curvature = 0.3) +
  geom_curve(data = S6_curve, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "black", curvature = -0.3)

#Export plot
ggsave("Water Access Points.png", dpi = 320, width = 15, height = 9)
