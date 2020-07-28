## Tidy Tuesday Week 31 - Palmer Penguins
## Data Source: Gorman, Williams and Fraser, 2014

#Load packages
library(tidyverse)
library(ggtext)
library(palmerpenguins)
library(extrafont)

#Inspect data
View(penguins)

#Wrangle data
penguins_tidy <- na.omit(penguins)

#Set theme
font_family <- 'Century Gothic'
background <- "#0b0d09"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "white"
theme_style <- theme(text = element_text(family = font_family),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_markdown(size = 16, colour = text_colour),
                  #plot.title = element_markdown(lineheight = 1.1),
                  plot.subtitle = element_text(size = 11, colour = text_colour),
                  plot.caption = element_text(size = 10, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.x = element_text(size = 12, colour= axis_colour),
                  axis.text.y = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.position = "none")

theme_set(theme_classic() + theme_style)

#Plot data
penguins_tidy %>% 
  ggplot(aes(bill_length_mm, species)) +
  geom_density_ridges(scale = 0.8, aes(fill = species), alpha = 0.5, color = plot_colour) +
  scale_x_continuous(labels = paste0(seq(30,60,10), " mm"), position = "top") +
  scale_fill_manual(values = c(Adelie = "#7c948c", Chinstrap = "#16F4D0", Gentoo = "#FF8811")) +
  labs(title = "**Bill Length of Palmer Penguins**  
    <span style='color:#16F4D0;font-size:11pt;font-weight:normal;'>Chinstrap
    <span style='color:#FFFFFF;'>penguins tend to see more variation in bill length compared to
    <span style='color:#FF8811;'>Gentoo
    <span style='color:#FFFFFF;'>and
    <span style='color:#7c948c;'>Adelie
    <span style='color:#FFFFFF;'>penguins
    </span>",
       caption = "Visualisation: @JaredBraggins | Source: Gorman, Williams and Fraser, 2014")



#Export plot
ggsave("Penguins.png", dpi = 700, width = 8, height = 6)
