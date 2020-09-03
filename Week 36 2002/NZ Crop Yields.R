## Tidy Tuesday Week 36 - Global Crop Yields
## Data Source: Our World in Data

#Load packages
library(tidyverse)
library(colorspace)
library(ggstream)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load('2020-09-01')
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

key_crops <- tuesdata$key_crop_yields

#Inspect data
glimpse(key_crops)

#Wrangle data
NZ_yield <- key_crops %>% 
  filter(Code == "NZL") %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower()) %>% 
  filter(!is.na(crop_production))

#Set theme
font_family1 <- 'Century Gothic'
font_family2 <- 'Times New Roman'
background <- "#39393A"
text_colour1 <- "white"
text_colour2 <- "black"
axis_colour <- "white"
theme_style <- theme(text = element_text(family = font_family1),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(size = 30, colour = text_colour1),
                  plot.subtitle = element_text(size = 16, colour = text_colour1),
                  plot.caption = element_text(size = 12, colour = text_colour1, margin=margin(60,0,0,0)),
                  panel.background = element_rect(fill = background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"),
                  axis.title = element_blank(),
                  axis.text.x = element_text(size = 14, colour= text_colour1),
                  axis.text.y = element_blank(),
                  axis.line = element_blank(),
                  legend.text = element_text(size = 10, colour= text_colour1),
                  legend.title = element_blank(),
                  legend.box = "horizontal",
                  legend.position="top",
                  legend.justification = "left")

theme_set(theme_classic() + theme_style)

#Set colour palette
cols <- c("#F3D2B3", "#F2B8A2", "#F38C8D", "#5E9DB8", "#ADA296", "#2C5F72")

#Plot data
ggplot(NZ_yield, aes(year, crop_production, fill = factor(crop))) +
  geom_stream(method = "raw", bw = .7) +
  scale_fill_manual(name = "crop", values = cols) +
  scale_x_continuous(breaks = seq(1960, 2020, 10)) +
  geom_vline(data = tibble(x = c(1960, seq(1970, 2010, by = 10), 2020)),
    aes(xintercept = x), color = axis_colour, size = .1) +
  geom_segment(aes(x=1961,xend=1982,y=45,yend=45),size = .5, color="white", linetype = 2) +
  annotate("label", x = 1971, y = 45, size = 6, fontface = "italic", label.size=NA,
           color = text_colour1, family = font_family2, fill = background,
           label =  "Golden Years of Agriculture") +
  geom_segment(aes(x=1984,xend=2018,y=45,yend=45),color="white", linetype = 2) +
  annotate("label", x = 2001, y = 45, size = 6, fontface = "italic", label.size=NA,
           color = text_colour1, family = font_family2, fill = background,
           label =  "Deregulation and Export Growth") +
  geom_curve(aes(x = 1975, y = -35, xend = 1975, yend = -17),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.5,
             color = "white", curvature = -0.3) +
  annotate("label", x = 1975, y = -35, size = 3.5, color = text_colour1, family = font_family1, fill = background,
           label = 
           "Trials for growing soybeans occurred\nin the 1970s, but evidence suggested it\nwasn't going to be a commercially\nviable crop"
           ) +
  annotate("text", x = 1965.5, y = 0, size = 3.5, color = text_colour2, family = font_family1,
           label = 
             "Majority of potatoes were\nsold fresh and prepared\nat home during the 1960s"
           ) +
  annotate("text", x = 1995, y = 0, size = 3.5, color = text_colour2, family = font_family1,
          label = 
            "Exporting Potatoes began\nin 1991 and have steadily\ngrown since then"
          ) +
  annotate("text", x = 2005, y = 5, size = 3.5, color = text_colour2, family = font_family1,
           label = "By 2007 over half of NZ's\npotato crop was processed\ninto chips and fries") +
  annotate("text", x = 2005, y = -15, size = 3.5, color = text_colour2, family = font_family1,
           label = "Additionally, Potatoes were\nNZ's second highest earning\nvegetable in 2007 (NZ$94.2M).\nOnions took the crown as the\nhighest earner") +
  labs(title = "Kiwis and their Potatoes",
       subtitle = "New Zealand Crop Yields (tonnes per hectare) Since 1961",
       caption = "Visualisation: @JaredBraggins | Sources: Our World in Data, Te Ara") +
  guides(fill = guide_legend(nrow = 1))

#Export plot
ggsave("NZ Crops.png", dpi = 700, width = 15, height = 9)
