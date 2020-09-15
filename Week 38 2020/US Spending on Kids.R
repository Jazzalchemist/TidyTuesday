## Tidy Tuesday Week 38 - US Govt Spending on Kids
## Data Source: Urban Institute

#Load packages
library(tidyverse)
library(extrafont)

#Import data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

#Inspect data
View(kids)

#Wrangle data
NY_kids <- kids %>% 
  select(state, variable, year, inf_adj) %>% 
  filter(state == "New York",
         year >= 2000,
         !is.na(inf_adj)) %>% 
  group_by(year) %>% 
  arrange(year, desc(inf_adj)) %>% 
  mutate(Rank = row_number())

NY_kids <- NY_kids %>% 
  mutate(highlight = ifelse(variable %in% c("PK12ed", "highered", "edsubs", "edservs", "pell",
                                            "HeadStartPriv"), TRUE, FALSE),
       variable_col = if_else(highlight == TRUE, variable, "NA"))

#Set theme
font_family <- 'Century Gothic'
background <- "#1D1D1D"
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
                  plot.margin = unit(c(2, 2, 2, 2), "cm"),
                  axis.title.y = element_text(face = 'bold', size = 16, colour = text_colour),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(size = 12, colour= axis_colour, vjust = 14),
                  axis.text.y = element_text(size = 12, colour = text_colour),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.text = element_text(size = 8, colour= text_colour),
                  legend.title = element_text(size = 10, colour= text_colour),
                  legend.position="none")

theme_set(theme_classic() + theme_style)

#Set colour palette
cols <- c("#FE5F55", "#4281A4", "#48A9A6", "#E4DFDA", "#39393A", "#D4B483", "#C1666B")

#Plot data
ggplot(NY_kids, aes(x = year, y = Rank, group = variable)) +
  geom_line(aes(color = variable_col), size = 2) +
  geom_point(aes(color = variable_col), size = 6) +
  scale_y_reverse(breaks = 23:1) +
  scale_x_continuous(breaks = seq(2000, 2016, 4), limits= c(1999, 2017), 
                     expand = c(.05, .05)) +
  geom_text(data = NY_kids %>%  filter(year == "2000"),
            aes(label = variable, x = 1999.8, color = variable_col),
            hjust = "right",
            fontface = "bold",
            size = 4) +
  geom_text(data = NY_kids %>%  filter(year == "2016"),
            aes(label = variable, x = 2016.2, color = variable_col),
            hjust = "left",
            fontface = "bold",
            size = 4) +
  coord_cartesian(ylim = c(24,1)) +
  scale_color_manual(values = cols) +
  #scale_y_reverse(breaks = 1:nrow(NY_kids)) +
  labs(title = "Education Spend in New York",
       subtitle = "Based on sixteen years of inflation-adjusted spend",
       caption = "Visualisation: @JaredBraggins | Sources: Urban Institute")

#Export plot
ggsave("NY Kids.png", width = 15, height = 9)
