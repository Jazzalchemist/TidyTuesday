## Tidy Tuesday Week 41 - NCAA Tournaments
## Data Source: FiveThirtyEight

#Load packages
library(tidyverse)
library(here)
library(ggtext)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load('2020-10-06')
tuesdata <- tidytuesdayR::tt_load(2020, week = 41)

tournament <- tuesdata$tournament

#Inspect data
View(tournament)

#Wrangle data
text_adj <- 0.3

t_data <- tournament %>%
  count(tourney_finish, school, sort = T) %>% 
  filter(tourney_finish == "Champ") %>% 
  slice(1:10) %>% 
  mutate(label_pos = case_when(
                  school == "UConn" ~ 10 + text_adj,
                  school == "Tennessee" ~ 9 + text_adj,
                  school == "Stanford" ~ 8 + text_adj,
                  school == "Southern California" ~ 7 + text_adj,
                  school == "Notre Dame" ~ 6 + text_adj,
                  school == "Louisiana Tech" ~ 5 + text_adj,
                  school == "Baylor" ~ 4 + text_adj,
                  school == "Old Dominion" ~ 3 + text_adj,
                  school == "North Carolina" ~ 2 + text_adj,
                  school == "Maryland" ~ 1 + text_adj,
                ))

t_year <- tournament %>%
  filter(tourney_finish == "Champ") %>%
  select(school, year) %>% 
  group_by(school) %>% 
  filter(year == max(year))

t_join <- inner_join(t_data, t_year, by = "school") %>% 
  mutate(school = fct_reorder(school, n))

#Set theme
font_family <- 'Be Vietnam'
background <- "#353531"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_markdown(size = 20, colour = text_colour),
                     plot.subtitle = element_text(size = 12, colour = text_colour),
                     plot.caption = element_markdown(size = 13, colour = text_colour),
                     panel.background = element_rect(fill = background, color = NA),
                     plot.margin = unit(c(1, 1, 1, 1), "cm"),
                     panel.border = element_blank(),
                     panel.grid.major.y = element_blank(),
                     panel.grid.major.x = element_blank(),
                     panel.grid.minor.x = element_blank(),
                     axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     axis.line = element_blank(),
                     legend.text = element_text(size = 8, colour= text_colour),
                     legend.title = element_text(size = 10, colour= text_colour),
                     legend.position="none")

theme_set(theme_classic() + theme_style)

#Colour palette
cols <- c("#FFC52A", "#FDB829", "#F99D27", "#F79026", "#F79026",
          "#F48324", "#F27623", "#F06822", "#EE5B21", "#EC4E20")

#Plot data
ggplot(t_join) +
  geom_col(aes(n, school, fill = school),
           width = 0.3) +
  geom_segment(aes(x = n, 
                   xend = 16, 
                   y = 10:1, 
                   yend = 10:1), 
               size = .4, 
               color = axis_colour, 
               linetype = 1) +
  geom_text(data = t_join, 
            aes(y = label_pos, 
                x = 16, 
                label = school),
            color = text_colour,
            hjust = "left",
            size = 5) +
  geom_text(data = t_join, 
            aes(y = label_pos - 0.2, 
                x = 16, 
                label = glue::glue("Last win: {year}")),
            color = text_colour,
            hjust = "left",
            size = 3) +
  geom_text(data = t_join, 
            aes(y = label_pos, 
                x = 0, 
                label = if_else(n == 1, glue::glue("{n} Win"), glue::glue("{n} Wins"))),
            color = text_colour,
            hjust = "right",
            size = 5) +
  scale_x_reverse() +
  scale_fill_manual(values = cols) +  
  labs(title = "**NCAA** | Top Ten Tournament Winners",
       caption = "**Visualisation:** @JaredBraggins | **Source:** FiveThirtyEight")

#Export plot
ggsave("NCAA.png", width = 210, height = 297, dpi = 500, unit = "mm")
