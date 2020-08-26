## Tidy Tuesday Week 35 - Chopped
## Data Source: Kaggle & IMDb

#Load packages
library(tidyverse)
library(ggtext)
library(extrafont)

#Import data
tuesdata <- tidytuesdayR::tt_load('2020-08-25')
tuesdata <- tidytuesdayR::tt_load(2020, week = 35)

chopped <- tuesdata$chopped

#Inspect data
glimpse(chopped)

#Wrangle data
text_adj <- 5

chopped_df <- chopped %>% 
  filter(season <= 5) %>% 
  group_by(season) %>% 
  mutate(mean_rating = mean(episode_rating),
         label_pos = case_when(
           season == 1 ~ 1 + text_adj,
           season == 2 ~ 14 + text_adj,
           season == 3 ~ 27 + text_adj,
           season == 4 ~ 40 + text_adj,
           season == 5 ~ 53 + text_adj
         )) %>% 
  ungroup() 

chopped_df$season <- factor(chopped_df$season) %>%
  fct_reorder(chopped_df$series_episode)

#Check for values to use in annotations
chopped_avg <- chopped_df %>% 
  summarise(mean = mean(episode_rating))

chopped_max <- chopped_df %>% 
  select(season, season_episode, air_date, episode_name, episode_rating, episode_notes) %>% 
  filter(episode_rating == max(episode_rating))
  
chopped_min <- chopped_df %>% 
  select(season, season_episode, air_date, episode_name, episode_rating, episode_notes) %>% 
  filter(episode_rating == min(episode_rating))

#Set theme
font_family1 <- 'Century Gothic'
font_family2 <- "Bebas Neue"
background <- "#545454"
text_colour1 <- "white"
text_colour2 <- "#D9D9D9"
axis_colour <- "white"
plot_colour <- "gray"
theme_style <- theme(text = element_text(family = font_family2),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.subtitle = element_text(family = font_family1, size = 14, colour = text_colour1),
                  plot.caption = element_text(family = font_family1, size = 9, colour = text_colour1),
                  panel.background = element_rect(fill = background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_line(color = plot_colour, linetype = 2, size = .2),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.title.y = element_text(family = font_family1, size = 10, colour= text_colour1),
                  axis.text.y = element_text(family = font_family1, size = 10, colour= text_colour1),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.position="none")

theme_set(theme_classic() + theme_style)

#Set colour palette
cols <- c("#86090E", "#9B3A0F", "#CF5A13", "#F9DC5C", "#F88514")

#Plot data
ggplot(chopped_df) +
  geom_text(data = chopped_df, aes(y = mean_rating + .06, x = label_pos, 
                                   label = glue::glue(" SEASON {season} ")), family = font_family2, 
            size = 14, color = text_colour2) +
  geom_segment(aes(series_episode, xend=series_episode, y=mean_rating, yend=episode_rating), 
               size = .4, color = axis_colour, linetype = 2) +
  geom_line(aes(series_episode, mean_rating), color = axis_colour, size = .4) +
  scale_y_continuous(limits = c(8, 9.5), breaks = seq(8, 9.5, 0.5), sec.axis = dup_axis(name = NULL)) +
  geom_point(aes(series_episode, y=episode_rating, fill = season), colour="white", pch=21, size = 3.5) +
  scale_fill_manual(name = "season", values = cols) +
  #Lowest rating
  geom_curve(aes(x = 35.5, y = 8.02, xend = 39.5, yend = 8),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "white", curvature = 0.3) +
  annotate("text", x = 28, y = 8.1, size = 3.5, hjust = 0, color = text_colour1, family = font_family1,
           label = paste("The episode 'Rattle & Roll', \nwhich aired on April 6, 2010,", "\nhad the lowest rating of 8")) +
  #Highest ratings
  geom_curve(aes(x = 11.7, y = 9.32, xend = 13, yend = 9.22),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "white", curvature = -0.4) +
  geom_curve(aes(x = 2.4, y = 9.32, xend = 0.9, yend = 9.22),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "white", curvature = 0.4) +
  annotate("text", x = 2.7, y = 9.32, size = 3.5, hjust = 0, color = text_colour1, family = font_family1,
           label = paste("The pilot and final episodes\nof season one both have\na high rating of 9.2")) +
  #Madison Cowan mention
  geom_curve(aes(x = 47, y = 9.25, xend = 50.5, yend = 9.2),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "white", curvature = 0.3) +
  annotate("text", x = 38, y = 9.32, size = 3.5, hjust = 0, color = text_colour1, family = font_family1,
           label = "'Crunch Time', which aired on July 6, 2010, was the first appearance of Madison Cowan. \nCowan would go on to feature in other Food Network shows like Iron Chef America.\nThe episode has a rating of 9.2") +
  labs(title = "<b>Rating the Early Seasons of</b>
  <span style='font-size:32pt; color:#E26610;'>*CHOPPED*
       </span>",
       subtitle = "The first season of Chopped began airing in 2009. Despite seeing a steady decline in IMDb ratings for seasons one through to four\n(season five saw ratings starting to improve), the show maintained an average rating of 8.7",
       caption = "\nVisualisation: @JaredBraggins | Source: Kaggle & IMDb",
       y ="IMDb Rating") +
  theme(plot.title = element_markdown(size = 18, colour = text_colour1, family = font_family1))

#Export plot
ggsave("Chopped.png", width = 15, height = 9)
