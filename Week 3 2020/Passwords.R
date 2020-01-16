## Tidy Tuesday Week 3 2020 - Passwords
## Data Source: Knowledge is Beautiful

#Load packages
library(tidyverse)
library(extrafont)
library(ggfittext)

#Import data
passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')


#Inpect data
head(passwords)

#Wrangle data
passwords_filtered <- passwords %>% 
  na.omit(category) %>% 
  select(category) %>% 
  group_by(category) %>% 
  tally() %>% 
  arrange(-n) %>% 
  mutate(category = fct_reorder(category, n))

#Calculate mean password strength for name category
passwords_mean <- passwords %>%
  group_by(category) %>%
  summarise(mean = mean(strength)) %>% 
  filter(category == "name")


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
                  plot.caption = element_text(size = 11, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_text(face = 'bold', size = 13, colour= my_axiscolour),
                  axis.title.x = element_blank(),
                  axis.text.y = element_text(size = 12, colour= my_axiscolour),
                  axis.text.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(colour = "black", size = 0.5),
                  axis.line.x = element_blank(),
                  legend.position="none")

theme_set(theme_light() + my_theme)

# Plot chart
passwords_filtered %>% 
  mutate(highlight_flag = ifelse(category == "name",T,F)) %>%
  ggplot(aes(category, n, fill = highlight_flag)) +
  scale_fill_manual(values = c('#8A8D91', '#734B5E')) +
  geom_col() + 
  geom_bar_text(place = "right", contrast = TRUE) +
  coord_flip() +
  geom_curve(aes(x = 7.5, y = 167, xend = 9.3, yend = 181),
             arrow = arrow(length = unit(0.3, "cm")), size = 0.4,
             color = "grey20", curvature = 0.3) +
  annotate("text", x = 7.5, y = 85, size = 4, hjust = 0, color = my_textcolour, family = my_font,
           label = "The average password strength is ") +
  annotate("text", x = 6.5, y = 85, size = 10, hjust = -2, color = '#734B5E', family = my_font,
           label = 7.2) +
  labs(title = "People Really Like Using Names as Passwords!",
       subtitle = "Based on number of passwords per category",
       caption = "\nVisualisation: @JaredBraggins | Data Source: Information is Beautiful")

ggsave('Passwords.png', type = "cairo")
