## Tidy Tuesday Week 33 - Avatar: The Last Airbender
## Data Source: appa R package

#Load packages
library(tidyverse)
library(scales)
library(lubridate)
library(extrafont)
library(tvthemes)

#Import data
avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

#Inspect data
View(avatar)

#Wrangle data
a_data <- avatar %>% 
  mutate("book" = case_when(
    book == "Water" ~ "Book One: Water",
    book == "Earth" ~ "Book Two: Earth",
    TRUE ~ "Book Three: Fire")) %>% 
  group_by(book, book_num, chapter, chapter_num) %>% 
  filter(!is.na(imdb_rating)) %>% 
  summarise("Rating" = mean(imdb_rating)) 

#Reorder books
a_data$book <- factor(a_data$book) %>%
  fct_reorder(a_data$book_num)

a_data$chapter <- factor(a_data$chapter) %>%
  fct_reorder(a_data$Rating)

#Calculate mean per book
a_data_mean <- a_data %>% 
  group_by(book) %>% 
  summarise_at(vars(Rating),            
               list("Mean_Rating" = mean))

#Join both datasets together (there's probably a better way of doing this)
a_data_final <- a_data %>% 
  left_join(a_data_mean)

#Set theme
font_1 <- "Herculanum"
font_2 = "Century Gothic"
background <- "#54544A"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "#4B4C4E"
theme_style <- theme(text = element_text(family = font_1),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(family = font_1, face = 'bold', hjust = -2, size = 26, colour = text_colour),
                  plot.subtitle = element_text(family = font_2, size = 15, hjust = -.8, colour = text_colour),
                  plot.caption = element_text(family = font_2, size = 10, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.x = element_blank(),
                  axis.text.x = element_text(family = font_2, size = 10, colour= text_colour),
                  axis.title.y = element_text(family = font_2, size = 14, colour= text_colour),
                  axis.text.y = element_text(family = font_2, size = 10, colour= text_colour),
                  axis.ticks = element_blank(),
                  axis.line.x = element_line(colour = axis_colour, size = .1),
                  axis.line.y = element_blank(),
                  legend.position="none",
                  strip.text.x = element_text(family = font_1, size = 20, hjust = -.02, colour= text_colour),
                  strip.placement = "outside",
                  strip.background = element_blank())

theme_set(theme_classic() + theme_style)

#Set palette
cols <- c("Book One: Water" = "#2e67a0", 
          "Book Two: Earth" = "#213A27", 
          "Book Three: Fire" = "#A10000")

#Set custom y-axis labels
yLabels <- paste(c(0,2.5,5,7.5,10), "")

#Plot data
ggplot(a_data_final) +
  geom_segment(aes(chapter, xend=chapter, y=0, yend=Rating), size = 0.2, color = "gray") +
  geom_hline(aes(yintercept = Mean_Rating), linetype="dashed", colour = text_colour, size = .3) +
  geom_point(aes(chapter, y=Rating, colour = book), size=4) +
  geom_text(aes(min(a_data_final$chapter_num)+1, vjust = 1, hjust = .01,  Mean_Rating, 
                label = paste0("Average Rating: ",round(Mean_Rating, digits = 1)), family = font_2, size = 4),
            colour = "white", size = 5, nudge_y = .3) +
  scale_colour_manual(name = "book", values=cols) +
  scale_y_continuous(breaks = c(0,2.5,5,7.5,10), limits = c(0, 15), labels = yLabels) +
  coord_flip()+
  facet_wrap(~book, ncol=1, scale="free_y", strip.position = "top") +
  labs(title = "Avatar: The Last Airbender", 
       subtitle = "IMDb Chapter Ratings Per Book",
       caption = "\n\n\nVisualisation: @JaredBraggins | Source: (appa) R Package",
       y = 'Rating')


#Export plot
ggsave("Avatar.png", dpi = 700, width = 25, height = 29, units = "cm")
