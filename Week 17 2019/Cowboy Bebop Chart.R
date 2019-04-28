# Load packages
library(tidyverse)
library(lubridate)
library(stringr)


# Import data
anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv") %>% 
  mutate(start_date = ymd(as.character(start_date))) %>% 
  mutate(start_year = year(start_date))


# Inspect data
View(anime)


# Filter all series that aired in 1998. Remove all films.
anime_filtered <- 
  anime %>% 
  select(title_english, type, score, start_date, start_year, status) %>%
  filter(status == "Finished Airing" & 
           type == "TV" & 
           start_year == 1998 &
           !is.na(title_english)) %>% 
  mutate(score = round(score, 1)) %>% 
  arrange(desc(score)) %>% 
  distinct(title_english, .keep_all = TRUE)


# Wrap long title names
anime_filtered$title_english = str_wrap(anime_filtered$title_english, width = 36)


# Set formatting for chart
my_font <- 'Bookman Old Style'
my_background <- '#191919'
my_palette <- c('#CCD0CF', '#EFCC77')
my_titlecolour <- '#EFCC77'
my_textcolour <- 'white'
my_labelcolour <- 'black'
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  plot.title = element_text(face = 'bold', size = 14, colour = my_titlecolour),
                  plot.subtitle = element_text(size = 12, colour = my_textcolour),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = 'none',
                  plot.caption = element_text(size = 9, colour = my_textcolour),
                  axis.ticks = element_blank(),
                  axis.text.x = element_blank(),
                  axis.text.y = element_text(size = 6, colour = my_textcolour))


theme_set(theme_light() + my_theme)

# Plot top 10 to see where Cowboy Bebop places
anime_filtered %>% 
  top_n(10, score) %>% 
  mutate(title_english = fct_reorder(title_english, score)) %>% 
  mutate(highlight_flag = ifelse(title_english == "Cowboy Bebop",T,F)) %>%
  ggplot(aes(title_english, score, fill = highlight_flag)) +
  geom_col() +
  geom_text(aes(label=score), hjust = 1.5, color = my_labelcolour) +
  scale_fill_manual(values = c(my_palette)) +
  coord_flip() +
  labs(title = "Cowboy Bebop Had the Highest Score",
       subtitle = "Of the top ten shows that aired in 1998",
       caption = "Data source: MyAnimeList",
       y = "",
       x = "")

ggsave(
  filename = 'CowboyBebop.png',
  height = 8,
  width = 15,
  units = 'cm',
  dpi = 'retina'
)
