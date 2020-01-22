## Tidy Tuesday Week 4 2020 - Song Genres
## Data Source: spotifyr

#Load packages
library(tidyverse)
library(extrafont)

#Import data
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

#Inpect data
head(spotify_songs)

#Check subgenres to find one to use in chart
subgenres <- spotify_songs %>%
  group_by(playlist_subgenre) %>%
  summarise() 

#Wrangle data
spotify_formatted <- spotify_songs %>% 
  separate(track_album_release_date, "track_year") %>% 
  filter(track_year >= 1965 & playlist_subgenre == "classic rock") %>% 
  mutate(Popularity = case_when(
    track_popularity >= 0 & track_popularity <= 25 ~ "0-25",
    track_popularity > 25 & track_popularity <= 50 ~ "26-50",
    track_popularity > 50 & track_popularity <= 75 ~ "51-75",
    track_popularity > 75 & track_popularity <= 100 ~ "76-100",
  )) 

#Deal with duplicate song titles
spotify_unique <- spotify_formatted %>% 
  mutate(distinct_name = paste(track_name, track_year, sep = "_")) %>% 
  distinct(distinct_name, .keep_all = TRUE) %>%
  group_by(distinct_name, Popularity, track_year) %>%
  tally() %>%
  arrange(n) %>%
  ungroup()

# Set theme
my_font <- 'Tahoma'
my_background <- '#191716'
my_textcolour <- "white"
my_axiscolour <- "white" 
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 14, colour = my_textcolour),
                  plot.subtitle = element_text(size = 11, colour = my_textcolour),
                  plot.caption = element_text(size = 9, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_text(size = 12, colour= my_axiscolour),
                  axis.text = element_text(size = 10, colour= my_axiscolour),
                  axis.ticks.y = element_line(colour = "white", size = 0.5),
                  axis.ticks.x = element_line(colour = "white", size = 0.5),
                  axis.line = element_line(colour = "white", size = 0.5),
                  panel.grid.minor = element_blank(),
                  legend.text = element_text(size = 9, colour= my_axiscolour),
                  legend.title = element_text(size = 10, colour= my_axiscolour),
                  legend.position="right")

theme_set(theme_light() + my_theme)

#Plot data (with a graphic equaliser theme)
ggplot(spotify_unique, aes(track_year, n, fill = Popularity)) +
  geom_col(color = my_background) +
  scale_fill_manual(values = c('#D62828', '#F77F00', "#ABE188", "#06D6A0")) +
  labs(y = "Number of songs", x = "Year of release", 
       title = "Popularity of Songs on Spotify",
       subtitle = "Under the 'Classic Rock' Sub-Genre",
       caption = "\nVisualisation: @JaredBraggins | Data Source: spotifyr") +
  scale_x_discrete(breaks=c(1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020), 
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

#Save chart
ggsave("Spotify_Chart.png", height = 7, width = 12, dpi = 320)
