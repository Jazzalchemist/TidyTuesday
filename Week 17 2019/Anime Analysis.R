# Load packages
library(tidyverse)
library(gganimate)
library(lubridate)

# Import data
anime <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv") %>% 
  mutate(start_date = ymd(as.character(start_date))) %>% 
  mutate(start_year = year(start_date))

# Inspect data
View(anime)


# Filter all series that have finished airing, but have aired since 1998. Remove all films.
anime_filtered <- 
  anime %>% 
  select(title_english, type, score, start_date, start_year, status) %>%
  filter(status == "Finished Airing" & 
           type == "TV" & 
           start_year >= 1998 &
           !is.na(title_english)) %>% 
  arrange(desc(score)) %>% 
  distinct(title_english, .keep_all = TRUE)

anime_filtered

# Plot top 20 to see where Cowboy Bebop places
anime_filtered %>% 
  top_n(20, score) %>% 
  mutate(title_english = fct_reorder(title_english, score)) %>% 
  ggplot(aes(x = title_english, y = score)) +
  geom_col() +
  coord_flip()

# Create loop to assign rank per year for all shows
for (i in 1998:2018) {
  anime_temp <- anime_filtered %>%
    top_n(20, score)
  anime_filtered <- anime_filtered %>% 
    bind_rows(anime_temp)
}

# Add frame ids for transitions
anime_final <- anime_filtered %>% 
  mutate(., frame_id = group_indices(., start_year)) %>% 
  ungroup()

anime_final

# Plot top 20 where shows began airing from 1998 to see where Cowboy Bebop places


anime_race <- 
  anime_final %>% 
  top_n(20, score) %>% 
  mutate(title_english = fct_reorder(title_english, score)) %>% 
  ggplot(aes(x = title_english, y = score)) +
  geom_col() +
  coord_flip() +
transition_states(frame_id, transition_length = 4, state_length = 3) +
  ease_aes('cubic-in-out')

animate(anime_race, nframes = 750, fps = 25, end_pause = 50, width = 400, height = 266, res = 80, detail = 3)

# NOTE: There needs to be a frame_id applied to each of the top 20 shows per year 
