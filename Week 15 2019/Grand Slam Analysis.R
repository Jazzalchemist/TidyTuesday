# Load packages
library(tidyverse)
library(scales)

# Import data
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# Add a case statement for court types
grand_slams <-
  grand_slams %>%
  mutate(court_type = case_when(grand_slam == "australian_open" ~ "Hard Court",
                              grand_slam == "us_open" ~ "Hard Court",
                              grand_slam == "french_open" ~ "Clay Court",
                              grand_slam == "wimbledon" ~ "Grass Court"))

# Set theme for charts
theme_set(theme_classic())

# Plot Top Ten Winners
grand_slams %>%
count(name, court_type, sort = TRUE) %>%
add_count(name, wt = n) %>%
filter(n >=5) %>% 
mutate(name = fct_reorder(name, n, sum)) %>% 
ggplot(aes(name, n, fill = court_type)) +
geom_col() +
scale_fill_manual(values = c('#F6BD60', '#7FB069', '#548687')) +
coord_flip() +
labs(x = "", 
     y = "No. of Grand Slam Wins",
     title = "Top Grand Slam Winners By Court Type",
     subtitle = "1968 - 2019") +
theme(legend.position="top",
      legend.title = element_blank(),
      legend.spacing.x = unit(0.2, 'cm'),
      plot.title=element_text(size=12,face="bold"),
      plot.subtitle=element_text(face="italic",size=11,colour="grey40"))

aspect_ratio <- 2
ggsave("top_players_court.png", height = 5 , width = 5 * aspect_ratio)

# Calculate age
age <-
  player_dob %>%
  select(name, date_of_birth) %>% 
  inner_join(grand_slams, by = "name") %>%
  mutate(age = as.numeric(difftime(tournament_date, date_of_birth, unit = "days"))/365)

# Boxplot of player ages by tournament
age %>% 
  mutate(grand_slam = str_to_title(str_replace(grand_slam, "_", " "))) %>% 
  ggplot(aes(grand_slam, age, fill = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c('#8700F9', '#00C4AA')) +
  labs(x = "Grand Slam", 
       y = "Age",
       title = "Distribution of Age By Grand Slam") +
  theme(legend.position="top",
        legend.title = element_blank(),
        legend.spacing.x = unit(0.2, 'cm'),
        plot.title=element_text(size=12,face="bold"))

aspect_ratio <- 2
ggsave("grand_slams_age_distribution.png", height = 5 , width = 5 * aspect_ratio)
