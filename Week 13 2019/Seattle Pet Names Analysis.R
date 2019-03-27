library(tidyverse)

seattle_pets <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

# Inspect data
View(seattle_pets)

# Find top ten names for all pets
pet_names <-
  seattle_pets %>% 
  filter(!is.na(animals_name)) %>%
  group_by(animals_name) %>%
  summarise(count=n()) %>%
  top_n(n=10, count)

# Reorder names by count from largest to smallest
pet_names$animals_name <- factor(pet_names$animals_name) %>%
  fct_reorder(pet_names$count)

# Plot Pet Names
pet_names %>%
  mutate(highlight_flag = ifelse(animals_name == "Lucy",T,F)) %>%
  ggplot(aes(x = animals_name, y = count, fill = highlight_flag)) +
  geom_col() + 
  scale_fill_manual(values = c('grey65', 'skyblue4')) +
  theme_classic() +
  labs(title = "Top Ten Pet Names in Seattle",
       subtitle = "For Pets Registered in 2018",
       x = "",
       y = "",
       caption = "Data source: seattle.gov") +
  coord_flip() +
  geom_text(aes(label=count), hjust = 1.5, color = 'white') +
  theme(legend.position = "none",
        text = element_text(size = 10),
        plot.caption=element_text(hjust=1,size=9,colour="grey30"),
        plot.subtitle=element_text(face="italic",size=11,colour="grey40"),
        plot.title=element_text(size=14,face="bold"),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        axis.text.x = element_blank())
