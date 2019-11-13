# Load libraries
library(tidyverse)
library(gridExtra)
library(extrafont)

# Load data
tuesdata <- tidytuesdayR::tt_load("2019-11-12")
tuesdata <- tidytuesdayR::tt_load(2019, week = 46)

cran_code <- tuesdata$loc_cran_packages

# Inspect data
View(cran_code)

# Group top 10 languages by sum of comments
top_10_comments <- cran_code %>% 
  group_by(language) %>%
  summarise(total_comments = sum(comment)) %>%
  top_n(10) %>%
  mutate(language = fct_reorder(language, total_comments))

# Group top 10 languages by lines of code
top_10_code <- cran_code %>% 
  group_by(language) %>%
  summarise(total_code = sum(code)) %>%
  top_n(10) %>%
  mutate(language = fct_reorder(language, total_code))

# Set theme
my_font <- 'Century Gothic'
my_background <- '#1C1F26'
my_titlecolour <- "white"
my_textcolour <- "gray70"
my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 15, colour = my_titlecolour),
                  plot.subtitle = element_text(size = 11, colour = my_textcolour),
                  plot.caption = element_text(size = 10, colour = my_textcolour),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text.y = element_text(size = 10, colour= my_textcolour),
                  axis.text.x = element_text(size = 10, colour = my_textcolour),
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_line(size = 0.5, colour = my_textcolour),
                  axis.line.x = element_line(colour = "white", size = 0.5, linetype = "solid"),
                  legend.position="none")

theme_set(theme_light() + my_theme)

# Plot by comments
p1 <- top_10_comments %>%
  mutate(highlight_flag = ifelse(language == "R",T,F)) %>%
  ggplot(aes(x = language, y = total_comments, fill = highlight_flag)) +
  geom_col() + 
  scale_fill_manual(values = c('grey65', '#81A4CD')) +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(title = "Top 10 Languages Used in R Packages",
       caption = "\n",
       subtitle = "Grouped by total number of comments per package")

# Plot by lines of code
p2 <- top_10_code %>%
  mutate(highlight_flag = ifelse(language == "R",T,F)) %>%
  ggplot(aes(x = language, y = total_code, fill = highlight_flag)) +
  geom_col() + 
  scale_fill_manual(values = c('grey65', '#FE4E00')) +
  scale_y_continuous(labels = scales::comma, breaks=seq(0,20000000,10000000)) +
  coord_flip() +
  labs(title = " ",
       subtitle = "Grouped by total lines of code per package",
       caption = "\nVisualisation: @JaredBraggins | Data Source: CRAN Packages")

# Merge plots
grid.arrange(p1, p2, nrow = 1)

# Save png
g <- arrangeGrob(p1, p2, nrow=1)
ggsave(file = "CRAN.png", g)
