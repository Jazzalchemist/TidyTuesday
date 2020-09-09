## Tidy Tuesday Week 37 - Friends
## Data Source: friends R package

#Helpful links
#https://medium.com/@liztersahakyan/the-one-with-the-data-scientist-a-closer-look-at-the-friends-of-friends-d3530d1902af

#Load packages
library(tidyverse)
library(ggdist)
library(patchwork)
library(friends)
library(extrafont)


#Import data
tuesdata <- tidytuesdayR::tt_load('2020-09-08')
tuesdata <- tidytuesdayR::tt_load(2020, week = 37)

friends_emotions <- tuesdata$friends_emotions

#Inspect data
View(friends_emotions)

#Wrangle data
f_data <- friends_emotions %>% 
  group_by(season) %>% 
  count(emotion) %>% 
  mutate(emotion = fct_reorder(emotion, n, .desc=F))

#Wrangle data
f_data <- friends_emotions %>% 
  group_by(season) %>% 
  count(emotion) %>% 
  mutate(highlight = 
           ifelse(n > 0, 
                  ifelse(season == 1 & emotion == "Neutral", 1,
                  ifelse(season == 1 & emotion == "Joyful", 2, 
                  ifelse(season == 2 & emotion == "Scared", 3, 
                  ifelse(season == 2 & emotion == "Peaceful", 7, 
                  ifelse(season == 3 & emotion == "Joyful", 2, 
                  ifelse(season == 3 & emotion == "Mad", 4, 
                  ifelse(season == 4 & emotion == "Joyful", 2, 
                  ifelse(season == 4 & emotion == "Mad", 4, 
                  ifelse(season == 4 & emotion == "Powerful", 6, 0))))))))))) %>% 
  ungroup() %>% 
  arrange(season, desc(n), emotion) %>% 
  mutate(emotion = reorder(emotion, n))

f_data2 <- f_data %>% 
  select(emotion, n) %>% 
  group_by(emotion) %>% 
  summarise(n = sum(n)) %>% 
  mutate(emotion = fct_reorder(emotion, n, .desc=F))  
  
#Create locations in the facet for text
S1_text <- data.frame(emotion = "Neutral", n = 1340,
                      season = factor(1 ,levels = c(1, 2, 3, 4)))
S2_text <- data.frame(emotion = "Neutral", n = 1310,
                      season = factor(2 ,levels = c(1, 2, 3, 4)))
S3_text <- data.frame(emotion = "Neutral", n = 1320,
                      season = factor(3 ,levels = c(1, 2, 3, 4)))
S4_text <- data.frame(emotion = "Neutral", n = 1310,
                      season = factor(4 ,levels = c(1, 2, 3, 4)))

#Set theme
font_family1 <- "Josef Xuereb's Friends"
font_family2 <- 'Century Gothic'
background <- "#1D1D1D"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family2),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(font_family1, face = 'bold', size = 36, colour = text_colour),
                  plot.subtitle = element_text(size = 16, colour = text_colour),
                  plot.caption = element_text(size = 10, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  panel.border = element_blank(),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.position="none",
                  strip.text.x = element_text(family =font_family1, face = 'bold', size = 20, hjust = -.008, colour= text_colour),
                  strip.placement = "inside",
                  strip.background = element_blank())

theme_set(theme_classic() + theme_style)

#Add colour palettes
cols = c(Powerful = "#7B506F", Peaceful = "#70C1B3", Scared = "#247BA0",
         Sad = "#BFDBF7", Joyful = "#FFE066", Mad = "#F25F5C", Neutral = "#9E9898")

cols2 = c(`0` = "#3D3D3D", `1` = "#9E9898", `2` = "#FFE066", `3` = "#247BA0", 
          `4` = "#F25F5C", `6` = "#7B506F", `7` = "#70C1B3")

#Plot all seasons
p1 <- f_data2 %>% 
  ggplot(aes(emotion, n, fill = emotion)) +
  geom_col() +
  scale_fill_manual(values = cols) +
  scale_y_continuous(limits = c(0, 4500), breaks = seq(0, 3500, 500)) +
  geom_hline(data = tibble(y = c(0, seq(0, 3500, by = 500), 3500)),
             aes(yintercept = y), color = axis_colour, size = .3, linetype="dotted") +
  geom_text(aes(label = paste(emotion, " "), y = 0), family = font_family2, color = text_colour, hjust = 1) +
  theme(axis.text.x = element_text(size = 10, colour = text_colour)) +
  coord_polar(theta = "y", start = 0, clip = "off")


#Plot each season
p2 <- ggplot(f_data, aes(emotion, n, fill = factor(highlight))) +
  geom_col() +
  scale_fill_manual(values = cols2) +
  scale_color_manual(values = cols2) +
  scale_y_continuous(limits = c(0, 1500), breaks = seq(0, 1500, 500)) +
  coord_polar(theta = "y", start = 0, clip = "off") +
  facet_wrap(~ season, 
             labeller = labeller(season = 
                                   c("1" = "season One",
                                     "2" = "season Two",
                                     "3" = "season Three",
                                     "4" = "season Four")))

#Add text to facet
p2 <- p2 +
  geom_label(data = S1_text, label = "Neutral was the\nlargest emotion,\nfollowed by Joyful",
            size = 3, color = text_colour, family = font_family2, hjust = "center", fill = background) +
  geom_label(data = S2_text, label = "More emotional\nrange in S2. Both\nScared and Peaceful\nincreased on S1",
            size = 3, color = text_colour, family = font_family2, hjust = "center", fill = background) +
  geom_label(data = S3_text, label = "Both Joyful and Mad\nsaw increases when\ncompared to S2",
            size = 3, color = text_colour, family = font_family2, hjust = "center", fill = background) +
  geom_label(data = S4_text, label = "Joyful was the largest\nemotion, and Mad\nsaw a decrease on\nS3. Powerful was\ngradually growing\nsince S1",
            size = 3, color = text_colour, family = font_family2, hjust = "center", fill = background)


#Combine charts
final <- (p1 + p2) +
  plot_annotation(title = "The \xb7 One \xb7 with \xb7 the \xb7 Emotions",
                  subtitle = "Analysing emotions displayed across the first four seasons of Friends",
                  caption = "Visualisation: @JaredBraggins | Sources: friends R package")

#Export plot
ggsave("Friends.png", width = 15, height = 9)
