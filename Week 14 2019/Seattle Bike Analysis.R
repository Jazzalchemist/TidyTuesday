
# Load packages
library(tidyverse)
library(scales)
library(lubridate)

# Import data and tidy dates
bt <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv",
               col_types = "cccdd") %>% 
  mutate(date = mdy_hms(date))

# Glimpse data
glimpse(bt)

# Plot the total number of bikes by month
bt  %>%
  select(date, bike_count) %>%
  filter(!is.na(bike_count)) %>%
  mutate(month = as.Date(floor_date(date, "month"))) %>%
  group_by(month) %>%
  summarise(total_bike_count = sum(bike_count)) %>%
  ggplot(aes(x = month, y = total_bike_count/1000)) +
  geom_line(color = 'springgreen4') +
  geom_area(fill = 'springgreen3', alpha = .1) +
  scale_x_date(labels = date_format("%b %Y"), 
               name = "Month", 
               date_breaks = "6 months") +
  theme_classic() +
  labs(y = "Bike Count (Thousands)",
       title = "Seattle Monthly Bike Traffic",
       subtitle = "December 2013 - February 2019",
       caption = "Data source: Seattle Department of Transportation | By @JaredBraggins") +
  theme(plot.title=element_text(size=20,face="bold"),
        plot.subtitle=element_text(face="italic",size=14),
        axis.text = element_text(size = 12),
        text=element_text(size=12,  family="Century Gothic"),
        panel.grid.major.y = element_line(size = .1, color = "gray90"))


# Save image of monthly chart
ggsave(
  filename = 'Seattle_Monthly_Bike.png',
  height = 20,
  width = 29,
  units = 'cm',
  dpi = 'retina'
)

# Plot bike traffic per stop for each year
bt  %>%
  select(date, bike_count, crossing) %>%
  filter(!is.na(bike_count)) %>%
  mutate(month = as.Date(floor_date(date, "month"))) %>%
  group_by(month, crossing) %>%
  summarise(total_bike_count = sum(bike_count)) %>%
  ungroup() %>%
  ggplot(aes(x = month, y = total_bike_count/1000, color = as.factor(crossing))) +
  geom_line(size = 0.75) +
  scale_x_date(labels = date_format("%b %Y"), 
               name = "Month", 
               date_breaks = "6 months") +
  scale_color_manual(values=c("#D3D3D3", "#D3D3D3", "#008080", "#D3D3D3",
                              "#D3D3D3", "#D3D3D3", 
                              "#D3D3D3")) +
  theme_classic() +
  annotate("text", 
           x = as.Date("2013-12-25"), 
           y = 65, 
           label = "The Burke Gilman Trail saw the most bike traffic\nover Summer each year",
           hjust = 0, size = 5, family = "Century Gothic", color = '#008080') +
  labs(y = "Bike Count (Thousands)",
       title = "Seattle Monthly Bike Traffic By Crossing",
       subtitle = "December 2013 - February 2019",
       caption = "Data source: Seattle Department of Transportation | By @JaredBraggins") +
  theme(plot.title=element_text(size=20,face="bold"),
        plot.subtitle=element_text(face="italic",size=14),
        legend.position="none",
        legend.title = element_blank(),
        axis.text = element_text(size = 12),
        text=element_text(size=12,  family="Century Gothic"),
        panel.grid.major.y = element_line(size = .1, color = "gray90"))

# Save image of monthly crossing chart
ggsave(
  filename = 'Seattle_Monthly_Crossing.png',
  height = 20,
  width = 29,
  units = 'cm',
  dpi = 'retina'
)
