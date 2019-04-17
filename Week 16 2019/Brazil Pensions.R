# Load libraries
library(tidyverse)
library(scales)

# Import data
pensions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-16/pensions.csv")

# Set chart theme
theme_set(theme_light())

# Filter countries for colours/labels
Brazil <- pensions %>% 
  filter(country == "Brazil")

Labels <- pensions %>% 
  filter(country %in% c("France", "Japan", "Italy", "Mexico", "South Korea", "Turkey", 
                        "United States", "Greece", "Poland"))

# Calculate OECD average
OECD = pensions %>% 
  summarise(pop_65_percent = mean(pop_65_percent), 
            gov_spend_percent_gdp = mean(gov_spend_percent_gdp))

# Create scatterplot
pensions %>% 
  ggplot(aes(pop_65_percent, gov_spend_percent_gdp)) +
  geom_point(color = "#2DC0D2", size = 3, alpha = 1) +
  geom_point(aes(pop_65_percent, gov_spend_percent_gdp), 
             data = Brazil, color =  "black", pch = 21, size = 4) +
  geom_point(aes(pop_65_percent, gov_spend_percent_gdp),
             data = Labels, color =  "black", pch = 21, size = 4) +
  geom_point(aes(pop_65_percent, gov_spend_percent_gdp),
             fill = "#2DC0D2", data = OECD, color =  "black", pch = 21, size = 4) +
  geom_text(aes(pop_65_percent, gov_spend_percent_gdp, label = country), 
            data = Brazil, color = "black", fontface = "bold", size = 3.5, nudge_y = 1, nudge_x = -1.5) +
  geom_text(aes(pop_65_percent, gov_spend_percent_gdp, label = country), 
            data = Labels, color = "black", size = 3, nudge_y = 1, nudge_x = -1.5) +
  geom_text(aes(pop_65_percent, gov_spend_percent_gdp, label = "OECD average"), 
            data = OECD, color = "black", fontface = "italic", size = 3, nudge_y = 1, nudge_x = -1.5) +
  scale_y_continuous(limits = c(0,20)) +
  scale_x_continuous(limits = c(0,35)) +
  labs(title = "Brazil's golden oldie blowout",
       subtitle = "Latest Available",
       x = "Population aged 65 years and over, % of total",
       y = "Government spending\non pension benefits\n% of GDP",
       caption = "Sources: OECD; World Bank; Previdencia Social") +
  theme(axis.title.y = element_text(size = 10, colour="grey15", vjust = 1, hjust = 0, angle = 0),
        axis.title.x = element_text(size = 10, colour="grey15"),
        plot.title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=11,colour="grey15"),
        plot.caption = element_text(size=10,colour="grey38"),
        plot.background = element_rect(fill = "#D9E9F0"),
        panel.background = element_rect(fill = "#D9E9F0"))

# Save chart as png
aspect_ratio <- 2
ggsave("pension.png", height = 5 , width = 3 * aspect_ratio)
