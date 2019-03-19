library(tidyverse)
library(scales)

# import data
combined_data <- read.csv("https://raw.githubusercontent.com/5harad/openpolicing/master/results/data_for_figures/combined_data.csv")


# Convert abbr to state name  (Credit to Jerrick Tram for this great solution)
names(combined_data)[2] <- "region"
combined_data$region <- str_to_lower(state.name[match(combined_data$region, state.abb)])


# Plot number of stops per ethnicity per state 
combined_data %>%
group_by(region, driver_race) %>%
summarise(n = sum(stops_per_year)) %>%
arrange(-n) %>%
mutate(highlight_flag = ifelse(driver_race == 'White', T, F)) %>%
ggplot(aes(x=reorder(driver_race, n), y=n, fill = highlight_flag)) + 
scale_fill_manual(values = c('#595959', 
                              'orange2')) +
geom_bar(stat="identity") +
theme_classic() +
theme(legend.position = "none",
      text = element_text(size = 10),
      plot.caption=element_text(hjust=1,size=9,colour="grey30"),
      plot.subtitle=element_text(face="italic",size=11,colour="grey40"),
      plot.title=element_text(size=14,face="bold")) +
labs( title = " Annual Stops by Law Enforcement",
      subtitle = "Grouped By Driver Race",
      x = "Driver Race",
      y = "Stops",
      caption = "Data Source: Stanford Open Policing Project") +
scale_y_continuous(labels = comma) +
coord_flip() +
facet_wrap(~region, ncol = 3)  +
theme(strip.text.x = element_text(size=8),
      strip.text.y = element_text(size=10, face="bold"),
      strip.background = element_rect(colour="white", 
                                  fill="gray96"))


# Save image of chart
ggsave(
  filename = 'Annual_Stops.png',
  height = 20,
  width = 20,
  units = 'cm',
  dpi = 600
)
