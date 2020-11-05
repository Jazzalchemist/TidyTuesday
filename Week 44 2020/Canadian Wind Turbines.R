## Tidy Tuesday Week 44 - Canadian Wind Turbines
## Data Source: open.canada.ca

# Load packages
library(tidyverse)
library(extrafont)
library(patchwork)
library(scales)
library(here)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

# Import data
tuesdata <- tidytuesdayR::tt_load('2020-10-27')
tuesdata <- tidytuesdayR::tt_load(2020, week = 44)

turbine <- tuesdata$`wind-turbine`

# Inspect data
View(turbine)

# Map of Canada
canada <- ne_states(country = "canada", returnclass = "sf")

# Turbine count
t_count <- turbine %>% 
  count(province_territory, turbine_identifier, sort = T) %>% 
  group_by(province_territory) %>% 
  summarise(turbines = sum(n)) %>% 
  mutate_if(is.character, str_to_upper)

# Dataframe for province labels
provinces <- data.frame(
                       province_territory = c("ONTARIO", "QUEBEC", "ALBERTA", "BRITISH COLUMBIA", 
                                    "MANITOBA", "NEW BRUNSWICK", "SASKATCHEWAN", "NOVA SCOTIA",
                                    "PRINCE EDWARD ISLAND", "NEWFOUNDLAND AND LABRADOR", "YUKON",
                                    "NORTHWEST TERRITORIES"), 
                       lat = c(50.000000, 53.000000, 55.000000, 53.726669, 
                               56.415211,	46.498390, 55.000000,	45.000000,
                               46.250000, 53.135509, 63.6333308, 64.2666656), 
                       lng = c(-85.000000, -70.000000, -115.000000, -127.647621, 
                               -98.739075, -66.159668, -106.000000, -63.000000,
                               -63.000000, -57.660435, -135.7666636, -119.1833326),
                       y_label = c(60, 63, 45, 42, 
                               45, 42, 42, 42,
                               50, 58, 75, 80), 
                       x_label = c(-87, -55, -115, -130, 
                               -93, -67, -102, -42,
                               -35, -35, -136, -125))

# join turbine count and province labels
province_data <- inner_join(provinces, t_count, by = "province_territory")

# adjustment parameter for labels
text_adj <- 0.4

# energy output dataframe
output <- turbine %>%
  group_by(province_territory,.drop = FALSE) %>%
  summarise(capacity = round(sum(total_project_capacity_mw)),2) %>% 
  mutate(label_pos = case_when(
    province_territory == "Ontario" ~ 12 + text_adj,
    province_territory == "Quebec" ~ 11 + text_adj,
    province_territory == "Alberta" ~ 10 + text_adj,
    province_territory == "British Columbia" ~ 9 + text_adj,
    province_territory == "Manitoba" ~ 8 + text_adj,
    province_territory == "New Brunswick" ~ 7 + text_adj,
    province_territory == "Saskatchewan" ~ 6 + text_adj,
    province_territory == "Nova Scotia" ~ 5 + text_adj,
    province_territory == "Prince Edward Island" ~ 4 + text_adj,
    province_territory == "Newfoundland and Labrador" ~ 3 + text_adj,
    province_territory == "Northwest Territories" ~ 2 + text_adj,
    province_territory == "Yukon" ~ 1 + text_adj,
  )) %>% 
  mutate(province_territory = fct_reorder(province_territory, capacity))

# Convert province name from factor to character
output$Province <-as.character(output$province_territory)

# Make province names uppercase
output <- output %>% 
  mutate_if(is.character, str_to_upper)

#Set theme
font_family <- 'Century Gothic'
background <- "white"
text_colour <- "#181716"
text_colour_2 <- "#114E5C"
axis_colour <- "white"
plot_colour <- "#166475"
theme_style <- theme(text = element_text(family = font_family),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.caption = element_text(size = 13, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title = element_blank(),
                  axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.text = element_text(size = 8, colour= text_colour),
                  legend.title = element_text(size = 10, colour= text_colour),
                  legend.position="left")

theme_set(theme_classic() + theme_style)

bold_col <- crayon::bold

# Plot map
p1 <- ggplot() +
  geom_sf(data = canada, 
          aes(geometry = geometry)) +
  geom_point(data = turbine, 
             aes(x = longitude, 
                 y = latitude), 
             col = plot_colour,
             size = 1) +
  geom_text(data = province_data, 
                  aes(x = x_label, 
                      y = y_label, 
                      label = province_territory),
                  size = 3.5) +
  geom_text(data = province_data, 
            aes(x = x_label, 
                y = y_label - 1.5, 
                label = turbines),
            col = text_colour_2,
            size = 4.5) +
  geom_segment(data = province_data, 
               aes(x = lng,
                   xend = x_label,
                   y = lat,
                   yend = ifelse(y_label > 45, y_label - 3, y_label + 1)),
               size = .3, 
               color="black", 
               linetype = 2) +
  theme(plot.title = element_text(size = 14, 
                                  colour = text_colour_2),
        plot.subtitle = element_text(size = 10, 
                                     colour = text_colour)) +
  scale_x_continuous(limits = c(-140, -15)) +
  labs(title = "NUMBER OF TURBINES")

# Plot output by province
p2 <- ggplot(output, aes(capacity, province_territory)) +
  geom_col(width = 0.3, fill = plot_colour) +
  geom_text(data = output, 
            aes(y = label_pos, 
                x = 0, 
                label = Province),
            color = text_colour,
            hjust = "left",
            size = 3.5) +
  geom_text(data = output, aes(y = label_pos - 0.4, 
                               x = capacity, 
                               label = comma(capacity)),
            color = text_colour_2,
            hjust = -0.1,
            size = 3) +
  scale_x_continuous(limits = c(0, 400000)) +
  theme(plot.title = element_text(size = 14, 
                                  colour = text_colour_2),
        plot.subtitle = element_text(size = 10, 
                                     colour = text_colour)) +
  labs(title = "ENERGY CAPACITY",
       subtitle = "CUMULATIVE TURBINE PROJECT CAPACITY [MW]\n")

# Add plots together
p1 + p2 + plot_layout(widths = c(3, 1)) +
  plot_annotation(title = "CANADIAN WIND TURBINES",
                  subtitle = "TOTAL NUMBER OF TURBINES AND CUMULATIVE ENERGY CAPACITY BY PROVINCE/TERRITORY",
                  caption = "Visualisation: @JaredBraggins | Source: open.canada.ca",
  theme = theme(plot.title = element_text(size = 20, hjust = 0.5, colour = text_colour_2),
                plot.subtitle = element_text(size = 12, hjust = 0.5, colour = text_colour), 
                plot.caption = element_text(size = 10, hjust = 0.5, colour = text_colour)))

#Export plot
ggsave("Turbines.png", width = 15, height = 9)
