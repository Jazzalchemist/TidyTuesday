## Tidy Tuesday Week 20 - US Broadband Access
## Data Source: Microsoft

# Load packages
library(tidyverse)
library(ggtext)
library(here)
library(janitor)
library(tigris)
library(sf)
library(albersusa)
library(zipcodeR)
library(stringr)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2021-05-11')
tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband_zip <- tuesdata$broadband_zip

# Clean data
broadband_zip <- broadband_zip %>% 
  clean_names() %>% 
  rename(state = st,
         fips = county_id) %>% 
  mutate(zipcode = postal_code,
         zipcode = case_when(zipcode < 9999 ~ str_c("0",as.character(zipcode)), 
                          TRUE ~ as.character(zipcode)),
         fips = case_when(fips < 9999 ~ str_c("0",as.character(fips)), 
                             TRUE ~ as.character(fips)),
         state = state.name[match(state, state.abb)])

# Get zipcode info
zipinfo <- zip_code_db %>%
  select(zipcode, county, state, population) %>% 
  rename(county_name = county) %>% 
  mutate(state = state.name[match(state, state.abb)])

# Join data
broadband_join <- broadband_zip %>% 
  inner_join(zipinfo %>% select(-county_name, -state), 
             by = c("zipcode"))

# Load county shapefile
county_map <- albersusa::counties_sf() %>% 
  mutate(fips = as.character(fips))

# Join shapefile and broadband data
county_map <- county_map %>% 
  geo_join(broadband_join, by_df = "fips", by_sp = "fips")

# Set theme
font_family <- 'Century Gothic'
background <- "#1C1D21"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family),
                     rect = element_rect(fill = background),
                     plot.background = element_rect(fill = background, color = NA),
                     plot.title = element_text(size = 26, colour = text_colour),
                     plot.subtitle = element_text(size = 14, colour = text_colour),
                     plot.caption = element_markdown(size = 10, colour = text_colour),
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
                     legend.title = element_blank(),
                     legend.position="top",
                     panel.spacing = unit(2, "lines"))

theme_set(theme_classic() + theme_style)

# Plot data
ggplot() +
  geom_sf(data = county_map, 
          aes(fill = broadband_usage),
          color = "#738290", 
          size = 0.05) +
   scale_fill_gradient2(low = '#00b4d8', high = '#023e8a',
                        breaks = c(.2,.4,.6,.8), 
                        labels = c("20%","40%","60%", "80%"),
                        midpoint = 0.3) +
  coord_sf(xlim = c(-130, -60), ylim = c(20, 50)) +
  labs(title = "US Broadband Availablity",
       subtitle = "Percentage of availability at a county level",
       caption = "Visualisation: @JaredBraggins | Source: The Verge/Microsoft")

# Export plot
ggsave("US_Broadband.png", dpi = 320, height = 8, width = 11)
