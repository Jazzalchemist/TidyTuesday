## Tidy Tuesday Week 43 - Beer Awards
## Data Source: Great American Beer Festival

# Load packages
library(tidyverse)
library(here)
library(ggtext)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load('2020-10-20')
tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

beer_awards <- tuesdata$beer_awards

# Inspect data
View(beer_awards)

# There are 515 unique beer types featured in the data
beer_types <- distinct(beer_awards, category) %>% 
  count(category) %>% 
  select(n) %>% 
  summarise(sum = sum(n))

# Let's narrow the focus to Ales
beer_data <- beer_awards %>% 
  filter(grepl("Ale", category))

# Next, let's group beers to get a count for each type
# NOTE: This was a ridiculous way of approaching grouping the data, 
# but I was having fun digging int the data!
Ale_types <- beer_data %>% 
  mutate(types = case_when(
    grepl('Belgian-Style Abbey', category) ~ "Belgian-Style Abbey",
    grepl('India', category) ~ "India Pale",
    grepl('American-Style Pale', category) ~ "American Pale",
    grepl('International', category) ~ "International",
    grepl('Hazy', category) ~ "Hazy",
    grepl('Red', category) ~ "Red",
    grepl('Black', category) ~ "Black",
    grepl('Brown', category) ~ "Brown",
    grepl('Cream', category) ~ "Cream",
    grepl('Sour', category) ~ "Sour",
    grepl('Blonde', category) ~ "Golden/Blonde",
    grepl('Barley', category) ~ "Barley Wine-Style",
    grepl('Summer', category) ~ "Summer",
    grepl('Wheat', category) ~ "Wheat",
    grepl('Scotch', category) ~ "Scotch",
    grepl('English', category) ~ "English",
    grepl('Australian', category) ~ "Australian-Style",
    grepl('Scottish', category) ~ "Scottish-Style",
    grepl('American-Belgo', category) ~ "American-Belgo-Style",
    grepl('Belgian- and French', category) ~ "Belgian and French-Style",
    grepl('Mild', category) ~ "Mild",
    grepl('Old', category) ~ "Old",
    grepl('Strong', category) ~ "Strong",
    grepl('Other', category) ~ "Other Belgian-Style",
    grepl('Special', category) ~ "Specialty",
    grepl('Quadrupel', category) ~ "Dark Strong/Quadrupel",
    TRUE ~ "Other")
    )

# Now to count each type and order by medal
order <- c("Gold","Silver","Bronze")

Ale_data <- Ale_types %>% 
  select(types, medal) %>% 
  count(types, medal) %>% 
  group_by(types, medal) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  mutate(types = fct_reorder(types, n, .desc = F),
  medal = factor(medal,levels=order))

# Set theme
font_family <- 'Century Gothic'
font_family2 <- 'Times New Roman'
background <- "#353531"
text_colour <- "white"
axis_colour <- "white"
plot_colour <- "black"
theme_style <- theme(text = element_text(family = font_family),
                  rect = element_rect(fill = background),
                  plot.background = element_rect(fill = background, color = NA),
                  plot.title = element_text(face = 'bold', hjust = -.6, size = 20, 
                                            colour = text_colour),
                  plot.subtitle = element_text(size = 12, hjust = 1.85, colour = text_colour),
                  plot.caption = element_markdown(size = 10, colour = text_colour),
                  panel.background = element_rect(fill = background, color = NA),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"),
                  panel.border = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(size = 10, hjust = 0, colour= text_colour, 
                                              family = font_family2, face = "italic"),
                  axis.text = element_text(size = 8, colour= text_colour),
                  axis.line.x = element_line(size = 0.2, colour = axis_colour),
                  axis.ticks = element_blank(),
                  axis.line.y = element_blank(),
                  legend.position="none",
                  strip.text.x = element_text(size = 12, hjust = 0, colour = text_colour,
                                              family = font_family2, face = "bold.italic"),
                  strip.placement = "inside",
                  strip.background = element_blank())

theme_set(theme_classic() + theme_style)

# Set colour palette
cols <- c("#C9B037","#D7D7D7","#AD8A56")

# Plot data
ggplot(Ale_data, aes(n, 
                    types, 
                    fill = medal,
                    alpha = factor(types))) +
  geom_vline(data = tibble(x = c(0, seq(20, 60, by = 20), 60)),
             aes(xintercept = x), 
             color = axis_colour, 
             size = 0.2,
             linetype="dotted") +
  geom_col() + 
  scale_fill_manual(values = cols) + 
  facet_wrap(~ medal, 
             ncol = 1) + 
  scale_x_continuous(expand = c(0, 0), sec.axis = dup_axis(name = NULL)) +
  labs(title = "India Pale Ales for the Win!",
       subtitle = "Comparing award-winning ale types at the Great American Beer Festival | 1987-2020",
       x = "No. of beers",
       caption = "**Visualisation:** @JaredBraggins | **Source:** Great American Beer Festival")

# Export plot
ggsave("Beer.png", width = 210, height = 297, dpi = 500, unit = "mm")
