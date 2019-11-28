# Load packages
library(tidyverse)
library(tidyr)
library(scales)
library(extrafont)

# Import data
tuesdata <- tidytuesdayR::tt_load("2019-11-26")
tuesdata <- tidytuesdayR::tt_load(2019, week = 48)

loans <- tuesdata$loans

# Inspect data
View(loans)

# Wrangle data
loans2 <- loans %>% 
  filter(year == 18 & !is.na(rehabilitation)) %>% 
  select(consolidation, rehabilitation, voluntary_payments, wage_garnishments) %>% 
  summarise_all(funs(sum))

# Transpose data
loans3 <- loans2 %>% 
  gather(Repayment, Sum, rehabilitation, consolidation, wage_garnishments, voluntary_payments)

# Tidy repayment names
loans3<- gather(loans3, Repayment, Sum) %>%
  mutate_if(is.character, 
            str_replace_all, pattern = "voluntary_payments", replacement = "Voluntary Payments") %>%
  mutate_if(is.character, 
            str_replace_all, pattern = "wage_garnishments", replacement = "Wage Garnishments") %>%
  mutate_if(is.character, 
            str_replace_all, pattern = "consolidation", replacement = "Consolidation") %>%  
  mutate_if(is.character, 
            str_replace_all, pattern = "rehabilitation", replacement = "Rehabilitation")

# Compute percentages, top of each rectangle, bottom of each rectangle, label position and labels
loans4 <- loans3 %>% 
  mutate(fraction = Sum / sum(Sum),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin) / 2,
         label = percent(fraction %>% round(2))) %>% 

# Reorder legend
loans4$Repayment <- factor(loans4$Repayment, levels = c("Rehabilitation", "Consolidation", 
                                                        "Wage Garnishments", "Voluntary Payments"))
# Calculate Total
total <- loans %>% 
  filter(year == 18 & !is.na(rehabilitation)) %>% 
  select(total) %>% 
  summarise_all(funs(sum))

# Set theme
my_font <- 'Century Gothic'
my_background <- 'gray95'
my_textcolour <- "gray17"
my_axiscolour <- "black" 

my_theme <- theme(text = element_text(family = my_font),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  plot.title = element_text(face = 'bold', size = 20, colour = my_textcolour),
                  plot.subtitle = element_text(size = 14),
                  plot.caption = element_text(size = 10, hjust = -2),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title = element_blank(),
                  axis.text =  element_blank(),
                  axis.ticks = element_blank(),
                  axis.line = element_blank(),
                  legend.title = element_text(face = 'bold', size = 14),
                  legend.text = element_text(size = 12),
                  legend.position="right")

pal <- paletteer_c(viridis, "viridis", n = 5)
theme_set(theme_light() + my_theme)

# Plot data
ggplot(loans4, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill = Repayment)) +
  geom_rect(colour = "white") +
  geom_label( x=4.8, aes(y=labelPosition, label = label), size=5, label.size = NA, fill = NA) +
  coord_polar(theta="y") + 
  xlim(c(-1, 4)) +
  scale_fill_brewer(palette="Set2") +
  scale_color_brewer(palette="Set2") +
  labs(title = "U.S. Student Debt Repayments in 2018",
       subtitle = paste0("Total Annual Student Debt: $", format(round(total / 1e9, 1), trim = TRUE), "B"),
       caption = "Visualisation: @JaredBraggins | Source: U.S. Department of Education")

ggsave("Student_Debt.png", width = 7.07, height = 5.58)
