## Tidy Tuesday WEEK 30 - Australian Animal Outcomes
## Data Source: RSPCA

#Load packages
library(tidyverse)
library(fmsb)
library(grid)
library(extrafont)

#Import data
animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

#Inspect data
View(animal_outcomes)

#Pivot data
dogs <- animal_outcomes %>%
  filter(year == 2018 & animal_type == "Dogs") %>% 
  pivot_longer(cols = c(ACT, NSW, NT, QLD, SA, TAS, VIC, WA), names_to = "State", values_to = "values") %>% 
  select(outcome, State, values) %>% 
  pivot_wider(names_from = outcome, values_from = values) 


#Change first column into row names 
dogs <- dogs %>% 
  remove_rownames %>% 
  column_to_rownames(var = "State")

#Add max and min values
dogs <- rbind(rep(5000,10) , rep(0,10) , dogs)

#Set theme
par(family = "Century Gothic")
background <- "white"
textcolour <- "black"
axiscolour <- "white"
plotcolour <- alpha("#284B63",0.8)
netcolour <- "#B4B8AB"

#Create titles
title <- c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA")

#Create SVG
svg("Dog Outcomes.svg", width = 10, height = 6)

#Screen setup
par(mar = rep(1,4))
par(mfrow = c(3,4))

#Plot data
for(i in 1:8){
  radarchart(dogs[c(1,2,i+2),],
             #polygon
             pcol=plotcolour, pfcol = plotcolour, paxislabels = 2,
             #grid
             cglcol = netcolour, cglwd=0.5, cglty = 1,
             #labels
             vlcex=0.8, title = title[i])
  }
dev.off()

