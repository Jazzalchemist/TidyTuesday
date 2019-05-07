# Load packages
library(tidyverse)

# Import data
student_ratio <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")

# View data
View(student_ratio)

# Group countries and calculate mean
ratio_grouped <-student_ratio %>%
  group_by(country) %>%
  mutate(mean = mean(student_ratio, na.rm = TRUE))

