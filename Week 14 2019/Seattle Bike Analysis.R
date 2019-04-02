
## 1. SET UP

# Load packages
library(tidyverse)
library(lubridate)

# Import data
bt <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-02/bike_traffic.csv")


## 2. EXPLORE DATA

# Glimpse data
glimpse(bt)

# Sum the total number of bikes in the dataset
sum(bt$bike_count,na.rm=TRUE)

# Check the latest date in the dataset
max(bt$date)


## 3. TIDY DATE COLUMN

# Convert time in 'date' to 24 hours. Separate 'date' into time and date columns
bt$date <- mdy_hms(bt$date)
bt$dates <- as.Date(bt$date,"%m/%d/%Y")
bt$time <- as.POSIXct(bt$date, "%m/%d/%Y %H:%M:%S", tz = "")


## 4. INVESTIGATE TOTAL BIKES OVER A ONE YEAR PERIOD

# Sum the total number of bikes per year
bt %>%
  select(dates, bike_count) %>%
  group_by(year=floor_date(dates, "$Y")) %>%
  arrange(dates) %>%
  filter(!is.na(bike_count)) %>%
  summarise(total_bike_count = sum(bike_count))

# Plot the total number of bikes over the year
bt  %>%
  group_by(year=floor_date(dates, "$M")) %>%
  filter(dates >= as.Date("02/01/2018")) %>%
  ggplot(aes(x = dates, y = bike_count)) +
  geom_line()

# Plot the total number of bikes per crossing over the year
bt_crossing <- 
  bt %>%
  select(crossing, bike_count) %>%
  group_by(crossing) %>%
  filter(!is.na(bike_count)) %>%
  summarise(total_bikes = sum(bike_count))

bt_crossing %>%
  ggplot(aes(x = crossing, y = total_bikes)) +
  geom_col() +
  theme_classic()+
  coord_flip()


## 5. INVESTIGATE AVERAGE NUMBER OF BIKES

# Plot the average number of bikes per day


# Plot peak times for cyclists

