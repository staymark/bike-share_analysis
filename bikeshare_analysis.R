# data
# https://divvy-tripdata.s3.amazonaws.com/index.html
# 202207-divvy-tripdata to 202307-divvy-tripdata

# load packages
library(tidyverse)
library(hms)

# cleaning ----------------------------------------------------------------

# load and merge data raw data
july_2022 <- read_csv("202207-divvy-tripdata.csv")
august_2022 <- read_csv("202208-divvy-tripdata.csv")
september_2022 <- read_csv("202209-divvy-tripdata.csv")
october_2022 <- read_csv("202210-divvy-tripdata.csv")
november_2022 <- read_csv("202211-divvy-tripdata.csv")
december_2022 <- read_csv("202212-divvy-tripdata.csv")
january_2023 <- read_csv("202301-divvy-tripdata.csv")
february_2023 <- read_csv("202302-divvy-tripdata.csv")
march_2023 <- read_csv("202303-divvy-tripdata.csv")
april_2023 <- read_csv("202304-divvy-tripdata.csv")
may_2023 <- read_csv("202305-divvy-tripdata.csv")
june_2023 <- read_csv("202306-divvy-tripdata.csv")
july_2023 <- read_csv("202307-divvy-tripdata.csv")
combined_data <- rbind(july_2022, august_2022, september_2022, 
                       october_2022, november_2022, december_2022, 
                       january_2023, february_2023, march_2023, april_2023, 
                       may_2023, june_2023, july_2023)

# basic understanding of data
head(combined_data)
str(combined_data)
colnames(combined_data)
glimpse(combined_data)
View(combined_data)

# rename 'member_casual' column 
combined_data <- rename(combined_data, 'user_type' = 'member_casual')
colnames(combined_data)

# format 'started_at' and 'ended_at' columns
combined_data$started_at <- ymd_hms(combined_data$started_at)
combined_data$ended_at <- ymd_hms(combined_data$ended_at)

# add 'day' column
combined_data <- combined_data %>% 
  mutate(day = wday(as.Date(started_at), label = TRUE), .before = started_at)

# add 'day_number' column - might be useful for analysis
combined_data <- combined_data %>% 
  mutate(day_number = 
           as.integer(
             factor(
               combined_data$day, 
               levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"), 
               ordered = TRUE)
             )
         , .after = day)

# add 'ride_length' column
combined_data <- combined_data %>% 
  mutate(ride_length = difftime(ended_at, started_at, units = "mins"), 
         .after = ended_at)

# min ride length
min(combined_data$ride_length)

# max ride length
max(combined_data$ride_length)

# looking for negative ride lengths 
weird_min <- filter(combined_data, ride_length < 0)
werid_min

# looking for extremely long ride lengths (longer than 1 day) (1 day = 1440 min)
weird_max <- filter(combined_data, ride_length > 1440)
weird_max

# remove ride lengths longer that are negative or longer than 1 day
combined_data_subset <- subset(combined_data, ride_length > 0 
                               & ride_length < 1440)
min(combined_data$ride_length)
max(combined_data$ride_length)

# how many people keep bikes longer than a day?

#greater_than_1_day <- combined_data %>% 
  group_by(user_type) %>% 
  filter(ride_length > 1440) %>% 
  summarize(n = n())

#greater_than_2_days <- combined_data %>% 
  group_by(user_type) %>% 
  filter(ride_length > 2880) %>% 
  summarize(n = n())

#greater_than_3_days <- combined_data %>% 
  group_by(user_type) %>% 
  filter(ride_length > 4320) %>% 
  summarize(n = n())
  
# casuals made up for majority of bikes that had timers running for longer than
# a day. no members kept bikes longer than 2 days. therefore, members are not 
# just holding onto the same bike as i thought might be the case. casuals might
# not know how to properly end sessions or properly return bikes to stations.
# i feel comfortable leaving ride lengths that are greater than 0 min and less
# than 1440 min in the analysis

# export 'combined_data' as a csv for future use
# (don't forget to add file title at the end of path)
# write_csv(combined_data, "/ENTER FILE PATH HERE/combined_data.csv")
write_csv(combined_data, "/Users/marknival/Desktop/Analytics/Bikeshare_Case_Study/combined_data.csv")
write_csv(combined_data_subset, "/Users/marknival/Desktop/Analytics/Bikeshare_Case_Study/combined_data_subset.csv")
# analysis ---------------------------------------------------------------------

# load cleaned data if necessary
# combined_data <- read_csv("combined_data.csv")

# business question: how do members and casuals differ in their usage of the bikes?

# mean ride length
mean_ride_length <- combined_data_subset %>% 
  aggregate(x = ride_length ~ user_type, FUN = mean)
mean_ride_length

# mean ride length per day
mrl_per_day <- combined_data_subset %>% 
  aggregate(x = ride_length ~ user_type + day, FUN = mean)

# number of rides per day
rides_per_day <- combined_data_subset %>%
  group_by(day, user_type) %>%  
  summarize(n = n())
# although there are always more members than casuals the disparity between the
# two is much larger on the weekdays than the weekends. this indicates that 
# casuals are much more likely to use the bikes on the weekends than the 
# weekdays

# number of rides - weekends
rides_per_wend <- combined_data_subset %>% 
  group_by(user_type) %>% 
  filter(day == "Sun" | day == "Sat") %>% 
  summarize(n = n())

# percentage of total rides belonging to casuals - weekends
wend_diff <- (939092 / (975318 + 939092)) * 100
wend_diff
# on weekends, casuals made up 49% of the total rides

# number of rides - weekdays
rides_per_wday <- combined_data_subset %>% 
  group_by(user_type) %>% 
  filter(day %in% c("Mon","Tue","Wed","Thu","Fri")) %>% 
  summarize(n = n())

# percentage of total rides belonging to casuals - weekdays
wkday_diff <- (1630932 / (2994814 + 1630932)) * 100
wkday_diff
# on weekdays, casuals make up only about 35% of the total rides. therefore,
# casuals are much more likely to bike on the weekends

# summary 
ride_summary <- combined_data_subset %>% 
  group_by(day, user_type) %>% 
  summarize(num_of_rides = n(), avg_ride_length = mean(ride_length))
