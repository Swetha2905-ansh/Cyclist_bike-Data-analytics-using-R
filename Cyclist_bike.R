library(tidyverse)

library(janitor)

library(lubridate)

library(ggplot2)

colnames(Divvy_2019_Q1)
colnames(Divvy_2020_Q1)

(Divvy_2019_Q1 <-rename (Divvy_2019_Q1
                         , ride_id = trip_id
                         , rideable_type = bikeid
                         , started_at = start_time
                         , ended_at = end_time
                         , start_station_name = from_station_name
                         , start_station_id = to_station_id
                         , end_station_name = to_station_name
                         , end_station_id = from_station_id
                         , member_casual = usertype))
str(Divvy_2019_Q1$ride_id)
str(Divvy_2020_Q1$ride_id)
str(Divvy_2019_Q1$rideable_type)
str(Divvy_2020_Q1$rideable_type)
Divvy_2019_Q1 <- mutate (Divvy_2019_Q1,ride_id = as.character(ride_id)
                         ,rideable_type = as.character(rideable_type))
all_trips <- bind_rows(Divvy_2019_Q1,Divvy_2020_Q1)
library(dplyr)
if(all(colnames(all_trips) %in% c("start_lat", "start_lng", "end_lat", "end_lng", "gender", "birthyear", "tripduration"))) {
  all_trips <- all_trips %>% 
    select(-c(start_lat, start_lng, end_lat, end_lng, gender, birthyear, tripduration))
} else {
  print("One or more column names are incorrect.")
}

unique(all_trips$member_casual)

all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,"Subscriber"="member","Customer"="casual"))

unique(all_trips$member_casual)
all_trips$date <- as.Date(all_trips$started_at)

all_trips$month <- format(as.Date(all_trips$date),"%m")
all_trips$day <- format(as.Date(all_trips$date),"%d")
all_trips$year <- format(as.Date(all_trips$date),"%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date),"%A")

all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length < 0),]

#MEAN
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)

#MEDIAN
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)

#MAX
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)

#MIN
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday)
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  #label=TRUE returns the weekday in characters
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=number_of_rides,fill=member_casual)) + geom_col(position = "dodge")
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=average_duration,fill=member_casual)) + geom_col(position = "dodge")
all_trips_v2 %>% 
  mutate(weekday = wday(started_at,label=TRUE)) %>%  
  group_by(member_casual,weekday) %>% 
  summarize(number_of_rides = n(),median_duration=median(ride_length)) %>% 
  arrange(member_casual,weekday) %>% 
  ggplot(aes(x=weekday,y=median_duration,fill=member_casual)) + geom_col(position = "dodge")
all_trips_v2 %>% 
  mutate(month = month(started_at, label=TRUE)) %>%  
  group_by(member_casual,month) %>% 
  summarize(number_of_rides = n(),average_duration=mean(ride_length)) %>% 
  arrange(member_casual,month) %>% 
  ggplot(aes(x=month,y=number_of_rides,fill=member_casual)) + geom_col(position = "dodge")
