### Data wrangling 

## Installing packages to be used

#Tidyverse package for data importation and wrangling 
install.packages("tidyverse")
library(tidyverse)

#Lubridate package for date wrangling attributes
install.packages("lubridate")
library(lubridate)

#Ggplot package for visualizing data
install.packages("ggplot2")
library(ggplot2)

#Dyplr package provides grammar for data manipulation
install.packages("dplyr")
library(dplyr)

##Displaying working directory
getwd()

##Setting working directory
setwd("/Users/Wale/Downloads/Projects/Google Analytics/Cyclistic/Divvy_Project")


##Importing Data 
#Uploading csv files 

q2_2019 <- read.csv("/Users/Wale/Downloads/Projects/Google Analytics/Cyclistic/Divvy_Project/Divvy_Trips_2019_Q2.csv")

q2_2019 <- read.csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")
q1_2020 <- read.csv("Divvy_Trips_2020_Q1.csv")

#Viewing Data
View(q3_2019)
View(q2_2019)
View(q4_2019)
View(q1_2020)

##Combining data
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)
colnames(q1_2020)

##Renaming columns to ensure consistency in all tables
#Columns in q2_2019, q3_2019 & q4_2019 to be renamed to make them consistent with q1_2020
q2_2019 <- rename(q2_2019,
                 ride_id = "X01...Rental.Details.Rental.ID",
                 rideable_type = "X01...Rental.Details.Bike.ID",
                 started_at = "X01...Rental.Details.Local.Start.Time",
                 ended_at = "X01...Rental.Details.Local.End.Time",
                 start_station_name = "X03...Rental.Start.Station.Name",
                 start_station_id = "X03...Rental.Start.Station.ID",
                 end_station_name = "X02...Rental.End.Station.Name",
                 end_station_id = "X02...Rental.End.Station.ID",
                 member_casual = "User.Type")

q3_2019 <- rename(q3_2019,
                  ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "start_time",
                  ended_at = "end_time",
                  start_station_name = "from_station_name",
                  start_station_id = "from_station_id",
                  end_station_name = "to_station_name",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")

q4_2019 <- rename(q4_2019,
                  ride_id = "trip_id",
                  rideable_type = "bikeid",
                  started_at = "start_time",
                  ended_at = "end_time",
                  start_station_name = "from_station_name",
                  start_station_id = "from_station_id",
                  end_station_name = "to_station_name",
                  end_station_id = "to_station_id",
                  member_casual = "usertype")

#Checking for differences in dataframes
str(q1_2020)
str(q2_2019)
str(q3_2019)
str(q4_2019)

##Converting data types in q2_2019, q3_2019 & q4_2019 to match data types in q1_2020
q2_2019 <- mutate(q2_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q3_2019 <- mutate(q3_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
q4_2019 <- mutate(q4_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

#Ensuring datatypes are consistent in all dataframes
str(q1_2020)
str(q2_2019)
str(q3_2019)
str(q4_2019)

#Combining all dataframes together
bike_trips <- bind_rows(q1_2020, q2_2019, q3_2019, q4_2019)

View(bike_trips)

##Removing irrelevant columns 
#birthyear, gender, start_lat, start_lng, end_lat, end_lng, member.gender, X05...member.details.member.birthday.year, tripduration, X01...Rental.details.duration.in.seconds.uncapped 
bike_trips <- bike_trips %>% 
  select(-c(birthyear, gender, start_lat, start_lng, end_lat, end_lng, Member.Gender, 
            "X05...Member.Details.Member.Birthday.Year", "tripduration", "X01...Rental.Details.Duration.In.Seconds.Uncapped"))


###Inspecting new data frame bike_trips
View(bike_trips)
colnames(bike_trips)
summary(bike_trips)
str(bike_trips)
head(bike_trips)
dim(bike_trips)
nrow(bike_trips)

###Ensuring consistency in columns
table(bike_trips$member_casual)

#member_casual has four (4) descriptions for members which needs to be consolidated to two (2)
#Members are "Member" & "Subscriber"
#Non-members are "Casual" & "Customer"
#Replacing Subscriber with member & Customer with casual

bike_trips <- bike_trips %>% 
  mutate(member_casual = recode(member_casual,
         "Subscriber" = "member",
         "Customer" = "casual"))

table(bike_trips$member_casual)
table(bike_trips$start_station_id)
table(bike_trips$rideable_type)

###Adding columns to enable for data aggregation
#Columns day, month & year to be added for each year

bike_trips$date <- as.Date(bike_trips$started_at)
bike_trips$month <- format(as.Date(bike_trips$date),"%m")
bike_trips$day <- format(as.Date(bike_trips$date),"%d")
bike_trips$year <- format(as.Date(bike_trips$date),"%Y")
bike_trips$day_of_week <- format(as.Date(bike_trips$date),"%A")
 
#Adding addtional column to calculate duration of rides

bike_trips$ride_length <- difftime(bike_trips$ended_at,bike_trips$started_at)

str(bike_trips)
colnames(bike_trips)

is.factor(bike_trips$ride_length)
bike_trips$ride_length <- as.numeric(as.character(bike_trips$ride_length))
is.numeric(bike_trips$ride_length)

##Removing bad data
bike_trips_v2 <- bike_trips[!(bike_trips$start_station_name == "HQ QR" | bike_trips$ride_length<0),]


###Performing descriptive analysis
mean(bike_trips_v2$ride_length)
median(bike_trips_v2$ride_length)
max(bike_trips_v2$ride_length)
min(bike_trips_v2$ride_length)

summary(bike_trips_v2$ride_length)

#Comparing members vs casual riders
aggregate(bike_trips_v2$ride_length ~ bike_trips_v2$member_casual, FUN = mean)
aggregate(bike_trips_v2$ride_length ~ bike_trips_v2$member_casual, FUN = median)
aggregate(bike_trips_v2$ride_length ~ bike_trips_v2$member_casual, FUN = max)
aggregate(bike_trips_v2$ride_length ~ bike_trips_v2$member_casual, FUN = min)

#Comparing average ride times by day for members vs casual riders
aggregate(bike_trips_v2$ride_length ~ bike_trips_v2$member_casual + bike_trips_v2$day_of_week, FUN = mean)

#Arranging the days of the week in order
bike_trips_v2$day_of_week <- ordered(bike_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Analyzing ridership data by type & weekday ----------------------
bike_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

#Visualizing average duration
library(dplyr)
library(ggplot2)

bike_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

  View(bike_trips_v2)
  
#Visualizing by rider type
  bike_trips_v2 %>% 
    mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, weekday) %>% 
    summarise(number_of_rides = n(),
              average_duration = mean(ride_length)) %>% 
    ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) + 
    geom_col(position = "dodge")
  
  
  ### Comparing member vs casual ride counts by weekday
  bike_trips_v2 %>%
    mutate(weekday = wday(started_at, label = TRUE)) %>%
    group_by(member_casual, weekday) %>%
    summarise(number_of_rides = n()) %>%
    ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Ride Counts by Weekday",
         x = "Weekday",
         y = "Number of Rides",
         fill = "Member/Casual") +
    theme_minimal()
  
  ### Average ride duration by weekday and rider type 
  bike_trips_v2 %>%
    mutate(weekday = wday(started_at, label = TRUE)) %>%
    group_by(member_casual, weekday) %>%
    summarise(average_duration = mean(ride_length)) %>%
    ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
    geom_col(position = "dodge") +
    labs(title = "Average Ride Duration by Weekday",
         x = "Weekday",
         y = "Average Ride Duration (minutes)",
         fill = "Member/Casual") +
    theme_minimal()
  
  
  ### Time series of Average daily Ride Count
  bike_trips_v2 %>%
    group_by(date) %>%
    summarise(average_daily_rides = n()) %>%
    ggplot(aes(x = date, y = average_daily_rides)) +
    geom_line() +
    labs(title = "Time Series of Average Daily Ride Counts",
         x = "Date",
         y = "Average Daily Ride Counts") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  
#Exporting data frame to csv file
  
write.csv(counts, file = '/Users/Wale/Desktop/Divvy_Project/avg_ride_length.csv')

write.csv(counts, file = "counts.csv", row.names = TRUE)
write.csv(bike_trips_v2, file = "bike_trips_v2.csv", row.names = TRUE)
  
counts <- aggregate(bike_trips_v2$ride_length ~ bike_trips_v2$member_casual + bike_trips_v2$day_of_week, FUN = mean)
  
table(counts)
table(bike_trips_v2)

dim(bike_trips_v2)
nrow(bike_trips_v2)

num_rows <- nrow(bike_trips_v2)

print(num_rows)
  
