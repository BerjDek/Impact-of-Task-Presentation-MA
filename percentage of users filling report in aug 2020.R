#Analyzing the proportaion of participants that submitted a form in the month of august 2020

Data <- read.csv("MA_Reports_Submitted(2015-2021).csv")
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


Data$date <- ymd_hms(Data$date)
Data$report_id <- as.factor(Data$report_id)
Data$user_id <- as.factor(Data$user_id)
Data$report_type <- as.factor(Data$report_type)
Data$has_note <- as.logical(Data$has_note)
Data$quality_adult <- as.factor(Data$quality_adult)
Data$n_photos <- as.numeric(Data$n_photos)
Data$location_choice <- as.factor(Data$location_choice)
Data$new_version <-as.logical(Data$app_version == "new_app")

#total number of participants

summary(Data)   #27779 unique total users

#users after the year of 2020

Data2020 <- Data %>% filter(date > "2020-01-03" )

summary(Data2020) # 7446 unique users after 2020

DataAug <- Data %>% filter( date > "2020-08-01", date<"2020-08-31" )
summary(DataAug) #675 unique users in Aug2020


# of users who have added a report in aug 2020
prop <- 675/7446  #0.09
