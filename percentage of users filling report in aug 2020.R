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

211#users after the year of 2020

Data2020 <- Data %>% filter(date > "2020-01-03", date < "2021-01-01")

summary(Data2020) # 7446 unique users after 2020

DataAug <- Data %>% filter( date > "2020-08-01", date<"2020-08-31" )
summary(DataAug) #675 unique users in Aug2020


# of users who have added a report in aug 2020
prop <- 675/7446  #0.09


d<- as.data.frame(table(Data$user_id))
summary(d)
sd(d$Freq) 
sum(d$Freq) #total number of reports 28495
sum(d$Freq > 0, na.rm=TRUE)  # total number of users with at least 1 Report  13869



s<- as.data.frame(table(Data2020$user_id))
summary(s)
sd(s$Freq) #SD 2.692406
sum(s$Freq) #number of reports in 2020  7795
sum(s$Freq > 0, na.rm=TRUE) # total number of users with at least 1 Report in 2020:  3366

v<- as.data.frame(table(DataAug$user_id))
summary(v)
sd(v$Freq) #SD  0.7692461
sum(v$Freq)#number of reports in August 2020  839
sum(v$Freq > 0, na.rm=TRUE)  # total number of users with at least 1 Report in  August 2020: 469


#reporters/users   for a total number of active users  for the year of 2020 (3366)  469 filled 
#a report in the month of August  469/3366 = 0.14 IE 14 percent of active users filled a report  
#reports/ users   for a total number of active users  for the year of 2020 (3366)   839 reports were filled 
#in the month of August  839/3366 = 0.24 reports per active users


