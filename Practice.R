Data1 <- read.csv("qual_data.csv")
Data2 <- read.csv("qual_data_app_version.csv")
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)


Data <- inner_join(Data1, Data2, by = "report_id")



Data$date <- ymd_hms(Data$date)
Data$report_id <- as.factor(Data$report_id)
Data$user_id <- as.factor(Data$user_id)
Data$report_type <- as.factor(Data$report_type)
Data$has_note <- as.logical(Data$has_note)
Data$quality_adult <- as.factor(Data$quality_adult)
Data$n_photos <- as.numeric(Data$n_photos)
Data$location_choice <- as.factor(Data$location_choice)
Data$new_version <-as.logical(Data$app_version == "new_app")


summary(Data)


ggplot(data = Data, aes(x= has_note)) + geom_bar() + facet_grid(.~ app_version)    #note
ggplot(data = Data, aes(x= location_choice)) + geom_bar() + facet_grid(.~ app_version) + scale_x_discrete(
  limits = c("current", "selected"))   #Location, missing removed since there is only one case


ggplot(data = Data, aes(x= app_version, y = user_id > 10 )) + geom_boxplot()
       
       
part = count(Data, user_id)
ggplot(part, aes(n)) +geom_histogram(binwidth = 50) + xlim(0,110) + ylim(0,5000)


ggplot(Data, aes(x = app_version , fill = report_type)) + 
  geom_bar(position= "fill") +
  labs(title = "Report Types by app version")    


ggplot(Data, aes(x = app_version , fill = location_choice)) + 
  geom_bar(position= "fill") +
  labs(title = "location choice by app version")  

ggplot(Data, aes(x = app_version , fill = quality_adult)) + 
  geom_bar(position= "fill") +
  labs(title = "Validation by app version")  


ggplot(Data, aes(x = app_version , fill = quality_adult)) + 
  geom_bar(stat= "summary", fun = "mean") +
  labs(title = "Validation by app version")  



Data1 <- Data %>% select(user_id , report_id , new_version) %>% filter(new_version == T)
Data2 <- Data %>% select(user_id , report_id , new_version) %>% filter(new_version == F)

?pivot_longer
?pivot_wider
vignette("pivot")

bata1 <- Data1 %>%  group_by(user_id, report_id) %>% summarise(report_id) %>%  mutate(report_id, n = n()) %>%  
  distinct(user_id,.keep_all=TRUE) %>%  select(-report_id)
bata2 <- Data2 %>%  group_by(user_id, report_id) %>% summarise(report_id) %>%  mutate(report_id, n = n()) %>%  
  distinct(user_id,.keep_all=TRUE) %>% select(-report_id)


mean(bata1$n)
mean(bata2$n)


summary(bata1)
summary(bata2)

hello
