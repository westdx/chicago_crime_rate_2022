library(lubridate) #date library
library(dplyr) #data manipulation library
library(ggplot2) #plotting library
library(ggfortify) #plotting library
options(scipen = 999)
theme_set(theme_bw())


crime_original <- read.csv('Crimes_-_2001_to_Present.csv', header = TRUE)
crime <- crime_original

#select important variables
#ID, Date, Primary Type, Arrest, District, Year
crime <- subset(crime, select = c(1, 3, 6, 9, 12, 18))
#format date 
crime$Date = mdy_hms(crime$Date)
crime$Date = as.Date(crime$Date)
#data cleaning
#Crime label
crime <- crime %>% mutate(Primary.Type = replace(Primary.Type, Primary.Type == 'CRIM SEXUAL ASSAULT', 'SEXUAL ASSAULT'))
crime <- crime %>% mutate(Primary.Type = replace(Primary.Type, Primary.Type == 'CRIMINAL SEXUAL ASSAULT', 'SEXUAL ASSAULT'))
crime <- crime %>% mutate(Primary.Type = replace(Primary.Type, Primary.Type == 'MOTOR VEHICLE THEFT', 'VEHICLE THEFT'))
crime <- crime %>% mutate(Primary.Type = replace(Primary.Type, Primary.Type == 'OTHER NARCOTIC VIOLATION', 'NARCOTICS'))
crime <- crime %>% mutate(Primary.Type = replace(Primary.Type, Primary.Type == 'NON-CRIMINAL (SUBJECT SPECIFIED)', 'NON-CRIMINAL'))
crime <- crime %>% mutate(Primary.Type = replace(Primary.Type, Primary.Type == 'NON - CRIMINAL', 'NON-CRIMINAL'))

#Simplify District
Central <- c(1, 12, 18)
North <- c(14, 19, 20, 24)
Northwest <- c(16, 17, 25, 31)
West <- c(10, 11, 15)
South <- c(2, 3, 7, 8, 9)
Southeast <- c(4, 5, 6, 21, 22)
#rename District
crime <- crime %>% mutate(District = replace(District, District %in% Central, 'CENTRAL'))
crime <- crime %>% mutate(District = replace(District, District %in% North, 'NORTH'))
crime <- crime %>% mutate(District = replace(District, District %in% Northwest, 'NORTHWEST'))
crime <- crime %>% mutate(District = replace(District, District %in% West, 'WEST'))
crime <- crime %>% mutate(District = replace(District, District %in% South, 'SOUTH'))
crime <- crime %>% mutate(District = replace(District, District %in% Southeast, 'SOUTHEAST'))


#filter in crime type
filter_1 <- c('ARSON', 'BATTERY', 'SEXUAL ASSAULT', 'HOMICIDE')
filter_2 <- c('BURGLARY', 'VEHICLE THEFT', 'ROBBERY', 'THEFT')
filter_3 <- c('ASSAULT', 'CRIMINAL DAMAGE', 'KIDNAPPING', 'SEX OFFENSE')


#lag year crime total
crime_type <- crime %>% group_by(Primary.Type, Year) %>% summarise(Total.Cases = n()) %>% as.data.frame()

crime_type_changes <- crime_type %>% group_by(Primary.Type) %>% arrange(Year) %>% mutate(Changes = ((Total.Cases - lag(Total.Cases)) / lag(Total.Cases)) * 100)

type_change_filter1 <- crime_type_changes %>% filter(Primary.Type %in% filter_1)
ggplot(data = type_change_filter1, aes(x = Year, y = Changes, color = Primary.Type)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(type_change_filter1$Year) + 1, max(type_change_filter1$Year)))) + facet_grid(Primary.Type ~ ., scales = "free_y") + geom_hline(yintercept = 0, linetype="dashed", color = "red") + ggtitle('Crime Changes in Percentage Over Years') + ylab('Change %') + theme(plot.title = element_text(hjust = 0.5))

type_change_filter2 <- crime_type_changes %>% filter(Primary.Type %in% filter_2)
ggplot(data = type_change_filter2, aes(x = Year, y = Changes, color = Primary.Type)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(type_change_filter2$Year) + 1, max(type_change_filter2$Year)))) + facet_grid(Primary.Type ~ ., scales = "free_y") + geom_hline(yintercept = 0, linetype="dashed", color = "red") + ggtitle('Crime Changes in Percentage Over Years') + ylab('Change %') + theme(plot.title = element_text(hjust = 0.5))

type_change_filter3 <- crime_type_changes %>% filter(Primary.Type %in% filter_3)
ggplot(data = type_change_filter3, aes(x = Year, y = Changes, color = Primary.Type)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(type_change_filter3$Year) + 1, max(type_change_filter3$Year)))) + facet_grid(Primary.Type ~ ., scales = "free_y") + geom_hline(yintercept = 0, linetype="dashed", color = "red") + ggtitle('Crime Changes in Percentage Over Years') + ylab('Change %') + theme(plot.title = element_text(hjust = 0.5))


#crime arrests
crime_arrests <- crime %>% group_by(Primary.Type, Year, Arrest) %>% summarise(Total.Arrests = n()) %>% as.data.frame()

crime_arrests_filter1 <- crime_arrests %>% filter(Primary.Type %in% filter_1)
ggplot(crime_arrests_filter1, aes(x = Year, y = Total.Arrests, color = Arrest)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(crime_arrests_filter1$Year), max(crime_arrests_filter1$Year)))) + facet_grid(Primary.Type ~ ., scales = "free_y") + ggtitle('Arrests Made over Years by Crime type') + ylab('Total Arrests') + theme(plot.title = element_text(hjust = 0.5))

crime_arrests_filter2 <- crime_arrests %>% filter(Primary.Type %in% filter_2)
ggplot(crime_arrests_filter2, aes(x = Year, y = Total.Arrests, color = Arrest)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(crime_arrests_filter2$Year), max(crime_arrests_filter2$Year)))) + facet_grid(Primary.Type ~ ., scales = "free_y") + ggtitle('Arrests Made over Years by Crime type') + ylab('Total Arrests') + theme(plot.title = element_text(hjust = 0.5))

crime_arrests_filter3 <- crime_arrests %>% filter(Primary.Type %in% filter_3)
ggplot(crime_arrests_filter3, aes(x = Year, y = Total.Arrests, color = Arrest)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(crime_arrests_filter3$Year), max(crime_arrests_filter3$Year)))) + facet_grid(Primary.Type ~ ., scales = "free_y") + ggtitle('Arrests Made over Years by Crime type') + ylab('Total Arrests') + theme(plot.title = element_text(hjust = 0.5))


#crime districts
crime_districts <- crime %>% group_by(District, Year) %>% summarise(Total.Cases = n()) %>% as.data.frame()
crime_districts$District <- as.character(crime_districts$District)
#filter NA district
crime_districts <- crime_districts[complete.cases(crime_districts),]

ggplot(crime_districts, aes(x = Year, y = Total.Cases, fill = District)) + geom_bar(stat = 'identity', position = 'stack') + scale_x_discrete(limits = c(seq(min(crime_districts$Year), max(crime_districts$Year)))) + ggtitle('Crime Cases Over Years by Districts') + ylab('Total Cases') + theme(plot.title = element_text(hjust = 0.5))

ggplot(crime_districts, aes(x = Year, y = Total.Cases, color = District)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(crime_districts$Year), max(crime_districts$Year)))) + facet_grid(District ~ ., scales = "free_y") + ggtitle('Crime Cases Over Years by Districts') + ylab('Total Cases') + theme(plot.title = element_text(hjust = 0.5))


#District Arrests
crime_districts_arrests <- crime %>% group_by(District, Year, Arrest) %>% summarise(Total.Arrests = n()) %>% as.data.frame() 
crime_districts_arrests <- crime_districts_arrests[complete.cases(crime_districts_arrests),]

ggplot(crime_districts_arrests, aes(x = Year, y = Total.Arrests, color = Arrest)) + geom_line(size = 1) + geom_point(size = 2) + theme(legend.position = "bottom") + scale_x_discrete(limits = c(seq(min(crime_districts_arrests$Year), max(crime_districts_arrests$Year)))) + facet_grid(District ~ ., scales = "free_y") + ggtitle('Arrests Made Over Years by Districts') + ylab('Total Arrests') + theme(plot.title = element_text(hjust = 0.5))

