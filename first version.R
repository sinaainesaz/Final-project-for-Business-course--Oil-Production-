library(tidyverse)
library(ggplot2)
library(dplyr)
library(forecast)
library(ggplot2)

data <- read_csv("Oil Production Dataset.csv")
head(data)
str(data)

# Among the 8 columns in this dataset only 3 of them are useful in this study #
data <- select(data, LOCATION, TIME, Value)
summary(data)
summary(data$Value)

# dimension of the dataset #
dim(data)

# The focus in this study will be on the oil production of the most wealthies countries in the world #
# Which are : Venezuela, Saudi Arabia, Canada, Iran, Iraq, Russia, Kuwait, UAE, United States and Norway #

oil_venezuela <- data %>% filter(LOCATION == 'VEN') %>% filter(TIME > 1970)

# checking on the presence of any null value#
sum(is.na(oil_venezuela))

oil_saudi <- data %>% filter(LOCATION == 'SAU') %>% filter(TIME > 1970)

sum(is.na(oil_saudi))

oil_canada <- data %>% filter(LOCATION == 'CAN') %>% filter(TIME > 1970)

sum(is.na(oil_canada))

oil_iran <- data %>% filter(LOCATION == 'IRN') %>% filter(TIME > 1970)

sum(is.na(oil_iran))

oil_iraq <- data %>% filter(LOCATION == 'IRQ') %>% filter(TIME > 1970)

sum(is.na(oil_iraq))

#oil_russia <- data %>% filter(LOCATION == 'RUS') %>% filter(TIME > 1989)

#sum(is.na(oil_russia))

oil_kuwait <- data %>% filter(LOCATION == 'KWT') %>% filter(TIME > 1970)

sum(is.na(oil_kuwait))

oil_emirates <- data %>% filter(LOCATION == 'ARE') %>% filter(TIME > 1970)

sum(is.na(oil_emirates))

oil_usa <- data %>% filter(LOCATION == 'USA') %>% filter(TIME > 1970)

sum(is.na(oil_usa))

oil_norway <- data %>% filter(LOCATION == 'NOR') %>% filter(TIME > 1970)

sum(is.na(oil_norway))

head(oil_venezuela)
head(oil_saudi)
head(oil_canada)
head(oil_iran)
head(oil_iraq)
#head(oil_russia)
head(oil_kuwait)
head(oil_emirates)
head(oil_usa)
head(oil_norway)

# Time-Series Analysis

ts_venezuela = ts(oil_venezuela$Value)
plot(ts_venezuela)

ggplot(data = oil_saudi) + geom_line(mapping =
       aes(x = TIME, y = Value))

ggplot(data = oil_iran) + geom_line(mapping = aes(x = TIME, y = Value))
ts_saudi = ts(oil_saudi$Value)
plot(ts_saudi)

ts_canada = ts(oil_canada$Value)
plot(ts_canada)

ts_iran = ts(oil_iran$Value)
plot(ts_iran)

ts_iraq = ts(oil_iraq$Value)
plot(ts_iraq)

#ts_russia = ts(oil_russia$Value)
#plot(ts_russia)

ts_kuwait = ts(oil_kuwait$Value)
plot(ts_kuwait)

ts_emirates = ts(oil_emirates$Value)
plot(ts_emirates)

ts_usa = ts(oil_usa$Value)
plot(ts_usa)

ts_norway = ts(oil_norway$Value)
plot(ts_norway)
