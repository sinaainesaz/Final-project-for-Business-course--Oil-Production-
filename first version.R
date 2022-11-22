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

oil_saudi <- data %>% filter(LOCATION == 'SAU') %>% filter(TIME > 1970)

oil_canada <- data %>% filter(LOCATION == 'CAN') %>% filter(TIME > 1970)

oil_iran <- data %>% filter(LOCATION == 'IRN') %>% filter(TIME > 1970)

oil_iraq <- data %>% filter(LOCATION == 'IRQ') %>% filter(TIME > 1970)

#oil_russia <- data %>% filter(LOCATION == 'RUS') %>% filter(TIME > 1989)

oil_kuwait <- data %>% filter(LOCATION == 'KWT') %>% filter(TIME > 1970)

oil_emirates <- data %>% filter(LOCATION == 'ARE') %>% filter(TIME > 1970)

oil_usa <- data %>% filter(LOCATION == 'USA') %>% filter(TIME > 1970)

oil_norway <- data %>% filter(LOCATION == 'NOR') %>% filter(TIME > 1970)


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

