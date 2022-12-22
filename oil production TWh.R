library(tidyverse)
library(ggplot2)
library(dplyr)
library(forecast)
library(ggplot2)

data <- read_csv("oil-production-by-country.csv")
colnames(data)
data <- rename(data, Production = "Oil production (TWh)")
head(data)
str(data)

# Among the columns in this dataset only 3 of them are useful in this study #
data <- select(data, Code, Year, Production) 
summary(data)
summary(data$Production)

# dimension of the dataset #
dim(data)

# The focus in this study will be on the oil production of the most wealthies countries in the world #
# Which are : Venezuela, Saudi Arabia, Canada, Iran, Iraq, Russia, Kuwait, UAE, United States and Norway #

oil_venezuela <- data %>% filter(Code == 'VEN') %>% filter(Year > 1913)

# checking on the presence of any null value#
sum(is.na(oil_venezuela))

oil_saudi <- data %>% filter(Code == 'SAU') %>% filter(Year > 1935)

sum(is.na(oil_saudi))

oil_canada <- data %>% filter(Code == 'CAN') %>% filter(Year > 1900)

sum(is.na(oil_canada))

oil_iran <- data %>% filter(Code == 'IRN') %>% filter(Year > 1910)

sum(is.na(oil_iran))

oil_iraq <- data %>% filter(Code == 'IRQ') %>% filter(Year > 1926)

sum(is.na(oil_iraq))

oil_russia <- data %>% filter(Code == 'RUS') %>% filter(Year > 1985)

sum(is.na(oil_russia))

oil_kuwait <- data %>% filter(Code == 'KWT') %>% filter(Year > 1945)

sum(is.na(oil_kuwait))

oil_emirates <- data %>% filter(Code == 'ARE') %>% filter(Year > 1961)

sum(is.na(oil_emirates))

oil_usa <- data %>% filter(Code == 'USA') %>% filter(Year > 1900)

sum(is.na(oil_usa))

oil_norway <- data %>% filter(Code == 'NOR') %>% filter(Year > 1970)

sum(is.na(oil_norway))

head(oil_venezuela)
head(oil_saudi)
head(oil_canada)
head(oil_iran)
head(oil_iraq)
head(oil_russia)
head(oil_kuwait)
head(oil_emirates)
head(oil_usa)
head(oil_norway)

#making plot of year and production variables#
ggplot(data = oil_venezuela) +
  geom_line(mapping = aes(x = Year, y = Production))

# Production variable of each country#
Production_venezuela <- oil_venezuela$Production
Production_saudi <- oil_saudi$Production
Production_canada <- oil_canada$Production
Production_iran <- oil_iran$Production
Production_iraq <- oil_iraq$Production
Production_russia <- oil_russia$Production
Production_kuwait <- oil_kuwait$Production
Production_emirates <- oil_emirates$Production
Production_usa <- oil_usa$Production
Production_norway <- oil_norway$Production


# ACF of Production variable #
acf(Production_venezuela)
#Fitting Linear regression model#
linear_model_venezuela <- lm(data = oil_venezuela, Production ~ Year)
summary(linear_model_venezuela)

#plotting model #

ggplot(oil_venezuela, aes(x = Year, y = Production)) + geom_point() + 
  stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'Year', y = 'Production in TWh')

#test of DW#
dwtest(linear_model_venezuela)

# Diagnostic #
par(mfrow=c(2,2))
plot(linear_model_venezuela)
par(mfrow=c(1,1))

#---Time series of the production variable of each country---#
ts_venezuela = ts(Production_venezuela, frequency = 1, start = 1914)
ts_saudi = ts(Production_saudi, frequency = 1, start = 1936)
ts_canada = ts(Production_canada, frequency = 1, start = 1900)
ts_iran = ts(Production_iran, frequency = 1, start = 1911)
ts_iraq = ts(Production_iraq, frequency = 1, start = 1927)
ts_russia = ts(Production_russia, frequency = 1, start = 1985)
ts_kuwait = ts(Production_kuwait, frequency = 1, start = 1946)
ts_emirates = ts(Production_emirates, frequency = 1, start = 1962)
ts_usa = ts(Production_usa, frequency = 1, start = 1900)
ts_norway = ts(Production_norway, frequency = 1, start = 1971)

#--Plotting the Time Series--#
autoplot(ts_venezuela, series = 'Venezuela', lwd = 1.2) +
  autolayer(ts_saudi, series = 'Saudi Arabia', lwd = 1.2)+
  autolayer(ts_canada, series = 'Canada', lwd = 1.2)+
  autolayer(ts_iran, series = 'Iran', lwd = 1.2)+
  autolayer(ts_iraq, series = 'Iraq', lwd = 1.2)+
  autolayer(ts_russia, series = 'Russia', lwd = 1.2)+
  autolayer(ts_kuwait, series = 'Kuwait', lwd = 1.2)+
  autolayer(ts_emirates, series = 'Emirates', lwd = 1.2)+
  autolayer(ts_usa, series = 'USA', lwd = 1.2)+
  autolayer(ts_norway, series = 'Norway', lwd = 1.2)+
  scale_color_manual(values = c("red", "blue", "green", "yellow", "purple", "black", "gray", "orange", "white", "brown"))+
  labs(title = 'Crude Oil Production in TWh plot', x = '\nYear', y = 'Production Value in TWh\n', 
       subtitle = 'in Venezuela, Saudi Arabia, Canada, Iran, Iraq, Russia, Kuwait, Emirates, USA and Norway')+
  theme(panel.background = element_rect(fill = '#EDE0D4', colour = '#EDE0D4'),
        plot.background = element_rect(fill = '#EDE0D4', colour = '#EDE0D4'),
        plot.title = element_text(size = 22, colour = 'gray15', face = 'bold', hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(size = 14, colour = 'gray15', face = 'italic', hjust = 0.5, vjust = 0.5),
        plot.caption = element_text(color = 'gray25', face = 'italic', size = 12),
        panel.grid.major.y = element_line(linetype = 'dotted', colour = 'gray70'),
        panel.grid.minor.y = element_line(linetype = 'dotted', colour = 'gray75'),
        panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),  
        axis.title = element_text(size = 14, face = 'bold', colour = 'gray15'),
        axis.text = element_text(size = 12, colour = 'gray8'), legend.position = 'bottom',
        axis.line = element_line(), legend.text = element_text(size = 10),
        legend.title = element_text(colour = 'gray5', size = 14, face = 'bold'),
        legend.background = element_rect(fill = '#EDE0D6', linetype = 'blank'))
# linear model for time series#
ts.plot(ts_venezuela, type="o")

#--Fitting linear model with tslm function--#
linear_model_venezuela_ts<- tslm(ts_venezuela~trend)
summary(linear_model_venezuela_ts)
dwtest(linear_model_venezuela_ts)
# obtained the same results as simple linear model#

# linear model with trend and seasonality#
linear_model_venezuela_ts_se<- tslm(ts_venezuela~trend+season)
summary(linear_model_venezuela_ts_se)
dwtest(linear_model_venezuela_ts_se)
res_linear_model_venezuela_ts_se <- residuals(linear_model_venezuela_ts_se)
plot(res_linear_model_venezuela_ts_se, ylab="residuals")
dwtest(linear_model_venezuela_ts_se)

#plot of the model
plot(ts_venezuela, ylab="Venezuela Oil Production", xlab="Time")
lines(fitted(linear_model_venezuela_ts_se), col=2)

###Non-linear Model###

#Implementing a simple Bass Model#
bass_model_venezuela <- BM(Production_venezuela,display = T)
summary(bass_model_venezuela)
###predicting
pred_bass_model_venezuela <- predict(bass_model_venezuela, newx=c(1:50))
pred_inst_model_venezuela <- make.instantaneous(pred_bass_model_venezuela)
plot(Production_venezuela, type= "b",xlab="Year", ylab="Annual Oil Prodcution of Venezuela in TWh",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred_inst_model_venezuela, lwd=2, col=2)

# --- ACF & PACF Plot for countries ---#
tsdisplay(ts_venezuela, main = 'ACF and PACF Plot - Venezuela', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_saudi, main = 'ACF and PACF Plot - Saudi Arabia', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_canada, main = 'ACF and PACF Plot - Canada', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_iran, main = 'ACF and PACF Plot - Iran', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_iraq, main = 'ACF and PACF Plot - Iraq', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_russia, main = 'ACF and PACF Plot - Russia', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_kuwait, main = 'ACF and PACF Plot - Kuwait', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_emirates, main = 'ACF and PACF Plot - Emirates', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_usa, main = 'ACF and PACF Plot - USA', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)
tsdisplay(ts_norway, main = 'ACF and PACF Plot - Norway', col = '#6F1D1B', lwd = 1.5, xlab = 'Year', ylab = 'Oil Produced', cex.main = 1.5, cex.lab = 1.15)

#--ARIMA model--#

#Splitting data in train and test sets#
venezuela = round(length(ts_venezuela)*0.7, 0)
saudi = round(length(ts_saudi)*0.7, 0)
canada = round(length(ts_canada)*0.7, 0)
iran = round(length(ts_iran)*0.7, 0)
iraq = round(length(ts_iraq)*0.7, 0)
russia = round(length(ts_russia)*0.7, 0)
kuwait = round(length(ts_kuwait)*0.7, 0)
emirates = round(length(ts_emirates)*0.7, 0)
usa = round(length(ts_usa)*0.7, 0)
norway = round(length(ts_norway)*0.7, 0)

# Canada - Train & Test #
train_venezuela = window(ts_venezuela, end = 1989)
test_venezuela = window(ts_venezuela, start = 1990)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)
# Canada - Train & Test #
train_can = window(myts_can, end = 2003)
test_can = window(myts_can, start = 2004)

#--Applying ARIMA--#
# ARIMA(0,0,3) #
arima_venezuela_003 = Arima(ts_venezuela, order = c(0,0,3))
arima_venezuela_003
fitted(arima_venezuela_003)
plot(ts_venezuela)
lines(fitted(arima_venezuela_003), col=2)
# ARIMA(3,0,0) #
arima_venezuela_300 = Arima(ts_venezuela, order = c(3,0,0))
arima_venezuela_300
fitted(arima_venezuela_300)
plot(ts_venezuela)
lines(fitted(arima_venezuela_300), col=2)
# ARIMA(1,2,1) #
arima_venezuela_121 = Arima(ts_venezuela, order = c(1,2,1))
arima_venezuela_121
fitted(arima_venezuela_121)
plot(ts_venezuela)
lines(fitted(arima_venezuela_121), col=2)
# ARIMA(1,2,2)
arima_venezuela_122 = Arima(ts_venezuela, order = c(1,2,2))
arima_venezuela_122
fitted(arima_venezuela_122)
plot(ts_venezuela)
lines(fitted(arima_venezuela_122), col=2)
# COMPARING SUMMARIES#
summary(arima_venezuela_003)
summary(arima_venezuela_300)
summary(arima_venezuela_121)
summary(arima_venezuela_122)
# ARIMA with 1 AR has lower AIC #
fore_arima_121 <- forecast(arima_venezuela_121)
plot(fore_arima_121)
forecast(fore_arima_121)
plot(forecast(fore_arima_121))
checkresiduals(fore_arima_121)

###--- Using Auto ARIMA--- ###
auto_arima_venezuela <- auto.arima(ts_venezuela)
forecast(auto_arima_venezuela)
plot(forecast(auto_arima_venezuela))
checkresiduals(auto_arima_venezuela)
summary(auto_arima_venezuela)
