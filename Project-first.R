#Importing required Libraries#
library(dplyr)
library(cran)
library(tidyverse)
library(lattice)
library(caret)
library(ggplot2)
library(corrplot)
library(forecast)
library(zoo)
library(lmtest) 
library(fpp2)
library(minpack.lm)
library(numDeriv)
library(reshape2)
library(deSolve)
library(DIMORA)
library(splines)
library(foreach)
library(gam)

#---Importing the Complete Data set---#

data <- read.csv("oil data2.csv", sep=";")
colnames(data)[2] <- "country_code"
data$country_code <- as.factor(data$country)
colnames(data)[3] <- "indicator"
data$indicator <- as.factor(data$indicator)
data$Country.Name <- NULL
data$Indicator.Code <- NULL
levels(data$indicator)

data[,3:ncol(data)] <- sapply(data[,3:ncol(data)], as.numeric)

head(data,10)

# oil production #
Production <- data %>% filter (data$indicator == "Oil production (TWh)")
Production$indicator <- NULL
Years <- data.frame(c(1960:2021))
coun = Production$country_code

Production <- as.data.frame(t(as.matrix(Production)))
colnames(Production) <- coun
Production <- Production[-1,]
rownames(Production) <- NULL
Production <- cbind(Production, Years)
colnames(Production)[ncol(Production)] <- "Years"
Production <- as.data.frame(sapply(Production, as.numeric))


Production

# Exploring #
options(repr.plot.width=25, repr.plot.height=6)

ggplot(Production, aes(x=Years))+
  geom_line(aes(y=USA, color="USA"),na.rm = TRUE)+
  geom_line(aes(y=RUS, color="RUS"),na.rm = TRUE)+
  geom_line(aes(y=SAU, color="SAU"),na.rm = TRUE)+
  geom_line(aes(y=CHN, color="CHN"),na.rm = TRUE)+
  geom_line(aes(y=IRN, color="IRN"),na.rm = TRUE)+
  geom_line(aes(y=IRQ, color="IRQ"),na.rm = TRUE)+
  geom_line(aes(y=ARE, color="ARE"),na.rm = TRUE)+
  geom_line(aes(y=KWT, color="KWT"),na.rm = TRUE)+        
  geom_line(aes(y=CAN, color="CAN"),na.rm = TRUE)+
  scale_color_discrete(name = "Countries")+
  ggtitle("Oil Production in TWh")+
  ylab("Production")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
# GDP #
GDP <- data %>% filter (data$indicator == "GDP (current US$)")
GDP$indicator <- NULL
Years <- data.frame(c(1960:2021))
coun = GDP$country_code

GDP <- as.data.frame(t(as.matrix(GDP)))
colnames(GDP) <- coun
GDP <- GDP[-1,]
rownames(GDP) <- NULL
GDP <- cbind(GDP, Years)
colnames(GDP)[ncol(GDP)] <- "Years"
GDP <- as.data.frame(sapply(GDP, as.numeric))


head(GDP)
# exploring #
options(repr.plot.width=25, repr.plot.height=6)

ggplot(GDP, aes(x=Years))+
  geom_line(aes(y=USA, color="USA"),na.rm = TRUE)+
  geom_line(aes(y=RUS, color="RUS"),na.rm = TRUE)+
  geom_line(aes(y=SAU, color="SAU"),na.rm = TRUE)+
  geom_line(aes(y=CHN, color="CHN"),na.rm = TRUE)+
  geom_line(aes(y=IRN, color="IRN"),na.rm = TRUE)+
  geom_line(aes(y=IRQ, color="IRQ"),na.rm = TRUE)+
  geom_line(aes(y=ARE, color="ARE"),na.rm = TRUE)+
  geom_line(aes(y=KWT, color="KWT"),na.rm = TRUE)+        
  geom_line(aes(y=CAN, color="CAN"),na.rm = TRUE)+
  scale_color_discrete(name = "Countries")+
  ylab("GDP")+
  ggtitle("GDP in USD$")

# Oil Consumption #
Consumption <- data %>% filter (data$indicator == "Oil Consumption  TWh")
Consumption$indicator <- NULL
Years <- data.frame(c(1960:2021))
coun = Consumption$country_code

Consumption <- as.data.frame(t(as.matrix(Consumption)))
colnames(Consumption) <- coun
Consumption <- Consumption[-1,]
rownames(Consumption) <- NULL
Consumption <- cbind(Consumption, Years)
colnames(Consumption)[ncol(Consumption)] <- "Years"
Consumption <- as.data.frame(sapply(Consumption, as.numeric))


head(Consumption)
# exploring #
options(repr.plot.width=25, repr.plot.height=6)

ggplot(Consumption, aes(x=Years))+
  geom_line(aes(y=USA, color="USA"),na.rm = TRUE)+
  geom_line(aes(y=RUS, color="RUS"),na.rm = TRUE)+
  geom_line(aes(y=SAU, color="SAU"),na.rm = TRUE)+
  geom_line(aes(y=CHN, color="CHN"),na.rm = TRUE)+
  geom_line(aes(y=IRN, color="IRN"),na.rm = TRUE)+
  geom_line(aes(y=IRQ, color="IRQ"),na.rm = TRUE)+
  geom_line(aes(y=ARE, color="ARE"),na.rm = TRUE)+
  geom_line(aes(y=KWT, color="KWT"),na.rm = TRUE)+        
  geom_line(aes(y=CAN, color="CAN"),na.rm = TRUE)+
  scale_color_discrete(name = "Countries")+
  ylab("Oil Consumption")+
  ggtitle("Oil Consumption in TWh")
# Oil Reserves #
Reserves <- data %>% filter (data$indicator == "Oil reserves")
Reserves$indicator <- NULL
Years <- data.frame(c(1960:2021))
coun = Reserves$country_code

Reserves <- as.data.frame(t(as.matrix(Reserves)))
colnames(Reserves) <- coun
Reserves <- Reserves[-1,]
rownames(Reserves) <- NULL
Reserves <- cbind(Reserves, Years)
colnames(Reserves)[ncol(Reserves)] <- "Years"
Reserves <- as.data.frame(sapply(Reserves, as.numeric))
Reserves[c(21:61),]
typeof(Reserves)
head(Reserves)
# exploring #
options(repr.plot.width=25, repr.plot.height=6)

ggplot(Reserves[c(21:61),], aes(x=Years))+
  geom_line(aes(y=USA, color="USA"),na.rm = TRUE)+
  geom_line(aes(y=RUS, color="RUS"),na.rm = TRUE)+
  geom_line(aes(y=SAU, color="SAU"),na.rm = TRUE)+
  geom_line(aes(y=CHN, color="CHN"),na.rm = TRUE)+
  geom_line(aes(y=IRN, color="IRN"),na.rm = TRUE)+
  geom_line(aes(y=IRQ, color="IRQ"),na.rm = TRUE)+
  geom_line(aes(y=ARE, color="ARE"),na.rm = TRUE)+
  geom_line(aes(y=KWT, color="KWT"),na.rm = TRUE)+        
  geom_line(aes(y=CAN, color="CAN"),na.rm = TRUE)+
  scale_color_discrete(name = "Countries")+
  ylab("Reserves")+
  ggtitle("Total reserves of each country by 2020")
#-------------------------------------DATA PREPARATION------------------------------------------#
#---Making dataframe for selected countries---#
# USA #
production <- Production$USA
consumption <- Consumption$USA
reserves <- Reserves$USA
gdp <- GDP$USA
years <- Production$Years
USA <- cbind.data.frame(production, consumption, reserves, gdp, years)
USA
min(USA$years)
typeof(USA)
USA_ts <- ts(USA[-5],start = min(USA$years),end=max(USA$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(USA$production)), 
    "\nNAs in GDP:",sum(is.na(USA$gdp)),
    "\nNAs in Consumption:", sum(is.na(USA$consumption)),
    "\nNAs in Reserves:", sum(is.na(USA$reserves)),
    "\nNAs in years:", sum(is.na(USA$years)))


USA = USA[complete.cases(USA),]
#complete.cases() function in R Language is used to return a logical vector with cases which are complete, i.e., no missing value
row.names(USA) <- NULL #reset the index
head(USA)
USA
Production$USA
USA$production
Year$USA
# Plotting time series for USA #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(USA_ts)
# Checking on the ACF of USA #
options(repr.plot.width=10, repr.plot.height=10)
par(mfrow=c(2,2))
acf(USA$production)
acf(USA$consumption)
acf(USA$gdp)
acf(USA$reserves)

# Plottig all the variables of the USA dataset #
options(repr.plot.width=25, repr.plot.height=6)
norm.USA <- cbind.data.frame(scale(USA[-5]), USA$years)

ggplot(norm.USA, aes(x=USA$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  geom_line(aes(y=reserves, color="reserves"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")

# Saudi Arabia #
production <- Production$SAU
consumption <- Consumption$SAU
reserves <- Reserves$SAU
gdp <- GDP$SAU
years <- Production$Years
SAU <- cbind.data.frame(production, consumption, reserves, gdp, years)
SAU
min(SAU$years)
typeof(SAU)
SAU_ts <- ts(SAU[-5],start = min(SAU$years),end=max(SAU$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(SAU$production)), 
    "\nNAs in GDP:",sum(is.na(SAU$gdp)),
    "\nNAs in Consumption:", sum(is.na(SAU$consumption)),
    "\nNAs in Reserves:", sum(is.na(SAU$reserves)),
    "\nNAs in years:", sum(is.na(SAU$years)))

SAU = SAU[complete.cases(SAU),]
row.names(SAU) <- NULL #reset the index
head(SAU)
# Plotting time series for Saudi Arabia #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(SAU_ts)
# Checking on the ACF of Saudi Arabia #
par(mfrow=c(2,2))
acf(SAU$production)
acf(SAU$consumption)
acf(SAU$gdp)
acf(SAU$reserves)
# Plottig all the variables of the Saudi Arabia dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.SAU <- cbind.data.frame(scale(SAU[-5]), SAU$years)

ggplot(norm.SAU, aes(x=SAU$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  geom_line(aes(y=reserves, color="reserves"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")

# Iran #
production <- Production$IRN
consumption <- Consumption$IRN
reserves <- Reserves$IRN
gdp <- GDP$IRN
years <- Production$Years
IRN <- cbind.data.frame(production, consumption, reserves, gdp, years)
IRN

# filling the years 1991 and 1992 of gdp with average of the years 1990 and 1993
IRN$gdp[c(32,33)] = c(94278460000, 94278460000)
# filling the year 2021 of gdp with value of 2020
IRN$gdp[c(62)] = c(231547600000)
# filling the year 2021 of reserve with value of 2020
IRN$reserves[c(62)] = c(21523920416)

IRN_ts <- ts(IRN[-5],start = min(IRN$years),end=max(IRN$years))
# checking on NAn value #
cat("NAs in Production:",sum(is.na(IRN$production)), 
    "\nNAs in GDP:",sum(is.na(IRN$gdp)),
    "\nNAs in Consumption:", sum(is.na(IRN$consumption)),
    "\nNAs in Reserves:", sum(is.na(IRN$reserves)),
    "\nNAs in years:", sum(is.na(IRN$years)))


IRN = IRN[complete.cases(IRN),]
row.names(IRN) <- NULL #reset the index
head(IRN)
IRN_ts

# Plotting time series for Iran #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(IRN_ts)
# Checking on the ACF of Iran #
par(mfrow=c(2,2))
acf(IRN$production)
acf(IRN$consumption)
acf(IRN$gdp)
acf(IRN$reserves)
# Plottig all the variables of the Iran dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.IRN <- cbind.data.frame(scale(IRN[-5]), IRN$years)

ggplot(norm.IRN, aes(x=IRN$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  geom_line(aes(y=reserves, color="reserves"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")


############ A plot including all the things together ##########
autoplot(USA_ts[,1], series = 'United States', lwd = 0.7) +
  autolayer(SAU_ts[,1], series = 'Saudi Arabia', lwd = 0.7)+
  autolayer(IRN_ts[,1], series = 'Iran', lwd = 0.7)+
  scale_color_manual(values = c('#6F1D1B', '#CD5C5C', '#000080'))+
  labs(title = 'Crude Oil Production Plot', x = '\nYear', y = 'Value\n', 
       subtitle = 'in United States, Saudi Arabia and Iran')+
  geom_hline(yintercept = 2009.201, linetype = 'longdash', color = '#6F1D1B')+
  annotate('text', x = 1960, y = 2000, label = 'Avg. Value for Iran:\n2009.201', size = 3, color = '#6F1D1B')+
  annotate('text', x = 1970, y = 600, label = '1960 - Lowest crude oil value for Iran\n(622.1003)', size = 3, fontface = 'italic', 
           color = '#6F1D1B')+
  annotate(geom = 'curve', x = 1970, y = 600, xend = 1961, yend = 600, curvature = -0.1, arrow = arrow(length = unit(0.5, 'cm')),
           color = '#6F1D1B')+
  annotate('text', x = 1965, y = 3000, label = '1974 - Highest crude\noil value for Iran (3526.183)', size = 3, fontface = 'italic', 
           color = '#6F1D1B')+
  annotate(geom = 'curve', x = 1974, y = 3570, xend = 1965, yend = 3250, curvature = 0.3, arrow = arrow(length = unit(0.5, 'cm')),
           color = '#6F1D1B')+
  geom_hline(yintercept = 5178.294, linetype = 'longdash', color = '#000080')+
  annotate('text', x = 1962, y = 5200, label = 'Avg. Value for United States:\n5178.294', size = 3, color = '#000080')+
  annotate('text', x = 1997, y = 2800, label = '2008 - Lowest crude\noil value for United States (3514.844) and from this year\nCrude Oil production starts to increase until the year 2014.', size = 3, fontface = 'italic', 
           color = '#000080')+
  annotate(geom = 'curve', x = 2008, y = 3514.844, xend = 2000, yend = 3000, curvature = 0.1, arrow = arrow(length = unit(0.5, 'cm')),
           color = '#000080')+
  annotate('text', x = 2010, y = 8300, label = '2019 - Highest crude\noil value for United States (8721.28)', size = 3, fontface = 'italic', 
          color = '#000080')+
  annotate(geom = 'curve', x = 2019, y = 8760, xend = 2010, yend = 8500, curvature = 0.2, arrow = arrow(length = unit(0.5, 'cm')),
     color = '#000080')+
  annotate('text', x = 1985, y = 6810, label = 'From 1985 the Oil production value in USA\ncontinues to decrease for the next 22 years until the year 2007 to reach\nUSAs lowest value.', size = 3, fontface = 'italic', 
           color = '#000080')+
  annotate(geom = 'curve', x = 1985, y = 5850, xend = 1985, yend = 6500, curvature = 0.2, arrow = arrow(length = unit(0.5, 'cm')),
           color = '#000080')+
 geom_hline(yintercept = 4361.518, linetype = 'longdash', color = '#CD5C5C')+
annotate('text', x = 2020, y = 4380, label = 'Avg. Value for Saudi Arabia:\n4361.518', size = 3, color = '#CD5C5C')+
  annotate('text', x = 1960, y = 1300, label = '1960 - Lowest crude\noil value for Saudi Arabia (750.4141)', size = 2.7, fontface = 'italic', 
           color = '#CD5C5C')+
 annotate('text', x = 2005, y = 7300, label = '2016- Highest crude\noil value for Saudi Arabia (6823.463)', size = 3, fontface = 'italic', 
 color = '#CD5C5C')+
  annotate(geom = 'curve', x = 2016, y = 6850, xend = 2005, yend = 6990, curvature = -0.2, arrow = arrow(length = unit(0.5, 'cm')),
           color = '#CD5C5C')+
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
        legend.background = element_rect(fill = '#EDE0D4', linetype = 'blank'))

#####################
####Correlations#####
#####################
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
####USA####
corr_USA <- cor(USA[-5], use="pairwise.complete.obs")

corrplot(corr_USA, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)
####SAUDI ARABIA#####
corr_SAU <- cor(SAU[-5], use="pairwise.complete.obs")

corrplot(corr_SAU, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)
####IRAN####
corr_IRN <- cor(IRN[-5], use="pairwise.complete.obs")

corrplot(corr_IRN, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)
#-- <- <- <- <- <- <- <- <- Splitting the data into Train and Test sets <- <- <- <- <- <- <- <- #
set.seed(1)

train_usa <- USA %>% filter(USA$years >= min(USA$years) & USA$years<=2008)
test_usa <- USA %>% filter(USA$years >= 2008)

train_sau <- SAU %>% filter(SAU$years >= min(SAU$years) & SAU$years<=2016)
test_sau <- SAU %>% filter(SAU$years >= 2016)

train_irn <- IRN %>% filter(IRN$years >= min(IRN$years) & IRN$years<=2016)
test_irn <- IRN %>% filter(IRN$years >= 2016 )

#----------------------------------Modelling and the Analysis--------------------------------------#

#---------------------------------------------------------#
#------------------ Linear Regression---------------------#
#---------------------------------------------------------#

#---------Linear Regression model for USA-------#

#Fitting linear model using only with the highest correlated variable with the target column#
MLR_USA_res <- tslm(production ~reserves, data=window(USA_ts, start=1980, end=2018 -.1))
summary(MLR_USA_res)
# On train set of USA #
LM1_USA_res <- lm(production~reserves, data=train_usa)
summary(LM1_USA_res)
# Stepwise Regression
LM2_USA_res <- step(LM1_USA_res, direction="both")
summary(LM2_USA_res)
AIC(MLR_USA_res)
AIC(LM1_USA_res)
AIC(LM2_USA_res)
accuracy(LM2_USA_res, test_usa)
fit_LM_USA_res <- fitted.values(LM2_USA_res)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
usa_fit <- cbind.data.frame(train_usa$production,fit_LM_USA_res, train_usa$years)
colnames(usa_fit) <- c("production", "fitted","years")
ggplot(usa_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
usa_pred <- predict(LM2_USA_res, test_usa)
summary(usa_pred)
usa_pred <- cbind.data.frame(test_usa$production, usa_pred, test_usa$years)
colnames(usa_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=usa_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=usa_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = usa_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = usa_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for UNITED STATES")

# Useful plots #
par(mfrow=c(2,2))
plot(LM2_USA_res)
par(mfrow=c(1,1))

#analysis of residuals
residuals_usa_res<- residuals(LM2_USA_res) 
plot(residuals_usa_res, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_usa_res)
dw<- dwtest(LM2_USA_res, alt="two.sided")
dw

###### <- <- Analysis on the degree polynomial of the variables <- <- <- <- <- ##############
############
##reserves##
############
LM_USA_res1 <- lm(production~reserves, data=train_usa)
summary(LM_USA_res1)

ggplot(train_usa, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'reserves of usa 1', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_USA_res1)

par(mfrow=c(1,1))
#with 2 degrees
LM_USA_res2 <- lm(production~poly(reserves,2), data=train_usa)
summary(LM_USA_res2)

ggplot(train_usa, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'reserves of usa 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_USA_res2)

par(mfrow=c(1,1))

LM_USA_res_poly <- lm(data=train_usa, production ~ reserves + I(reserves**2) + I(reserves**3))
summary(LM_USA_res_poly)
ggplot(train_usa, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'reserves of usa degree 5', y = 'production in TWh')
anova(LM_USA_res_poly) ##degree equal to 1 is better and significant
# result for reserves: we will go on with degree 1 #
###############
##consumption##
###############
LM_USA_con1 <- lm(production~consumption, data=train_usa)
summary(LM_USA_con1)

ggplot(train_usa, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'consumption in TWh 1', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_USA_con1)

par(mfrow=c(1,1))

# seems degree 2 would be good idea let's check it #
LM_USA_con2 <- lm(production~ poly(consumption,2), data=train_usa)
summary(LM_USA_con2)

ggplot(train_usa, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'consumption in TWh 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_USA_con2)

par(mfrow=c(1,1))

LM_USA_cons_multi <- lm(data=train_usa, production ~ consumption + I(consumption**2) + I(consumption**3))
summary(LM_USA_cons_multi)
ggplot(train_usa, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'consumption in TWh 3', y = 'production in TWh')

anova(LM_USA_cons_multi) ##degree equal to 1 is better and significant for consumption also

# result for conumption: we will go on with degree 1 #

###############
######gdp######
###############
LM_USA_gdp1 <- lm(production~gdp, data=train_usa)
summary(LM_USA_gdp1)

ggplot(train_usa, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'GDP 1', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_USA_gdp1)

par(mfrow=c(1,1))

# seems polynomial degree equal to 2 would be good idea #
LM_USA_gdp2 <- lm(production~ poly(gdp,2), data=train_usa)
summary(LM_USA_gdp2)

ggplot(train_usa, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'GDP 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_USA_gdp2)
anova(LM_USA_gdp1, LM_USA_gdp2)

# degree 2 better fits the data #
par(mfrow=c(1,1))
LM_USA_gdp_multi <- lm(data=train_usa, production ~ gdp + I(gdp**2) + I(gdp**3))
ggplot(train_usa, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'GDP 3', y = 'production in TWh')

summary(LM_USA_gdp_multi)
anova(LM_USA_gdp2, LM_USA_gdp_multi)

#result for gdp : we will go on with degree 2 #
######## <- <- <- <- <- <- <- <- <- ##########

#Fitting linear model using all the variable with their best degree of polynomial#
MLR_USA_full <- tslm(production ~ consumption + poly(gdp,2) + reserves, data=window(USA_ts, start=1980, end=2018 -.1))
summary(MLR_USA_full)
# On train set of USA #
LM_USA_full <- lm(production~consumption + poly(gdp,2) + reserves, data=train_usa)
summary(LM_USA_full)
# Stepwise Regression
LM2_USA_full <- step(LM_USA_full, direction="both")
summary(LM2_USA_full)
AIC(MLR_USA_full)
AIC(LM_USA_full)
AIC(LM2_USA_full)

fit_LM_USA <- fitted.values(LM2_USA_full)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
usa_fit <- cbind.data.frame(train_usa$production,fit_LM_USA, train_usa$years)
colnames(usa_fit) <- c("production", "fitted","years")
ggplot(usa_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
usa_pred <- predict(LM2_USA_full, test_usa)
usa_pred <- cbind.data.frame(test_usa$production, usa_pred, test_usa$years)
colnames(usa_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=usa_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=usa_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = usa_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = usa_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for UNITED STATES with full model")

# Useful plots #
par(mfrow=c(2,2))
plot(LM2_USA_full)
par(mfrow=c(1,1))

#analysis of residuals
residuals_usa<- residuals(LM2_USA_full) 
plot(residuals_usa, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_usa)
dw<- dwtest(LM2_USA_full, alt="two.sided")
dw

#------------Linear Regression model for Saudi Arabia ---------#
#Fitting linear model using only with the highest correlated variable with the target column#
MLR_SAU_gdp <- tslm(formula = production ~ gdp, data=window(SAU_ts, start=1980, end=2009 -.1))
summary(MLR_SAU_gdp)
# Linear Model On train set of Saudi Arabia #
LM1_SAU_gdp <- lm(formula= production ~ gdp, data=train_sau)
summary(LM1_SAU_gdp)
# Stepwise Regression
LM2_SAU_gdp <- step(LM1_SAU_gdp, direction="both")
summary(LM2_SAU_gdp)
AIC(MLR_SAU_gdp)
AIC(LM1_SAU_gdp)
AIC(LM2_SAU_gdp)

fit_LM_SAU_gdp <- fitted.values(LM2_SAU_gdp)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
sau_fit <- cbind.data.frame(train_sau$production,fit_LM_SAU_gdp, train_sau$years)
colnames(sau_fit) <- c("production", "fitted","years")
ggplot(sau_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
sau_pred <- predict(LM2_SAU_gdp, test_sau)
sau_pred <- cbind.data.frame(test_sau$production, sau_pred, test_sau$years)
colnames(sau_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=sau_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=sau_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = sau_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = sau_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for SAUDI ARABIA contatining only gdp")

par(mfrow=c(2,2))
plot(LM2_SAU_gdp)
par(mfrow=c(1,1))

#analysis of residuals
residuals_sau_gdp<- residuals(LM2_SAU_gdp) 
plot(residuals_sau_gdp, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_sau_gdp)
dw<- dwtest(LM2_SAU_gdp, alt="two.sided")
dw
###### <- <- Analysis on the degree polynomial of the variables <- <- <- <- <- ##############
############
##reserves##
############
LM_SAU_res1 <- lm(production~reserves, data=train_sau)
summary(LM_SAU_res1)

ggplot(train_sau, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'reserves of sau 1', y = 'production in TWh')

# From what is obvious from the plot the higher degree of polynomial would not be a good idea.
# Although that we are going to check higher degrees.
# Diagnostic
par(mfrow=c(2,2))
plot(LM_SAU_res1)

par(mfrow=c(1,1))
#with 2 degrees
LM_SAU_res2 <- lm(production~poly(reserves,2), data=train_sau)
summary(LM_SAU_res2)
# Diagnostic
par(mfrow=c(2,2))
plot(LM_SAU_res2)
par(mfrow=c(1,1))

ggplot(train_sau, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'reserves of sau 2', y = 'production in TWh')

anova(LM_SAU_res1, LM_SAU_res2)
##degree equal to 1 is better and is significant#

# result for reserves: we will go on with degree 1 #

###############
##consumption##
###############
LM_SAU_con1 <- lm(production~consumption, data=train_sau)
summary(LM_SAU_con1)

ggplot(train_sau, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'consumption in TWh 1', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_SAU_con1)
par(mfrow=c(1,1))
# seems degree 2 or 3 would be good idea let's check them #
LM_SAU_con2 <- lm(production~ poly(consumption,2), data=train_sau)
summary(LM_SAU_con2)

ggplot(train_sau, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'consumption in TWh 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_SAU_con2)

anova(LM_SAU_con1, LM_SAU_con2)

par(mfrow=c(1,1))

LM_SAU_cons_multi <- lm(data=train_sau, production ~ consumption + I(consumption**2) + I(consumption**3))
summary(LM_SAU_cons_multi)
ggplot(train_sau, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'consumption in TWh 3', y = 'production in TWh')

anova(LM_SAU_cons_multi, LM_SAU_con1) ##degree equal to 1 is better and significant for consumption also

# result for consumption: we will go on with degree 1 #

###############
#####GDP#######
###############
LM_SAU_gdp1 <- lm(production~gdp, data=train_sau)
summary(LM_SAU_gdp1)

ggplot(train_sau, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'GDP 1', y = 'production in TWh')

# seems degree 2 would be good idea let's check it #
LM_SAU_gdp2 <- lm(production~ poly(gdp,2), data=train_sau)
summary(LM_SAU_gdp2)

ggplot(train_sau, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'GDP 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_SAU_gdp2)

anova(LM_SAU_gdp1, LM_SAU_gdp2)

par(mfrow=c(1,1))
# degree 2 is better than 1 but to make sure we try degree 3 too #
LM_SAU_gdp_multi <- lm(data=train_sau, production ~ gdp + I(gdp**2) + I(gdp**3))
summary(LM_SAU_gdp_multi)
ggplot(train_sau, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'GDP 3', y = 'production in TWh')

anova(LM_SAU_gdp_multi, LM_SAU_gdp2) ##degree equal to 1 is better and significant for gdp

# result for gdp: we will go on with degree 1 #
######## <- <- <- <- <- <- <- <- <- ##########

#Fitting linear model using all the variable with their best degree of polynomial#
MLR_SAU_full <- tslm(production ~ consumption + gdp + reserves, data=window(SAU_ts, start=1980, end=2009 -.1))
summary(MLR_SAU_full)
# On train set of SAUDI ARABIA #
LM_SAU_full <- lm(production~consumption + gdp + reserves, data=train_sau)
summary(LM_SAU_full)
# Stepwise Regression
LM2_SAU_full <- step(LM_SAU_full, direction="both")
summary(LM2_SAU_full)
AIC(MLR_SAU_full)
AIC(LM_SAU_full)
AIC(LM2_SAU_full)

fit_LM_SAU <- fitted.values(LM2_SAU_full)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
sau_fit <- cbind.data.frame(train_sau$production,fit_LM_SAU, train_sau$years)
colnames(sau_fit) <- c("production", "fitted","years")
ggplot(sau_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
sau_pred <- predict(LM2_SAU_full, test_sau)
sau_pred <- cbind.data.frame(test_sau$production, sau_pred, test_sau$years)
colnames(sau_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=sau_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=sau_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = sau_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = sau_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for SAUDI ARABIA with full model")

# Useful plots #
par(mfrow=c(2,2))
plot(LM2_SAU_full)
par(mfrow=c(1,1))

#analysis of residuals
residuals_sau<- residuals(LM2_SAU_full) 
plot(residuals_sau, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_sau)
dw<- dwtest(LM2_SAU_full, alt="two.sided")
dw

#-----------------Linear Regression model for IRAN -----------#
#Fitting linear model using only with the highest correlated variable with the target column#
MLR_IRN_cons <- tslm(formula = production ~ consumption, data=window(IRN_ts, start=1980, end=2009 -.1))
summary(MLR_IRN_cons)
# Linear Model On train set of IRAN #
LM1_IRN_cons <- lm(formula= production ~ consumption, data=train_irn)
summary(LM1_IRN_cons)
# Stepwise Regression
LM2_IRN_cons <- step(LM1_IRN_cons, direction="both")
summary(LM2_IRN_cons)
AIC(MLR_IRN_cons)
AIC(LM1_IRN_cons)
AIC(LM2_IRN_cons)

fit_LM_IRN_cons <- fitted.values(LM2_IRN_cons)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
irn_fit <- cbind.data.frame(train_irn$production,fit_LM_IRN_cons, train_irn$years)
colnames(irn_fit) <- c("production", "fitted","years")
ggplot(irn_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
irn_pred <- predict(LM2_IRN_cons, test_irn)
irn_pred <- cbind.data.frame(test_irn$production, irn_pred, test_irn$years)
colnames(irn_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=irn_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=irn_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = irn_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = irn_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for IRAN containing only consumption variable.")

par(mfrow=c(2,2))
plot(LM2_IRN_cons)
par(mfrow=c(1,1))

#analysis of residuals
residuals_irn_cons<- residuals(LM2_IRN_cons) 
plot(residuals_irn_cons, type = "b") 

Acf(residuals_irn_cons)
dw<- dwtest(LM2_IRN_cons, alt="two.sided")
dw
###### <- <- Analysis on the degree polynomial of the variables <- <- <- <- <- ##############
############
##reserves##
############
LM_IRN_res1 <- lm(production~reserves, data=train_irn)
summary(LM_IRN_res1)

ggplot(train_irn, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'reserves of irn 1', y = 'production in TWh')

# From what is obvious from the plot the higher degree of polynomial would not be a good idea.
# Although that we are going to check higher degrees.
# Diagnostic
par(mfrow=c(2,2))
plot(LM_IRN_res1)

par(mfrow=c(1,1))
#with 2 degrees
LM_IRN_res2 <- lm(production~poly(reserves,2), data=train_irn)
summary(LM_IRN_res2)
ggplot(train_irn, aes(x = reserves, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'reserves of irn 2', y = 'production in TWh')

# there is not much improvement #

# Diagnostic
par(mfrow=c(2,2))
plot(LM_IRN_res2)
par(mfrow=c(1,1))

anova(LM_IRN_res1, LM_IRN_res2)
##degree equal to 1 is better and is significant#

# result for reserves: we will go on with degree 1 #

###############
##consumption##
###############
LM_IRN_con1 <- lm(production~consumption, data=train_irn)
summary(LM_IRN_con1)

ggplot(train_irn, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'consumption in TWh 1', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_IRN_con1)
par(mfrow=c(1,1))
# seems degree 2 or 3 would be good idea let's check them #
LM_IRN_con2 <- lm(production~ poly(consumption,2), data=train_irn)
summary(LM_IRN_con2)

ggplot(train_irn, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'consumption in TWh 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_IRN_con2)
# Residuals plot is now better #

anova(LM_IRN_con1, LM_IRN_con2)

par(mfrow=c(1,1))

LM_IRN_cons_multi <- lm(data=train_irn, production ~ consumption + I(consumption**2) + I(consumption**3))
summary(LM_IRN_cons_multi)
ggplot(train_irn, aes(x = consumption, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'consumption in TWh 3', y = 'production in TWh')

anova(LM_IRN_cons_multi, LM_IRN_con1) ##degree equal to 2 is better and significant for consumption also

# result for conumption: we will go on with degree 2 #

###############
######GDP######
###############
LM_IRN_gdp1 <- lm(production~gdp, data=train_irn)
summary(LM_IRN_gdp1)

ggplot(train_irn, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ x) + labs(x = 'GDP 1', y = 'production in TWh')

# seems degree 2 would be good idea let's check it #
LM_IRN_gdp2 <- lm(production~ poly(gdp,2), data=train_irn)
summary(LM_IRN_gdp2)

ggplot(train_irn, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,2)) + labs(x = 'GDP 2', y = 'production in TWh')

# Diagnostic
par(mfrow=c(2,2))
plot(LM_IRN_gdp2)

anova(LM_IRN_gdp1, LM_IRN_gdp2)

par(mfrow=c(1,1))
# degree 2 is not better than 1 but to make sure we try degree 3 too #
LM_IRN_gdp_multi <- lm(data=train_irn, production ~ gdp + I(gdp**2) + I(gdp**3))
summary(LM_IRN_gdp_multi)
ggplot(train_irn, aes(x = gdp, y = production)) + geom_point() + stat_smooth(method = "lm", formula = y ~ poly(x,3)) + labs(x = 'GDP 3', y = 'production in TWh')

anova(LM_IRN_gdp_multi, LM_IRN_gdp2) ##degree equal to 1 is better and significant for gdp

# gdp variable seems not to be helpful variable for IRAN analysis We will try not to have this variable in the main functions to see what happens.

######## <- < <- <- <- <- <- <- <- <- <- <- <- <- <- <- <- - <- <- <- <- <- <- <- ##########

#Fitting linear model using all the variable with their best degree of polynomial#
MLR_IRN_full <- tslm(production ~ poly(consumption,2) + reserves, data=window(IRN_ts, start=1980, end=2009 -.1))
summary(MLR_IRN_full)
# On train set of IRAN #
LM_IRN_full <- lm(production~poly(consumption,2) + reserves, data=train_irn)
summary(LM_IRN_full)
# Stepwise Regression
LM2_IRN_full <- step(LM_IRN_full, direction="both")
summary(LM2_IRN_full)
AIC(MLR_IRN_full)
AIC(LM_IRN_full)
AIC(LM2_IRN_full)

fit_LM_IRN <- fitted.values(LM2_IRN_full)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
irn_fit <- cbind.data.frame(train_irn$production,fit_LM_IRN, train_irn$years)
colnames(irn_fit) <- c("production", "fitted","years")
ggplot(irn_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
irn_pred <- predict(LM2_IRN_full, test_irn)
irn_pred <- cbind.data.frame(test_irn$production, irn_pred, test_irn$years)
colnames(irn_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=irn_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=irn_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = irn_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = irn_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for IRAN with chosen variables.")

# Useful plots #
par(mfrow=c(2,2))
plot(LM2_IRN_full)
par(mfrow=c(1,1))

#analysis of residuals
residuals_irn<- residuals(LM2_IRN_full) 
plot(residuals_irn, type = "b") 

Acf(residuals_irn)
dw<- dwtest(LM2_IRN_full, alt="two.sided")
dw


#----------------------------------------Diffusion Models---------------------------------------#

#---------------------Bass standard model on USA to get the coefficients-------------------#

# LIKE THE LAB SESSIONS#
# Estimating a simple bass model #
BM_USA <- BM(USA$production)
summary(BM_USA)
coef(BM_USA)
plot.Dimora(BM_USA, oos=25)
residuals(BM_USA)
par(mfrow=c(1,1))
plot(residuals(BM_USA))
BM.fitted<- fitted(BM_USA)

predicted_usa = predict.Dimora(BM_USA, newx = c(1:62))
predicted_usa
plot(predicted_usa)

pred_BM_USA<- predict(BM_USA, newx=c(1:62))
pred.instBM_USA<- make.instantaneous(pred_BM_USA)

plot(USA$production, type= "b",xlab="Year", ylab="Annual productiton",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37,46, 55, 56), labels=USA$years[c(1,10,19,28,37,46, 55, 56)])
lines(pred.instBM_USA, lwd=2, col=1)

plot(USA$production, type="l",xaxt="n",xlab = "Year",ylab = "production")
axis(1, at = seq(1,length(USA$production),1),labels = USA$year)
lines(pred.instBM_USA, col=2)
length(Production$USA)
length(USA$years)
USA

Production$USA

USA$production
pred.instBM_USA
####graphical inspection of BM e GBMe1
BMS.fitted <- cbind.data.frame(pred_BM_USA, USA$years)
colnames(BMS.fitted) <- c("bm", "years")
# BMS.pred <- cbind.data.frame(predictedGBM, predictedBM, test$years)
# colnames(BMS.pred) <- c("gbm", "bm", "years")

ggplot()+
  geom_line(data=train_usa, aes(x= years,y=production, color="Actual Train"))+
  geom_line(data=BMS.fitted, aes(x=years,y=bm, color="BM"))
#geom_line(data=BMS.pred, aes(x=years,y=bm, color="BM"))
# GBM with 1 exponantial shock #
GBMe1_USA<- GBM(Production$USA,shock = "exp",nshock = 1,prelimestimates = c(BM(Production$USA, display=FALSE)$Estimate[1,1],
                                                                            BM(Production$USA, display=FALSE)$Estimate[2,1],
                                                                            BM(Production$USA, display=FALSE)$Estimate[3,1],
                                                                            10,0.1,0.1))
summary(GBMe1_USA)
plot.Dimora(GBMe1_USA, oos= 5)
predict.Dimora(GBMe1_USA, newx = c(1:62))

# GBM with mixed shocks shocks for USA #
GBM_mix_USA<- GBM(Production$USA, shock = "mixed", nshock = 2, 
                         prelimestimates = c(3.212495e+05,
                                             1.441450e-02,
                                             3.512072e-02,
                                             44,0.05,0.1,10,0.1,-0.1)) #44,0.05,0.1,10,0.1,-0.1
summary(GBM_mix_USA) # in summary gozashte shavad
plot.Dimora(GBM_mix_USA, oos=3) #in plot gozashte shavad

predicted_usa = predict.Dimora(GBM_mix_USA, newx = c(1:62))
predicted_usa
plot(predicted_usa)
GBM.fitted<- fitted(GBM_mix_USA)
GBMS.fitted <- cbind.data.frame(GBM.fitted, USA$years)
colnames(GBMS.fitted) <- c("bm", "years")
# BMS.pred <- cbind.data.frame(predictedGBM, predictedBM, test$years)
# colnames(BMS.pred) <- c("gbm", "bm", "years")

ggplot()+
  geom_line(data=train_usa, aes(x= years,y=production, color="Actual Train"))+
  geom_line(data=test_usa, aes(x= years,y=production, color="Actual Test"))+
  geom_line(data=GBMS.fitted, aes(x=years,y=bm, color="BM"))


####residual analysis of GBM for USA #
resGBM_mix_USA<- residuals(GBM_mix_USA)
Acf(resGBM_mix_USA) #in plot gozashte shavad
Pacf(resGBM_mix_USA) #in plot gozashte shavad

pred_GBM_mix_USA<- predict(GBM_mix_USA, newx=c(1:60))
pred.instGBM_mix_USA<- make.instantaneous(pred_GBM_mix_USA)

plot(Production$USA, type= "b",xlab="Year", ylab="Annual productiton",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,19,28,37), labels=USA$years)
lines(pred.instGBM_mix_USA, lwd=2, col=1)
#---------------------Bass standard model on SAUDI ARABIA to get the coefficients-------------------#

# LIKE THE LAB SESSIONS#
# Estimating a simple bass model #
BM_SAU <- BM(Production$SAU, display=T)
summary(BM_SAU)
coef(BM_SAU)


###plot of model 
plot.Dimora(BM_SAU)

# GBM with 1 exponantial shock #
GBMe1_SAU<- GBM(Production$SAU[1:57],shock = "exp",nshock = 1,prelimestimates = c(BM(Production$SAU, display=FALSE)$Estimate[1,1],
                                                                            BM(Production$SAU, display=FALSE)$Estimate[2,1],
                                                                            BM(Production$SAU, display=FALSE)$Estimate[3,1],
                                                                            15,0.1,-0.05))
summary(GBMe1_SAU)
Production$SAU[1:57]
plot.Dimora(GBMe1_SAU, oos = 5)
# GBM with mixed shocks shocks for SAUDI ARABIA #
GBM_mix_SAU<- GBM(Production$SAU, shock = "mixed", nshock = 2, 
                  prelimestimates = c(4.854750e+05,
                                      1.386853e-03,
                                      1.295628e-01,
                                      21,0.1,-0.1,15,0.05,0.1),oos = round(length(Production$SAU)*0.25), display = T) #15,-0.1,0.1,4,0.05,0.1 - #17,-0.1,0.1,5,-0.05,0.1 #35,-0.1,0.1,15,0.5,0.05
summary(GBM_mix_SAU) # in summary gozashte shavad.  #21,0.05,-0.1,15,0.05,0.1. #21,0.1,-0.1,15,0.05,0.1
plot.Dimora(GBM_mix_SAU, oos = round(length(Production$SAU)*0.25)) # in plot gozashte shavad

####residual analysis of GBM for SAUDI ARABIA #
resGBM_SAU<- residuals(GBM_mix_SAU)
Acf(resGBM_SAU) # in plot gozashte shavad
Pacf(resGBM_SAU) # in plot gozashte shavad

### we fit a GGM (better model...but we need to interpret well the parameters)
GGM_SAU<- GGM(Production$SAU, prelimestimates=c(4.854750e+05, 0.001, 0.01, 1.386853e-03, 1.295628e-01))
summary(GGM_SAU)
plot.Dimora(GGM_SAU)

#---------------------Bass standard model on IRAN to get the coefficients-------------------#

# LIKE THE LAB SESSIONS#
# Estimating a simple bass model #
BM_IRN <- BM(Production$IRN, display=T)
summary(BM_IRN)
plot.Dimora(BM_IRN)

# GBM with 1 exponantial shock #
GBMe1_IRN<- GBM(Production$IRN,shock = "exp",nshock = 1,prelimestimates = c(BM(Production$IRN, display=FALSE)$Estimate[1,1],
                                                                            BM(Production$IRN, display=FALSE)$Estimate[2,1],
                                                                            BM(Production$IRN, display=FALSE)$Estimate[3,1],
                                                                            9,0.08,-0.09))
summary(GBMe1_IRN)
plot.Dimora(GBMe1_IRN)
# GBM with mixed shocks shocks for IRAN #
GBM_mix_IRN<- GBM(Production$IRN, shock = "mixed", nshock = 2, prelimestimates = c(1.534326e+05,
                                                                                   3.706325e-03,
                                                                                   1.404754e-01,
                                                                                   5,0.1,-0.1,30,0.1,0.1)) #12,0.1,-0.1,20,0.1,-0.1 #9,-0.05,-0.1,30,-0.05,-0.1 #5,0.1,-0.1,30,0.1,0.1
summary(GBM_mix_IRN)
plot.Dimora(GBM_mix_IRN)

pred_GBMmixIRN<- predict(GBM_mix_IRN, newx=c(1:80))
pred.instGBMmixIRN<- make.instantaneous(pred_GBMmixIRN)

par(mfrow=c(1,1))
plot(Production$IRN, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$IRN[c(1,10,20,30,40,51,61)])
lines(pred.instGBMmixIRN, lwd=2, col=2)

####residual analysis of GBM for IRAN #
resGBM_IRN<- residuals(GBM_mix_IRN)
plot(resGBM_IRN, type="b")
Acf(resGBM_IRN)
Pacf(resGBM_IRN)

#----------------------------------------ARIMA Models-------------------------------------------#
#-------------------ARIMA on USA----------------------#
production <- USA_ts[,1]
plot(production)
Acf(production)
Pacf(production)
# trying 1 differentiation #
USA_arima010 <- Arima(USA_ts[,1], order = c(0,1,0)) 
USA_resid010 <- residuals(USA_arima010)
tsdisplay(USA_resid010)
summary(USA_arima010)
plot(USA_ts[,1])
lines(fitted(USA_arima010), col=2)
# adding 1  AR #
USA_arima110 <- Arima(USA_ts[,1], order = c(1,1,0))
USA_resid110 <- residuals(USA_arima110)
tsdisplay(USA_resid110)
summary(USA_arima110)
plot(USA_ts[,1])
lines(fitted(USA_arima110), col=2)
# adding 1 MA #
USA_arima111 <- Arima(USA_ts[,1], order = c(1,1,1))
USA_resid111 <- residuals(USA_arima111)
tsdisplay(USA_resid111)
summary(USA_arima111)
plot(USA_ts[,1])
lines(fitted(USA_arima111), col=2)
# adding 2 AR #
USA_arima211 <- Arima(USA_ts[,1], order = c(2,1,1))
USA_resid211 <- residuals(USA_arima211)
tsdisplay(USA_resid211)
summary(USA_arima211)
plot(USA_ts[,1])
lines(fitted(USA_arima211), col=2)
# adding 2 MA #
USA_arima112 <- Arima(USA_ts[,1], order = c(1,1,2))
USA_resid112 <- residuals(USA_arima112)
tsdisplay(USA_resid112)
summary(USA_arima112)
plot(USA_ts[,1])
lines(fitted(USA_arima112), col=2)
# adding 2 AR #
USA_arima212 <- Arima(USA_ts[,1], order = c(2,1,2))
USA_resid212 <- residuals(USA_arima212)
tsdisplay(USA_resid212)
summary(USA_arima212)
plot(USA_ts[,1])
lines(fitted(USA_arima212), col=2)
# adding 3 MA #
USA_arima113 <- Arima(USA_ts[,1], order = c(1,1,3))
USA_resid113 <- residuals(USA_arima113)
tsdisplay(USA_resid113)
summary(USA_arima113)
plot(USA_ts[,1])
lines(fitted(USA_arima113), col=2)
# adding 2 AR #
USA_arima213 <- Arima(USA_ts[,1], order = c(2,1,3)) #BEST
USA_resid213 <- residuals(USA_arima213)
tsdisplay(USA_resid213)
summary(USA_arima213)
plot(USA_ts[,1])
lines(fitted(USA_arima213), col=2)
# adding 3 AR #
USA_arima313 <- Arima(USA_ts[,1], order = c(3,1,3))
USA_resid313 <- residuals(USA_arima313)
tsdisplay(USA_resid313)
summary(USA_arima313)
plot(USA_ts[,1])
lines(fitted(USA_arima313), col=2)
# adding 4 MA #
USA_arima214 <- Arima(USA_ts[,1], order = c(2,1,4))
USA_resid214 <- residuals(USA_arima214)
tsdisplay(USA_resid214)
summary(USA_arima214)
plot(USA_ts[,1])
lines(fitted(USA_arima214), col=2)
# adding 3 MA #
USA_arima314 <- Arima(USA_ts[,1], order = c(3,1,4))
USA_resid314 <- residuals(USA_arima314)
tsdisplay(USA_resid314)
summary(USA_arima314)
plot(USA_ts[,1])
lines(fitted(USA_arima314), col=2)
#Comparing the AICs
AIC(USA_arima010)
AIC(USA_arima110)
AIC(USA_arima111)
AIC(USA_arima211)
AIC(USA_arima112)
AIC(USA_arima212)
AIC(USA_arima113)
AIC(USA_arima213)
AIC(USA_arima313)
AIC(USA_arima214)
AIC(USA_arima314)


# FORECASTING WITH BEST MODEL #
usa.fitt <- fitted(USA_arima213)
usa.pro <- zoo(x=USA_ts[,1], order.by=index(usa.fitt))

for_USA <- forecast(USA_arima213)
plot(for_USA)
lines(usa.fitt, col="red")
fore_arima_USA <- forecast(USA_arima213)
plot(fore_arima_USA)
forecast(fore_arima_USA)
checkresiduals(fore_arima_USA)
# using auto arima #
USA_auto_arima <- auto.arima(USA_ts[,1])
USA_auto_resid <- residuals(USA_auto_arima)
tsdisplay(USA_auto_resid)
summary(USA_auto_arima)
# FORECASTING WITH AUTO MODEL #
forecast(USA_auto_arima)
plot(forecast(USA_auto_arima))
checkresiduals(USA_auto_arima)
summary(USA_auto_arima)


#-------------------ARIMA on SAUDI ARABIA----------------------#
production <- SAU_ts[,1]
plot(production)
Acf(production)
Pacf(production)
# trying 1 differentiation #
SAU_arima010 <- Arima(SAU_ts[,1], order = c(0,1,0)) 
SAU_resid010 <- residuals(SAU_arima010)
tsdisplay(SAU_resid010)
summary(SAU_arima010)
plot(SAU_ts[,1])
lines(fitted(SAU_arima010), col=2)
# adding 1  AR #
SAU_arima110 <- Arima(SAU_ts[,1], order = c(1,1,0)) 
SAU_resid110 <- residuals(SAU_arima110)
tsdisplay(SAU_resid110)
summary(SAU_arima110)
plot(SAU_ts[,1])
lines(fitted(SAU_arima110), col=2)
# adding 1 MA #
SAU_arima111 <- Arima(SAU_ts[,1], order = c(1,1,1))
SAU_resid111 <- residuals(SAU_arima111)
tsdisplay(SAU_resid111)
summary(USA_arima111)
plot(SAU_ts[,1])
lines(fitted(SAU_arima111), col=2)
# adding 2 AR #
SAU_arima211 <- Arima(SAU_ts[,1], order = c(2,1,1))
SAU_resid211 <- residuals(SAU_arima211)
tsdisplay(SAU_resid211)
summary(SAU_arima211)
plot(SAU_ts[,1])
lines(fitted(SAU_arima211), col=2)
# adding 2 MA #
SAU_arima112 <- Arima(SAU_ts[,1], order = c(1,1,2))
SAU_resid112 <- residuals(SAU_arima112)
tsdisplay(SAU_resid112)
summary(SAU_arima112)
plot(SAU_ts[,1])
lines(fitted(SAU_arima112), col=2)
# adding 2 AR #
SAU_arima212 <- Arima(SAU_ts[,1], order = c(2,1,2))
SAU_resid212 <- residuals(SAU_arima212)
tsdisplay(SAU_resid212)
summary(SAU_arima212)
plot(SAU_ts[,1])
lines(fitted(SAU_arima212), col=2)
# adding 3 MA #
SAU_arima113 <- Arima(SAU_ts[,1], order = c(1,1,3))
SAU_resid113 <- residuals(SAU_arima113)
tsdisplay(SAU_resid113)
summary(SAU_arima113)
plot(SAU_ts[,1])
lines(fitted(SAU_arima113), col=2)
# adding 2 AR #
SAU_arima213 <- Arima(SAU_ts[,1], order = c(2,1,3))
SAU_resid213 <- residuals(SAU_arima213)
tsdisplay(SA_resid213)
summary(SA_arima213)
plot(SA_ts[,1])
lines(fitted(SA_arima213), col=2)
# adding 3 AR #
SAU_arima313 <- Arima(SAU_ts[,1], order = c(3,1,3))
SAU_resid313 <- residuals(SAU_arima313)
tsdisplay(SAU_resid313)
summary(SAU_arima313)
plot(SAU_ts[,1])
lines(fitted(SAU_arima313), col=2)

#Comparing the AICs
AIC(SAU_arima010)
AIC(SAU_arima110)
AIC(SAU_arima111)
AIC(SAU_arima211)
AIC(SAU_arima112)
AIC(SAU_arima212)
AIC(SAU_arima113)
AIC(SAU_arima213)
AIC(SAU_arima313)


# FORECASTING WITH BEST MODEL #
sau.fitt <- fitted(SAU_arima010)
sau.pro <- zoo(x=SAU_ts[,1], order.by=index(sau.fitt))

for_SAU <- forecast(SAU_arima010)
plot(for_SAU)
lines(sau.fitt, col="red")
fore_arima_SAU <- forecast(SAU_arima010)
plot(fore_arima_SAU)
forecast(fore_arima_SAU)
checkresiduals(for_SAU)
# using auto arima #
SAU_auto_arima <- auto.arima(SAU_ts[,1])
SAU_auto_resid <- residuals(SAU_auto_arima)
tsdisplay(SAU_auto_resid)
summary(SAU_auto_arima)
# FORECASTING WITH AUTO MODEL #
forecast(SAU_auto_arima)
plot(forecast(SAU_auto_arima))
checkresiduals(SAU_auto_arima)
summary(SAU_auto_arima)


#-------------------ARIMA on IRAN----------------------#
production <- IRN_ts[,1]
plot(production)
Acf(production)
Pacf(production)
# trying 1 differentiation #
IRN_arima010 <- Arima(IRN_ts[,1], order = c(0,1,0)) 
IRN_resid010 <- residuals(IRN_arima010)
tsdisplay(IRN_resid010)
summary(IRN_arima010)
plot(IRN_ts[,1])
lines(fitted(IRN_arima010), col=2)
# adding 1  AR #
IRN_arima110 <- Arima(IRN_ts[,1], order = c(1,1,0))
IRN_resid110 <- residuals(IRN_arima110)
tsdisplay(IRN_resid110)
summary(IRN_arima110)
plot(IRN_ts[,1])
lines(fitted(IRN_arima110), col=2)
# adding 1 MA #
IRN_arima111 <- Arima(IRN_ts[,1], order = c(1,1,1))
IRN_resid111 <- residuals(IRN_arima111)
tsdisplay(IRN_resid111)
summary(IRN_arima111)
plot(IRN_ts[,1])
lines(fitted(IRN_arima111), col=2)
# adding 2 AR #
IRN_arima211 <- Arima(IRN_ts[,1], order = c(2,1,1))
IRN_resid211 <- residuals(IRN_arima211)
tsdisplay(IRN_resid211)
summary(IRN_arima211)
plot(IRN_ts[,1])
lines(fitted(IRN_arima211), col=2)
# adding 2 MA #
IRN_arima112 <- Arima(IRN_ts[,1], order = c(1,1,2))
IRN_resid112 <- residuals(IRN_arima112)
tsdisplay(IRN_resid112)
summary(IRN_arima112)
plot(IRN_ts[,1])
lines(fitted(IRN_arima112), col=2)
# adding 2 AR #
IRN_arima212 <- Arima(IRN_ts[,1], order = c(2,1,2))
IRN_resid212 <- residuals(IRN_arima212)
tsdisplay(IRN_resid212)
summary(IRN_arima212)
plot(IRN_ts[,1])
lines(fitted(IRN_arima212), col=2)
# adding 3 MA #
IRN_arima113 <- Arima(IRN_ts[,1], order = c(1,1,3))
IRN_resid113 <- residuals(IRN_arima113)
tsdisplay(IRN_resid113)
summary(IRN_arima113)
plot(IRN_ts[,1])
lines(fitted(IRN_arima113), col=2)
# adding 2 AR #
IRN_arima213 <- Arima(IRN_ts[,1], order = c(2,1,3))
IRN_resid213 <- residuals(IRN_arima213)
tsdisplay(IRN_resid213)
summary(IRN_arima213)
plot(IRN_ts[,1])
lines(fitted(IRN_arima213), col=2)
# adding 4 MA #
IRN_arima214 <- Arima(IRN_ts[,1], order = c(2,1,4))
IRN_resid214 <- residuals(IRN_arima214)
tsdisplay(IRN_resid214)
summary(IRN_arima214)
plot(IRN_ts[,1])
lines(fitted(IRN_arima214), col=2)
#Comparing the AICs
AIC(IRN_arima010)
AIC(IRN_arima110)
AIC(IRN_arima111)
AIC(IRN_arima211)
AIC(IRN_arima112)
AIC(IRN_arima212)
AIC(IRN_arima113)
AIC(IRN_arima213)
AIC(IRN_arima214)


# FORECASTING WITH BEST MODEL #
irn.fitt <- fitted(IRN_arima113)
irn.pro <- zoo(x=IRN_ts[,1], order.by=index(irn.fitt))

for_IRN <- forecast(IRN_arima113)
plot(for_IRN)
lines(irn.fitt, col="red")
fore_arima_IRN <- forecast(IRN_arima113)
plot(fore_arima_IRN)
forecast(fore_arima_IRN)
checkresiduals(fore_arima_IRN)
# using auto arima #
IRN_auto_arima <- auto.arima(IRN_ts[,1])
IRN_auto_resid <- residuals(IRN_auto_arima)
tsdisplay(IRN_auto_resid)
summary(IRN_auto_arima)
# FORECASTING WITH AUTO MODEL #
forecast(IRN_auto_arima)
plot(forecast(IRN_auto_arima))
checkresiduals(IRN_auto_arima)
summary(IRN_auto_arima)

#-----------------------------------------------GAM---------------------------------------------#
#---------------------GAM FOR USA----------------------#
# Simple Linear model #
simple_usa_gam <- gam(production~ consumption + gdp + reserves, data=train_usa)
summary(simple_usa_gam)
par(mfrow=c(2,2))
plot(simple_usa_gam, se=T)
AIC(simple_usa_gam)

# GAM with smoothing splines applied to all the variables #
ss_usa_gam <- gam(production~ s(gdp) + s(consumption) + s(reserves), data=train_usa)
summary(ss_usa_gam)
par(mfrow=c(2,2))
plot(ss_usa_gam, se=T)
AIC(ss_usa_gam)

# 3rd GAM with loess applied to reserves variable #
lo_usa_gam <- gam(production~ lo(gdp) + lo(consumption) + lo(reserves), data=train_usa)
summary(lo_usa_gam)
par(mfrow=c(2,2))
plot(lo_usa_gam, se=T)
AIC(lo_usa_gam)

# 3rd GAM with splines applied only to reserves variable and loess applied only on gdp based on the Anova report #
los_usa_gam <- gam(production~ lo(gdp) + consumption + s(reserves), data=train_usa) #BEST
summary(los_usa_gam)
par(mfrow=c(2,2))
plot(los_usa_gam, se=T)

AIC(simple_usa_gam)
AIC(ss_usa_gam)
AIC(lo_usa_gam)
AIC(los_usa_gam)

anova(los_usa_gam, ss_usa_gam, test = "Chisq")

tsdisplay(residuals(los_usa_gam))
dwtest(los_usa_gam)

los_usa_gam_fit <- fitted(los_usa_gam)
plot(train_usa$production)
lines(los_usa_gam_fit, col=2)

los_usa_gam_fit <- cbind.data.frame(train_usa$production,los_usa_gam_fit, train_usa$years)
colnames(los_usa_gam_fit) <- c("production", "fitted","years")

los_usa.pred <- predict(los_usa_gam, test_usa)
los_usa.pred <- cbind.data.frame(test_usa$production, los_usa.pred, test_usa$years)
colnames(los_usa.pred) <- c("production", "pred", "years")

ggplot() + 
  geom_line(data=los_usa_gam_fit, aes(x=years, y=production, color='Actual Train'), na.rm=TRUE) +
  geom_line(data=los_usa_gam_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = los_usa.pred, aes(x=years, y=production, color='Actual Test'), na.rm=TRUE) +
  geom_line(data = los_usa.pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE)

#---------------------------GAM FOR SAUDI ARABIA------------#
# simple GAM #
simple_sau_gam <- gam(production~gdp + consumption + reserves, data=train_sau)
summary(simple_sau_gam)
par(mfrow=c(2,2))
plot(simple_sau_gam, se=T)
AIC(simple_sau_gam)

# 2nd GAM with smoothing splines applied to all the variables #
ss_sau_gam <- gam(production~ s(gdp) + s(consumption) + s(reserves), data=train_sau)
summary(ss_sau_gam)
par(mfrow=c(2,2))
plot(ss_sau_gam, se=T)
AIC(ss_sau_gam)

# 3rd GAM with loess applied to all the variable #
lo_sau_gam <- gam(production~ lo(gdp) + lo(consumption) + lo(reserves), data=train_sau)
summary(lo_sau_gam)
par(mfrow=c(2,2))
plot(lo_sau_gam, se=T)
AIC(lo_sau_gam)

#  GAM with spline applied only gdp variable and consumption #
s_sau_gam <- gam(production~ s(gdp) + s(consumption) + reserves, data=train_sau) #BEST
summary(s_sau_gam)
par(mfrow=c(2,2))
plot(s_sau_gam, se=T)

AIC(simple_sau_gam)
AIC(ss_sau_gam)
AIC(lo_sau_gam)
AIC(s_sau_gam)

anova(simple_sau_gam, s_sau_gam, test = "Chisq")

tsdisplay(residuals(s_sau_gam))
dwtest(s_sau_gam)

s_sau_gam_fit <- fitted(s_sau_gam)
plot(train_sau$production)
lines(s_sau_gam_fit, col=2)

s_sau_gam_fit <- cbind.data.frame(train_sau$production,s_sau_gam_fit, train_sau$years)
colnames(s_sau_gam_fit) <- c("production", "fitted","years")

s_sau.pred <- predict(s_sau_gam, test_sau)
s_sau.pred <- cbind.data.frame(test_sau$production, s_sau.pred, test_sau$years)
colnames(s_sau.pred) <- c("production", "pred", "years")

ggplot() + 
  geom_line(data=s_sau_gam_fit, aes(x=years, y=production, color='Actual Train'), na.rm=TRUE) +
  geom_line(data=s_sau_gam_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = s_sau.pred, aes(x=years, y=production, color='Actual Test'), na.rm=TRUE) +
  geom_line(data = s_sau.pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE)

#------------GAM FOR IRAN------------#
# simple GAM #
simple_irn_gam <- gam(production~gdp + consumption + reserves, data=train_irn)
summary(simple_irn_gam)
par(mfrow=c(2,2))
plot(simple_irn_gam, se=T)
AIC(simple_irn_gam)

# 2nd GAM with smoothing splines applied to all the variables #
ss_irn_gam <- gam(production~ s(gdp) + s(consumption) + s(reserves), data=train_irn)
summary(ss_irn_gam)
par(mfrow=c(2,2))
plot(ss_irn_gam, se=T)
AIC(ss_irn_gam)

# 3rd GAM with loess applied to all the variable #
lo_irn_gam <- gam(production~ lo(gdp) + lo(consumption) + lo(reserves), data=train_irn)
summary(lo_irn_gam)
par(mfrow=c(2,2))
plot(lo_irn_gam, se=T)
AIC(lo_irn_gam)

#  GAM with splines applied to reserves variable without reserves #
s_irn_gam <- gam(production~ gdp + s(consumption) + s(reserves), data=train_irn) 
summary(s_irn_gam)
par(mfrow=c(2,2))
plot(s_irn_gam, se=T)

AIC(simple_irn_gam)
AIC(ss_irn_gam)
AIC(lo_irn_gam)
AIC(s_irn_gam)

anova(simple_irn_gam, s_irn_gam, test = "Chisq")

tsdisplay(residuals(s_irn_gam))
dwtest(s_irn_gam)

s_irn_gam_fit <- fitted(s_irn_gam)
plot(train_irn$production)
lines(s_irn_gam_fit, col=2)

s_irn_gam_fit <- cbind.data.frame(train_irn$production,s_irn_gam_fit, train_irn$years)
colnames(s_irn_gam_fit) <- c("production", "fitted","years")

s_irn.pred <- predict(s_irn_gam, test_irn)
s_irn.pred <- cbind.data.frame(test_irn$production, s_irn.pred, test_irn$years)
colnames(s_irn.pred) <- c("production", "pred", "years")

ggplot() + 
  geom_line(data=s_irn_gam_fit, aes(x=years, y=production, color='Actual Train'), na.rm=TRUE) +
  geom_line(data=s_irn_gam_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = s_irn.pred, aes(x=years, y=production, color='Actual Test'), na.rm=TRUE) +
  geom_line(data = s_irn.pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE)

#The End#
