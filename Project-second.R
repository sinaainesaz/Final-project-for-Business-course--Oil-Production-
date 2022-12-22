library(dplyr)
library(tidyverse)
library(caret)
library(ggplot2)
library(corrplot)
library(forecast)
library(lmtest) 
library(fpp2)
library(zoo)
library(minpack.lm)
library(numDeriv)
library(forecast)
library(reshape2)
library(deSolve)
library(DIMORA)
library(splines)
library(foreach)
library(gam)

#---Importing the Complete Data sets---#

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


head(Reserves)
# exploring #
options(repr.plot.width=25, repr.plot.height=6)

ggplot(Reserves, aes(x=Years))+
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

#---Making dataframe for selected countries---#
# USA #
production <- Production$USA
consumption <- Consumption$USA
#reserves <- Reserves$USA
gdp <- GDP$USA
years <- Production$Years
USA <- cbind.data.frame(production, consumption, gdp, years)
USA
min(USA$years)
typeof(USA)
USA[-4]
USA_ts <- ts(USA[-4],start = min(USA$years),end=max(USA$years))
USA_ts
# checking on NAn value #
cat("NAs in Production:",sum(is.na(USA$production)), 
    "\nNAs in GDP:",sum(is.na(USA$gdp)),
    "\nNAs in Consumption:", sum(is.na(USA$consumption)),
    "\nNAs in years:", sum(is.na(USA$years)))


USA = USA[complete.cases(USA),]
#complete.cases() function in R Language is used to return a logical vector with cases which are complete, i.e., no missing value
row.names(USA) <- NULL #reset the index
head(USA)
# Plotting time series for USA #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(USA_ts)
# Checking on the ACF of USA #
acf(USA[-4], na.action=na.pass)
# Plottig all the variables of the USA dataset #
options(repr.plot.width=25, repr.plot.height=6)
norm.USA <- cbind.data.frame(scale(USA[-4]), USA$years)

ggplot(norm.USA, aes(x=USA$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption"))+
  ggtitle("Features after Normalization")
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(USA[-4], use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)


# Saudi Arabia #
production <- Production$SAU
consumption <- Consumption$SAU
#reserves <- Reserves$SAU
gdp <- GDP$SAU
years <- Production$Years
SAU <- cbind.data.frame(production, consumption, gdp, years)
SAU
min(SAU$years)
SAU_ts <- ts(SAU[-4],start = min(SAU$years),end=max(SAU$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(SAU$production)), 
    "\nNAs in GDP:",sum(is.na(SAU$gdp)),
    "\nNAs in Consumption:", sum(is.na(SAU$consumption)),
    "\nNAs in years:", sum(is.na(SAU$years)))


SAU = SAU[complete.cases(SAU),]
row.names(SAU) <- NULL #reset the index
head(SAU)
# Plotting time series for Saudi Arabia #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(SAU_ts)
# Checking on the ACF of Saudi Arabia #
acf(SAU[-4], na.action=na.pass)
# Plottig all the variables of the Saudi Arabia dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.SAU <- cbind.data.frame(scale(SAU[-5]), SAU$years)

ggplot(norm.SAU, aes(x=SAU$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(SAU[-4], use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)

# Iran #
production <- Production$IRN
consumption <- Consumption$IRN
#reserves <- Reserves$IRN
gdp <- GDP$IRN
years <- Production$Years
IRN <- cbind.data.frame(production, consumption, gdp, years)
IRN
min(IRN$years)
typeof(IRN)
IRN_ts <- ts(IRN[-4],start = min(IRN$years),end=max(IRN$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(IRN$production)), 
    "\nNAs in GDP:",sum(is.na(IRN$gdp)),
    "\nNAs in Consumption:", sum(is.na(IRN$consumption)),
    "\nNAs in years:", sum(is.na(IRN$years)))


IRN = IRN[complete.cases(IRN),]
row.names(IRN) <- NULL #reset the index
head(IRN)
# Plotting time series for Iran #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(IRN_ts)
# Checking on the ACF of Iran #
acf(IRN[-4], na.action=na.pass)
# Plottig all the variables of the Iran dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.IRN <- cbind.data.frame(scale(IRN[-4]), IRN$years)

ggplot(norm.IRN, aes(x=IRN$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(IRN[-4], use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)
# Iraq #
production <- Production$IRQ
consumption <- Consumption$IRQ
#reserves <- Reserves$IRQ
gdp <- GDP$IRQ
years <- Production$Years
IRQ <- cbind.data.frame(production, consumption, gdp, years)
IRQ
min(IRQ$years)
typeof(IRQ)
IRQ_ts <- ts(IRQ[-4],start = min(IRQ$years),end=max(IRQ$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(IRQ$production)), 
    "\nNAs in GDP:",sum(is.na(IRQ$gdp)),
    "\nNAs in Consumption:", sum(is.na(IRQ$consumption)),
    "\nNAs in years:", sum(is.na(IRQ$years)))


IRQ = IRQ[complete.cases(IRQ),]
row.names(IRQ) <- NULL #reset the index
head(IRQ)
# Plotting time series for Iraq #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(IRQ_ts)
# Checking on the ACF of Iraq #
acf(IRQ[-4], na.action=na.pass)
# Plottig all the variables of the Iraq dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.IRQ <- cbind.data.frame(scale(IRQ[-4]), IRQ$years)

ggplot(norm.IRQ, aes(x=IRQ$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(IRQ[-4], use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)

# Russia #
production <- Production$RUS
consumption <- Consumption$RUS
reserves <- Reserves$RUS
gdp <- GDP$RUS
years <- Production$Years
RUS <- cbind.data.frame(production, consumption, reserves, gdp, years)
RUS
min(RUS$years)
typeof(RUS)
RUS_ts <- ts(RUS[-5],start = min(RUS$years),end=max(RUS$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(RUS$production)), 
    "\nNAs in GDP:",sum(is.na(RUS$gdp)),
    "\nNAs in Consumption:", sum(is.na(RUS$consumption)),
    "\nNAs in Reserves:", sum(is.na(RUS$reserves)),
    "\nNAs in years:", sum(is.na(RUS$years)))


RUS = RUS[complete.cases(RUS),]
row.names(RUS) <- NULL #reset the index
head(RUS)
# Plotting time series for Russia #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(RUS_ts)
# Checking on the ACF of Russia #
acf(RUS[-5], na.action=na.pass)
# Plottig all the variables of the Russia dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.RUS <- cbind.data.frame(scale(RUS[-5]), RUS$years)

ggplot(norm.RUS, aes(x=RUS$years))+
  geom_line(aes(y=production, color="production"),na.rm = TRUE) +
  geom_line(aes(y=gdp, color="GDP"),na.rm = TRUE)+
  geom_line(aes(y=consumption, color="consumption"),na.rm = TRUE)+
  geom_line(aes(y=reserves, color="reserves"),na.rm = TRUE)+
  scale_color_discrete(name = "Features", labels = c(
    "#CD5C5C"="production", "#4B0082"="gdp", "#000080"="consumption", "#7FFF00"="reserves"))+
  ggtitle("Features after Normalization")
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(RUS[-5], use="pairwise.complete.obs")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr, method="color", col=col(200), 
         type="lower", order="hclust", 
         addCoef.col = "black", 
         tl.col="black", tl.srt=40, 
         sig.level = 0.01, insig = "blank", 
         diag=FALSE)

#---Splitting the data into Train and Test sets---#
set.seed(1)

train_usa <- USA %>% filter(USA$years >= min(USA$years) & USA$years<=2015)
test_usa <- USA %>% filter(USA$years >= 2015 )

train_sau <- SAU %>% filter(SAU$years >= min(SAU$years) & SAU$years<=2005)
test_sau <- SAU %>% filter(SAU$years >= 2005 )

#train_rus <- RUS %>% filter(RUS$years >= min(RUS$years) & RUS$years<=2011)
#test_rus <- RUS %>% filter(RUS$years >= 2011 )

train_irn <- IRN %>% filter(IRN$years >= min(IRN$years) & IRN$years<=2003)
test_irn <- IRN %>% filter(IRN$years >= 2003 )

#train_irq <- IRQ %>% filter(IRQ$years >= min(IRQ$years) & IRQ$years<=2008)
#test_irq <- IRQ %>% filter(IRQ$years >= 2008 )

#---------------------------------Modelling and the Analysis----------------------------------------#

#------------------ Linear Regression---------------------#

#---------Linear Regression model for USA-------#

MLR_USA <- tslm(formula = production ~ poly(gdp,2) + poly(consumption,2), data=window(USA_ts, start=1965, end=2017 -.1))
summary(MLR_USA)
# On train set of USA #
LM_USA <- lm(formula= production ~ poly(gdp,2) + poly(consumption,2), data=train_usa)
summary(LM_USA)
fit_LM_USA <- fitted.values(LM_USA)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
usa_fit <- cbind.data.frame(train_usa$production,fit_LM_USA, train_usa$years)
colnames(usa_fit) <- c("production", "fitted","years")
ggplot(usa_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
usa_pred <- predict(LM_USA, test_usa)
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
plot(LM_USA)
par(mfrow=c(1,1))

#analysis of residuals
residuals_usa<- residuals(LM_USA) 
plot(residuals_usa, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_usa)
dw<- dwtest(LM_USA, alt="two.sided")
dw

#---------Linear Regression model for Russia-------#
MLR_RUS <- tslm(formula = production ~ gdp + consumption +reserves, data=window(RUS_ts, start=1991, end=2016 -.1))
summary(MLR_RUS)
# On train set of RUS #
LM_RUS <- lm(formula= production ~ gdp + consumption +reserves, data=train_rus)
summary(LM_RUS)
fit_LM_RUS <- fitted.values(LM_RUS)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
rus_fit <- cbind.data.frame(train_rus$production,fit_LM_RUS, train_rus$years)
colnames(rus_fit) <- c("production", "fitted","years")
ggplot(rus_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
rus_pred <- predict(LM_RUS, test_rus)
rus_pred <- cbind.data.frame(test_rus$production, rus_pred, test_rus$years)
colnames(rus_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=rus_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=rus_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = rus_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = rus_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE)+
  labs(title = "Actual, fitted and predicted values for RUSSIA")
# Useful plots #
par(mfrow=c(2,2))
plot(LM_RUS)
par(mfrow=c(1,1))

#analysis of residuals
residuals_rus<- residuals(LM_RUS) 
plot(residuals_rus, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_rus)
dw<- dwtest(LM_RUS, alt="two.sided")
dw

#------------Linear Regression model for Saudi Arabia ---------#
# Multiple linear model #
MLR_SAU <- tslm(formula = production ~ gdp + consumption +reserves, data=window(SAU_ts, start=1980, end=2009 -.1))
summary(MLR_SAU)
# Linear Model On train set of Saudi Arabia #
LM_SAU <- lm(formula= production ~ gdp + consumption +reserves, data=train_sau)
summary(LM_SAU)
fit_LM_SAU <- fitted.values(LM_SAU)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
sau_fit <- cbind.data.frame(train_sau$production,fit_LM_SAU, train_sau$years)
colnames(sau_fit) <- c("production", "fitted","years")
ggplot(sau_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
sau_pred <- predict(LM_SAU, test_sau)
sau_pred <- cbind.data.frame(test_sau$production, sau_pred, test_sau$years)
colnames(sau_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=sau_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=sau_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = sau_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = sau_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for SAUDI ARABIA")

par(mfrow=c(2,2))
plot(LM_SAU)
par(mfrow=c(1,1))

#analysis of residuals
residuals_sau<- residuals(LM_SAU) 
plot(residuals_sau, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_sau)
dw<- dwtest(LM_SAU, alt="two.sided")
dw


#-----------------Linear Regression model for IRAN -----------#
# Multiple linear model #
MLR_IRN_full <- tslm(formula = production ~ gdp + consumption +reserves, data=window(IRN_ts, start=1980, end=2009 -.1))
summary(MLR_IRN_full)
# without reserves #
MLR_IRN <- tslm(formula = production ~ gdp + consumption, data=window(IRN_ts, start=1980, end=2009 -.1))
summary(MLR_IRN)
# Linear Model On train set of IRAN #
LM_IRN <- lm(formula= production ~ gdp + consumption +reserves, data=train_irn)
summary(LM_SAU)
fit_LM_IRN <- fitted.values(LM_IRN)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
irn_fit <- cbind.data.frame(train_irn$production,fit_LM_IRN, train_irn$years)
colnames(irn_fit) <- c("production", "fitted","years")
ggplot(irn_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
irn_pred <- predict(LM_IRN, test_irn)
irn_pred <- cbind.data.frame(test_irn$production, irn_pred, test_irn$years)
colnames(irn_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=irn_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=irn_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = irn_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = irn_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for IRAN")

par(mfrow=c(2,2))
plot(LM_IRN)
par(mfrow=c(1,1))

#analysis of residuals
residuals_irn<- residuals(LM_IRN) 
plot(residuals_irn, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_irn)
dw<- dwtest(LM_IRN, alt="two.sided")
dw

#------------Linear Regression model for IRAQ ---------#
# Multiple linear model #
MLR_IRQ_full <- tslm(formula = production ~ gdp + consumption +reserves, data=window(IRQ_ts, start=1980, end=2009 -.1))
summary(MLR_IRQ_full)
# with gdp polynomial equal to 2 and without reserves and consumption #
MLR_IRQ <- tslm(formula = production ~ poly(gdp,2), data=window(IRQ_ts, start=1980, end=2016 -.1))
summary(MLR_IRQ)
# Linear Model On train set of IRAQ #
LM_IRQ_full <- lm(formula= production ~ gdp + consumption +reserves, data=train_irq)
summary(LM_IRQ_full)
# with gdp polynomial equal to 2 and without reserves and consumption #
LM_IRQ <- lm(formula= production ~ poly(gdp,2), data=train_irq)
summary(LM_IRQ)
fit_LM_IRQ <- fitted.values(LM_IRQ)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
irq_fit <- cbind.data.frame(train_irq$production,fit_LM_IRQ, train_irq$years)
colnames(irq_fit) <- c("production", "fitted","years")
ggplot(irq_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
irq_pred <- predict(LM_IRQ, test_irq)
irq_pred <- cbind.data.frame(test_irq$production, irq_pred, test_irq$years)
colnames(irq_pred) <- c("production", "pred", "years")
# Plotting everything together #
ggplot() + 
  geom_line(data=irq_fit, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data=irq_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = irq_pred, aes(x=years, y=production, color='Actual'), na.rm=TRUE) +
  geom_line(data = irq_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) +
  labs(title = "Actual, fitted and predicted values for IRAQ")

par(mfrow=c(2,2))
plot(LM_IRQ)
par(mfrow=c(1,1))

#analysis of residuals
residuals_irq<- residuals(LM_IRQ) 
plot(residuals_irq, type = "b") 
#the form of residuals indicates the presence of positive autocorrelation
Acf(residuals_irq)
dw<- dwtest(LM_IRQ, alt="two.sided")
dw
#----------------------------------------Diffusion Models-------------------------------------------#

#---------------------Bass standard model on USA to get the coefficents-------------------#
BM_USA <- BM(USA$production, display=T)
summary(BM_USA)
coef(BM_USA)
predicted_BM_USA <- predict(BM_USA,newx=c(1:60))
plot(BM_USA)
pred.instBM_USA<- make.instantaneous(predicted_BM_USA)
plot.Dimora(BM_USA)
BM_USA_fitted<- fitted(BM_USA)

#par(mfrow=c(1,1))
#plot(USA$years, USA$production, type="b", xlab="Year", ylab="Annual Production",  pch=16, lty=3, cex=0.6)
#lines(pred.instBM_USA)

# GBM with 1 exponantial shock #
GBMe1_USA<- GBM(USA$production,shock = "exp",nshock = 1,prelimestimates = c(BM(USA$production, display=FALSE)$Estimate[1,1],
                                                                            BM(USA$production, display=FALSE)$Estimate[2,1],
                                                                            BM(USA$production, display=FALSE)$Estimate[3,1],
                                                                            17,-0.1,0.1))
summary(GBMe1_USA)
coef(GBMe1_USA)
predicted_BMe1_USA <- predict(GBMe1_USA,newx=c(1:60))
plot.Dimora(GBMe1_USA)
# GBM with mixed shocks shocks for USA #
GBM_mix_USA<- GBM(USA$production, shock = "mixed", nshock = 2, 
                  prelimestimates = c(BM_USA$Estimate[1,1],
                                      BM_USA$Estimate[2,1],
                                      BM_USA$Estimate[3,1],
                                      6,-0.1,0.1, 20,30,0.1), oos=0)
summary(GBM_mix_USA)
GBM_USA_fitted<- fitted(GBM_mix_USA)
coef(GBM_mix_USA)

predictedGBM_mix_USA<- predict(GBM_mix_USA,newx = c(1:60))
plot(GBM_mix_USA, type = "all")
pred.instGBM_USA<- make.instantaneous(predictedGBM_mix_USA)
#par(mfrow=c(1,1))
#plot(USA$years, USA$production, type="b", xlab="Year", ylab="Annual Production",  pch=16, lty=3, cex=0.6)
#lines(pred.instGBM_USA)
# Graphical inspection of BM_USA e GBM_mix_USA #
BM_models_fitted_USA <- cbind.data.frame(GBM_USA_fitted, BM_USA_fitted, USA$years)
colnames(BM_models_fitted_USA) <- c("gbm", "bm", "years")

ggplot()+
  geom_line(data=train_usa, aes(x= years,y=production, color="Actual Train"))+
  geom_line(data=test_usa, aes(x= years,y=production, color="Actual Test"))+
  
  geom_line(data=BM_models_fitted_USA, aes(x=years,y=gbm, color="GBM"))+
  
  geom_line(data=BM_models_fitted_USA, aes(x=years,y=bm, color="BM"))
####residual analysis of GBM for USA #
resGBM_USA<- residuals(GBM_mix_USA)
plot(resGBM_USA, type="b")
Acf(resGBM_USA)
Pacf(resGBM_USA)

#---------------------Bass standard model on SAUDI ARABIA to get the coefficents-------------------#
BM_SAU <- BM(SAU$production, display=T)
summary(BM_SAU)
coef(BM_SAU)
predicted_BM_SAU <- predict(BM_SAU,newx=c(1:60))
plot(BM_SAU)
pred.instBM_SAU<- make.instantaneous(predicted_BM_SAU)
plot.Dimora(BM_SAU)
BM_SAU_fitted<- fitted(BM_SAU)

#par(mfrow=c(1,1))
#plot(SAU$years, SAU$production, type="b", xlab="Year", ylab="Annual Production",  pch=16, lty=3, cex=0.6)
#lines(pred.instBM_SAU)

# GBM with 1 exponantial shock #
GBMe1_SAU<- GBM(SAU$production,shock = "exp",nshock = 1,prelimestimates = c(BM(USA$production, display=FALSE)$Estimate[1,1],
                                                                            BM(USA$production, display=FALSE)$Estimate[2,1],
                                                                            BM(USA$production, display=FALSE)$Estimate[3,1],
                                                                            17,-0.1,0.1))
summary(GBMe1_SAU)
coef(GBMe1_SAU)
predicted_BMe1_SAU <- predict(GBMe1_SAU,newx=c(1:60))
plot.Dimora(GBMe1_SAU)
# GBM with mixed shocks shocks for SAU #
GBM_mix_SAU<- GBM(SAU$production, shock = "mixed", nshock = 2, 
                  prelimestimates = c(BM_SAU$Estimate[1,1],
                                      BM_SAU$Estimate[2,1],
                                      BM_SAU$Estimate[3,1],
                                      6,-0.1,0.1, 20,30,0.1), oos=0)
summary(GBM_mix_SAU)
GBM_SAU_fitted<- fitted(GBM_mix_SAU)
coef(GBM_mix_SAU)

predictedGBM_mix_SAU<- predict(GBM_mix_SAU,newx = c(1:60))
plot(GBM_mix_SAU, type = "all")
pred.instGBM_SAU<- make.instantaneous(predictedGBM_mix_SAU)
#par(mfrow=c(1,1))
#plot(SAU$years, SAU$production, type="b", xlab="Year", ylab="Annual Production",  pch=16, lty=3, cex=0.6)
#lines(pred.instGBM_SAU)
# Graphical inspection of BM_SAU e GBM_mix_SAU #
BM_models_fitted_SAU <- cbind.data.frame(GBM_SAU_fitted, BM_SAU_fitted, SAU$years)
colnames(BM_models_fitted_SAU) <- c("gbm", "bm", "years")

ggplot()+
  geom_line(data=train_sau, aes(x= years,y=production, color="Actual Train"))+
  geom_line(data=test_sau, aes(x= years,y=production, color="Actual Test"))+
  
  geom_line(data=BM_models_fitted_SAU, aes(x=years,y=gbm, color="GBM"))+
  
  geom_line(data=BM_models_fitted_SAU, aes(x=years,y=bm, color="BM"))
####residual analysis of GBM for SAU #
resGBM_SAU<- residuals(GBM_mix_SAU)
plot(resGBM_SAU, type="b")
Acf(resGBM_SAU)
Pacf(resGBM_SAU)

#---------------------Bass standard model on IRAN to get the coefficents-------------------#
BM_IRN <- BM(IRN$production, display=T)
summary(BM_IRN)
coef(BM_IRN)
predicted_BM_IRN <- predict(BM_IRN,newx=c(1:60))
plot(BM_IRN)
pred.instBM_IRN<- make.instantaneous(predicted_BM_IRN)
plot.Dimora(BM_IRN)
BM_IRN_fitted<- fitted(BM_IRN)

#par(mfrow=c(1,1))
#plot(IRN$years, IRN$production, type="b", xlab="Year", ylab="Annual Production",  pch=16, lty=3, cex=0.6)
#lines(pred.instBM_IRN)

# GBM with 1 exponantial shock #
GBMe1_IRN<- GBM(IRN$production,shock = "exp",nshock = 1,prelimestimates = c(BM(IRN$production, display=FALSE)$Estimate[1,1],
                                                                            BM(IRN$production, display=FALSE)$Estimate[2,1],
                                                                            BM(IRN$production, display=FALSE)$Estimate[3,1],
                                                                            17,-0.1,0.1))
summary(GBMe1_IRN)
coef(GBMe1_IRN)
predicted_BMe1_IRN <- predict(GBMe1_IRN,newx=c(1:60))
plot.Dimora(GBMe1_IRN)
# GBM with mixed shocks shocks for IRN #
GBM_mix_IRN<- GBM(IRN$production, shock = "mixed", nshock = 2, 
                  prelimestimates = c(BM_IRN$Estimate[1,1],
                                      BM_IRN$Estimate[2,1],
                                      BM_IRN$Estimate[3,1],
                                      6,-0.1,0.1, 20,30,0.1), oos=0)
summary(GBM_mix_IRN)
GBM_IRN_fitted<- fitted(GBM_mix_IRN)
coef(GBM_mix_IRN)

predictedGBM_mix_IRN<- predict(GBM_mix_IRN,newx = c(1:60))
plot(GBM_mix_IRN, type = "all")
pred.instGBM_IRN<- make.instantaneous(predictedGBM_mix_IRN)
#par(mfrow=c(1,1))
#plot(IRN$years, IRN$production, type="b", xlab="Year", ylab="Annual Production",  pch=16, lty=3, cex=0.6)
#lines(pred.instGBM_IRN)
# Graphical inspection of BM_IRN e GBM_mix_IRN #
BM_models_fitted_IRN <- cbind.data.frame(GBM_IRN_fitted, BM_IRN_fitted, IRN$years)
colnames(BM_models_fitted_IRN) <- c("gbm", "bm", "years")

ggplot()+
  geom_line(data=train_irn, aes(x= years,y=production, color="Actual Train"))+
  geom_line(data=test_irn, aes(x= years,y=production, color="Actual Test"))+
  
  geom_line(data=BM_models_fitted_IRN, aes(x=years,y=gbm, color="GBM"))+
  
  geom_line(data=BM_models_fitted_IRN, aes(x=years,y=bm, color="BM"))
####residual analysis of GBM for IRN #
resGBM_IRN<- residuals(GBM_mix_IRN)
plot(resGBM_IRN, type="b")
Acf(resGBM_IRN)
Pacf(resGBM_IRN)

#----------------------------------------ARIMA Models-----------------------------------------------#
#-------------------ARIMA on USA----------------------#
production <- USA_ts[,1]
plot(production)
Acf(production)
Pacf(production)
# trying 1 differentation #
USA_arima1 <- Arima(USA_ts[,1], order = c(0,1,0)) 
USA_resid1 <- residuals(USA_arima1)
tsdisplay(USA_resid1)
summary(USA_arima1)
fitted(USA_arima1)
plot(USA_ts[,1])
lines(fitted(USA_arima1), col=2)
# adding 1  AR #
USA_arima2 <- Arima(USA_ts[,1], order = c(1,1,0))
USA_resid2 <- residuals(USA_arima2)
tsdisplay(USA_resid2)
summary(USA_arima2)
fitted(USA_arima2)
plot(USA_ts[,1])
lines(fitted(USA_arima2), col=2)
# adding 1 MA #
USA_arima3 <- Arima(USA_ts[,1], order = c(0,1,1)) #BEST
USA_resid3 <- residuals(USA_arima3)
tsdisplay(USA_resid3)
summary(USA_arima3)
fitted(USA_arima3)
plot(USA_ts[,1])
lines(fitted(USA_arima3), col=2)
# adding 1 AR and 1 MA #
USA_arima4 <- Arima(USA_ts[,1], order = c(1,1,1))
USA_resid4 <- residuals(USA_arima4)
tsdisplay(USA_resid4)
summary(USA_arima4)
fitted(USA_arima4)
plot(USA_ts[,1])
lines(fitted(USA_arima4), col=2)
AIC(USA_arima1)
AIC(USA_arima2)
AIC(USA_arima3)
AIC(USA_arima4)
# FORECASTING WITH BEST MODEL #
fore_arima_USA <- forecast(USA_arima3)
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
# trying 1 differentation #
SAU_arima1 <- Arima(SAU_ts[,1], order = c(0,1,0)) #BEST
SAU_resid1 <- residuals(SAU_arima1)
tsdisplay(SAU_resid1)
summary(SAU_arima1)
fitted(SAU_arima1)
plot(SAU_ts[,1])
lines(fitted(SAU_arima1), col=2)
# adding 1  AR #
SAU_arima2 <- Arima(SAU_ts[,1], order = c(1,1,0))
SAU_resid2 <- residuals(SAU_arima2)
tsdisplay(SAU_resid2)
summary(SAU_arima2)
fitted(SAU_arima2)
plot(SAU_ts[,1])
lines(fitted(SAU_arima2), col=2)
# adding 1 MA #
SAU_arima3 <- Arima(SAU_ts[,1], order = c(0,1,1)) 
SAU_resid3 <- residuals(SAU_arima3)
tsdisplay(SAU_resid3)
summary(SAU_arima3)
fitted(SAU_arima3)
plot(SAU_ts[,1])
lines(fitted(SAU_arima3), col=2)
# adding 1 AR and 1 MA #
SAU_arima4 <- Arima(SAU_ts[,1], order = c(1,1,1)) 
SAU_resid4 <- residuals(SAU_arima4)
tsdisplay(SAU_resid4)
summary(SAU_arima4)
fitted(SAU_arima4)
plot(SAU_ts[,1])
lines(fitted(SAU_arima4), col=2)
AIC(SAU_arima1)
AIC(SAU_arima2)
AIC(SAU_arima3)
AIC(SAU_arima4)
# FORECASTING WITH BEST MODEL #
fore_arima_SAU <- forecast(SAU_arima1)
plot(fore_arima_SAU)
forecast(fore_arima_SAU)
checkresiduals(fore_arima_SAU)
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
# trying 1 differentation #
IRN_arima1 <- Arima(IRN_ts[,1], order = c(0,1,0)) 
IRN_resid1 <- residuals(IRN_arima1)
tsdisplay(IRN_resid1)
summary(IRN_arima1)
fitted(IRN_arima1)
plot(IRN_ts[,1])
lines(fitted(IRN_arima1), col=2)
# adding 1  AR #
IRN_arima2 <- Arima(IRN_ts[,1], order = c(1,1,0)) 
IRN_resid2 <- residuals(IRN_arima2)
tsdisplay(IRN_resid2)
summary(IRN_arima2)
fitted(IRN_arima2)
plot(IRN_ts[,1])
lines(fitted(IRN_arima2), col=2)
# adding 1 MA #
IRN_arima3 <- Arima(IRN_ts[,1], order = c(0,1,1)) #BEST 
IRN_resid3 <- residuals(IRN_arima3)
tsdisplay(IRN_resid3)
summary(IRN_arima3)
fitted(IRN_arima3)
plot(IRN_ts[,1])
lines(fitted(IRN_arima3), col=2)
# adding 1 AR and 1 MA #
IRN_arima4 <- Arima(IRN_ts[,1], order = c(1,1,1)) 
IRN_resid4 <- residuals(IRN_arima4)
tsdisplay(IRN_resid4)
summary(IRN_arima4)
fitted(IRN_arima4)
plot(IRN_ts[,1])
lines(fitted(IRN_arima4), col=2)
AIC(USA_arima1)
AIC(USA_arima2)
AIC(USA_arima3)
AIC(USA_arima4)
# FORECASTING WITH BEST MODEL #
fore_arima_IRN <- forecast(IRN_arima3)
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
#-----------------------------------------------GAM------------------------------------------------#
#------------GAM FOR USA------------#
# simple GAM #
simple_usa_gam <- gam(production~gdp + consumption, data=train_usa)
summary(simple_usa_gam)
par(mfrow=c(2,2))
plot(simple_usa_gam, se=T)
AIC(simple_usa_gam)

# 2nd GAM with smoothing splines applied to all the variables #
ss_usa_gam <- gam(production~ s(gdp) + s(consumption), data=train_usa)
summary(ss_usa_gam)
par(mfrow=c(2,2))
plot(ss_usa_gam, se=T)
AIC(ss_usa_gam)

# 3rd GAM with loess applied to reserves variable #
lo_usa_gam <- gam(production~ lo(gdp) + lo(consumption), data=train_usa)
summary(lo_usa_gam)
par(mfrow=c(2,2))
plot(lo_usa_gam, se=T)
AIC(lo_usa_gam)

# 3rd GAM with splines applied only to reserves variable #
s_usa_gam <- gam(production~ s(gdp) + consumption, data=train_usa) #BEST
summary(s_usa_gam)
par(mfrow=c(2,2))
plot(s_usa_gam, se=T)

AIC(simple_usa_gam)
AIC(ss_usa_gam)
AIC(lo_usa_gam)
AIC(s_usa_gam)

anova(simple_usa_gam, s_usa_gam, test = "Chisq")

tsdisplay(residuals(s_usa_gam))
dwtest(s_usa_gam)

s_usa_gam_fit <- fitted(s_usa_gam)
plot(train_usa$production)
lines(s_usa_gam_fit, col=2)

s_usa_gam_fit <- cbind.data.frame(train_usa$production,s_usa_gam_fit, train_usa$years)
colnames(s_usa_gam_fit) <- c("production", "fitted","years")

s_usa.pred <- predict(s_usa_gam, test_usa)
s_usa.pred <- cbind.data.frame(test_usa$production, s_usa.pred, test_usa$years)
colnames(s_usa.pred) <- c("production", "pred", "years")

ggplot() + 
  geom_line(data=s_usa_gam_fit, aes(x=years, y=production, color='Actual Train'), na.rm=TRUE) +
  geom_line(data=s_usa_gam_fit, aes(x=years, y=fitted, color='Fitted'), na.rm=TRUE) +
  geom_line(data = s_usa.pred, aes(x=years, y=production, color='Actual Test'), na.rm=TRUE) +
  geom_line(data = s_usa.pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE)
#------------GAM FOR SAUDI ARABIA------------#
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

#  GAM with loess applied to gdp variable #
s_sau_gam <- gam(production~ s(gdp) + consumption + reserves, data=train_sau) #BEST
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
lo_irn_gam <- gam(production~ gdp + consumption + lo(reserves), data=train_irn)
summary(lo_irn_gam)
par(mfrow=c(2,2))
plot(lo_irn_gam, se=T)
AIC(lo_irn_gam)

#  GAM with splines applied to  variable #
s_irn_gam <- gam(production~ gdp + consumption + lo(reserves), data=train_irn) 
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
