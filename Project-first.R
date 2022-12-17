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
# Plotting time series for USA #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(USA_ts)
# Checking on the ACF of USA #
acf(USA[-5], na.action=na.pass)
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
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(USA[-5], use="pairwise.complete.obs")

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
acf(SAU[-5], na.action=na.pass)
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
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(SAU[-5], use="pairwise.complete.obs")

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
reserves <- Reserves$IRN
gdp <- GDP$IRN
years <- Production$Years
IRN <- cbind.data.frame(production, consumption, reserves, gdp, years)
IRN
min(IRN$years)
typeof(IRN)
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
# Plotting time series for Iran #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(IRN_ts)
# Checking on the ACF of Iran #
acf(IRN[-5], na.action=na.pass)
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
# Checking of the Correlation of the Features #
par(mfrow=c(1,1))
options(repr.plot.width=6, repr.plot.height=7)
corr <- cor(IRN[-5], use="pairwise.complete.obs")

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
reserves <- Reserves$IRQ
gdp <- GDP$IRQ
years <- Production$Years
IRQ <- cbind.data.frame(production, consumption, reserves, gdp, years)
IRQ
min(IRQ$years)
typeof(IRQ)
IRQ_ts <- ts(IRQ[-5],start = min(IRQ$years),end=max(IRQ$years))

# checking on NAn value #
cat("NAs in Production:",sum(is.na(IRQ$production)), 
    "\nNAs in GDP:",sum(is.na(IRQ$gdp)),
    "\nNAs in Consumption:", sum(is.na(IRQ$consumption)),
    "\nNAs in Reserves:", sum(is.na(IRQ$reserves)),
    "\nNAs in years:", sum(is.na(IRQ$years)))


IRQ = IRQ[complete.cases(IRQ),]
row.names(IRQ) <- NULL #reset the index
head(IRQ)
# Plotting time series for Iraq #
options(repr.plot.width=8, repr.plot.height=7)
par(mfrow=c(1,3))
plot(IRQ_ts)
# Checking on the ACF of Iraq #
acf(IRQ[-5], na.action=na.pass)
# Plottig all the variables of the Iraq dataset #
par(mfrow=c(1,1))
options(repr.plot.width=25, repr.plot.height=6)
norm.IRQ <- cbind.data.frame(scale(IRQ[-5]), IRQ$years)

ggplot(norm.IRQ, aes(x=IRQ$years))+
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
corr <- cor(IRQ[-5], use="pairwise.complete.obs")

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

train_usa <- USA %>% filter(USA$years >= min(USA$years) & USA$years<=2010)
test_usa <- USA %>% filter(USA$years >= 2010 )

train_sau <- SAU %>% filter(SAU$years >= min(SAU$years) & SAU$years<=2010)
test_sau <- SAU %>% filter(SAU$years >= 2010 )

train_rus <- RUS %>% filter(RUS$years >= min(RUS$years) & RUS$years<=2010)
test_rus <- RUS %>% filter(RUS$years >= 2010 )

train_irn <- IRN %>% filter(IRN$years >= min(IRN$years) & IRN$years<=2010)
test_irn <- IRN %>% filter(IRN$years >= 2010 )

train_irq <- IRQ %>% filter(IRQ$years >= min(IRQ$years) & IRQ$years<=2010)
test_irq <- IRQ %>% filter(IRQ$years >= 2010 )

#---Modelling and the Analysis---#
# Linear Regression model for USA #
MLR_USA <- tslm(formula = production ~ gdp + consumption +reserves, data=window(USA_ts, start=1980, end=2011 -.1))
summary(MLR_USA)
# On train set of USA #
LM_USA <- lm(formula= production ~ gdp + consumption +reserves, data=train_usa)
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
  geom_line(data = usa_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) ## TODO: should be added a confidence intervall
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

# Linear Regression model for USA #
MLR_USA <- tslm(formula = production ~ gdp + consumption +reserves, data=window(USA_ts, start=1980, end=2011 -.1))
summary(MLR_USA)
# On train set of USA #
LM_USA <- lm(formula= production ~ gdp + consumption +reserves, data=train_usa)
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
  geom_line(data = usa_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE) ## TODO: should be added a confidence intervall
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

# Linear Regression model for Saudi Arabia #
# Multiple linear model #
MLR_SAU <- tslm(formula = production ~ gdp + consumption +reserves, data=window(SAU_ts, start=1980, end=2011 -.1))
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
  geom_line(data = sau_pred, aes(x=years, y=pred, color='Predicted'), na.rm=TRUE)

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

#---Diffusion Models---#

# Bass standard model on Oil Production of USA to get the coefficents #
BM_USA <- BM(USA$production, display=T)
summary(BM_USA)
coef(BM_USA)
predicted_BM_USA <- predict(BM_USA,newx=c(1:60))
plot(BM_USA)
pred.instBM_USA<- make.instantaneous(predicted_BM_USA)
plot.Dimora(BM_USA)

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
# Graphical inspection of BM_USA e GBMe1_USA #
BM_models_fitted <- cbind.data.frame(GBM_USA_fitted, BM_USA_fitted, USA$years)
colnames(BM_models_fitted) <- c("gbm", "bm", "years")

ggplot()+
  geom_line(data=train_usa, aes(x= years,y=production, color="Actual Train"))+
  geom_line(data=test_usa, aes(x= years,y=production, color="Actual Test"))+
  
  geom_line(data=BM_models_fitted, aes(x=years,y=gbm, color="GBM"))+

  geom_line(data=BM_models_fitted, aes(x=years,y=bm, color="BM"))
####residual analysis of GBM for USA #
resGBM_USA<- residuals(GBMe1_USA)
plot(resGBM_USA, type="b")
Acf(resGBM_USA)
Pacf(resGBM_USA)

#---ARIMA Models---#
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
# trying 2 differentation #
USA_arima2 <- Arima(USA_ts[,1], order = c(1,1,0))
USA_resid2 <- residuals(USA_arima2)
tsdisplay(USA_resid2)
summary(USA_arima2)
fitted(USA_arima2)
plot(USA_ts[,1])
lines(fitted(USA_arima2), col=2)
# adding 1 MA #
USA_arima3 <- Arima(USA_ts[,1], order = c(0,2,1))
USA_resid3 <- residuals(USA_arima3)
tsdisplay(USA_resid3)
summary(USA_arima3)
fitted(USA_arima3)
plot(USA_ts[,1])
lines(fitted(USA_arima3), col=2)
# adding 2 MA #
USA_arima4 <- Arima(USA_ts[,1], order = c(0,2,2))
USA_resid4 <- residuals(USA_arima4)
tsdisplay(USA_resid4)
summary(USA_arima4)
fitted(USA_arima4)
plot(USA_ts[,1])
lines(fitted(USA_arima4), col=2)
# adding 1 AR #
USA_arima5 <- Arima(USA_ts[,1], order = c(4,3,3))
USA_resid5 <- residuals(USA_arima5)
tsdisplay(USA_resid5)
summary(USA_arima5)
fitted(USA_arima5)
plot(USA_ts[,1])
lines(fitted(USA_arima5), col=2)
forecast(USA_arima5)
plot(forecast(USA_arima5))
lines(fitted(USA_arima5), col=2)
# using auto arima #
USA_auto_arima <- auto.arima(USA_ts[,1])
USA_auto_resid <- residuals(USA_auto_arima)
tsdisplay(USA_auto_resid)
summary(USA_auto_arima)

#---GAM---#
# simple GAM #
simple_usa_gam <- gam(production~gdp + consumption + reserves, data=train_usa)
summary(simple_usa_gam)
par(mfrow=c(2,2))
plot(simple_usa_gam, se=T)
AIC(simple_usa_gam)

# 2nd GAM with smoothing splines applied to all the variables #

ss_usa_gam <- gam(production~ gdp + consumption + s(reserves), data=train_usa)
summary(ss_usa_gam)
par(mfrow=c(2,2))
plot(ss_usa_gam, se=T)
AIC(ss_usa_gam)

# 3rd GAM with loess applied to all the variables #
lo_usa_gam <- gam(production~ lo(gdp) + lo(consumption) + lo(reserves), data=train_usa)
summary(lo_usa_gam)
par(mfrow=c(2,2))
plot(lo_usa_gam, se=T)
AIC(lo_usa_gam)
