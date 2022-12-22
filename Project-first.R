library(dplyr)
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
test_usa <- USA %>% filter(USA$years >= 2008 )

train_sau <- SAU %>% filter(SAU$years >= min(SAU$years) & SAU$years<=2008)
test_sau <- SAU %>% filter(SAU$years >= 2008 )

train_irn <- IRN %>% filter(IRN$years >= min(IRN$years) & IRN$years<=2008)
test_irn <- IRN %>% filter(IRN$years >= 2008 )

#----------------------------------Modelling and the Analysis--------------------------------------#

#---------------------------------------------------------#
#------------------ Linear Regression---------------------#
#---------------------------------------------------------#

#---------Linear Regression model for USA-------#

#Fitting linear model using only with the highest correlated variable with the target column#
MLR_USA_res <- tslm(production ~reserves, data=window(USA_ts, start=1980, end=2009 -.1))
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
MLR_USA_full <- tslm(production ~ consumption + poly(gdp,2) + reserves, data=window(USA_ts, start=1980, end=2009 -.1))
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
  labs(title = "Actual, fitted and predicted values for UNITED STATES")

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
  labs(title = "Actual, fitted and predicted values for SAUDI ARABIA")

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
  labs(title = "Actual, fitted and predicted values for SAUDI ARABIA")

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
MLR_IRN_gdp <- tslm(formula = production ~ consumption, data=window(IRN_ts, start=1980, end=2009 -.1))
summary(MLR_IRN_gdp)
# Linear Model On train set of IRAN #
LM1_IRN_gdp <- lm(formula= production ~ consumption, data=train_irn)
summary(LM1_IRN_gdp)
# Stepwise Regression
LM2_IRN_gdp <- step(LM1_IRN_gdp, direction="both")
summary(LM2_IRN_gdp)
AIC(MLR_IRN_gdp)
AIC(LM1_IRN_gdp)
AIC(LM2_IRN_gdp)

fit_LM_IRN_gdp <- fitted.values(LM2_IRN_gdp)
# Plotting the Fitted and Real values #
options(repr.plot.width=25, repr.plot.height=6)
irn_fit <- cbind.data.frame(train_irn$production,fit_LM_IRN_gdp, train_irn$years)
colnames(irn_fit) <- c("production", "fitted","years")
ggplot(irn_fit, aes(x=years))+
  geom_line(aes(y=production, color="Real"),na.rm = TRUE) +
  geom_line(aes(y=fitted, color="Fitted"),na.rm = TRUE)
# Predicting of test set #
irn_pred <- predict(LM2_IRN_gdp, test_irn)
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
plot(LM2_IRN_gdp)
par(mfrow=c(1,1))

#analysis of residuals
residuals_irn_gdp<- residuals(LM2_IRN_gdp) 
plot(residuals_irn_gdp, type = "b") 

Acf(residuals_irn_gdp)
dw<- dwtest(LM2_IRN_gdp, alt="two.sided")
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
  labs(title = "Actual, fitted and predicted values for IRAN")

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
BM_USA <- BM(Production$USA, display=T)
summary(BM_USA)
coef(BM_USA)

###prediction (out-of-sample)
pred_bmusa<- predict(BM_USA, newx=c(1:60))
pred.instusa<- make.instantaneous(pred_bmusa)
###plot of fitted model 
plot(Production$USA, type= "b",xlab="Year", ylab="Annual Oil production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$USA[c(1,10,20,30,40,51,61)])
lines(pred.instusa, lwd=2, col=2)

predicted_BM_USA <- predict(BM_USA,newx=c(1:60))
plot(BM_USA)
pred.instBM_USA<- make.instantaneous(predicted_BM_USA)
plot.Dimora(BM_USA)
BM_USA_fitted<- fitted(BM_USA)

###we estimate the model with 50% of the data
bm_usa50<-BM(Production$USA[1:31],display = T)
summary(bm_usa50)

pred_bmusa50<- predict(bm_usa50, newx=c(1:50))
pred.instusa50<- make.instantaneous(pred_bmusa50)

plot(Production$USA, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$USA[c(1,10,20,30,40,51,61)])
lines(pred.instusa50, lwd=2, col=2)

###we estimate the model with 25% of the data
bm_usa75<-BM(Production$USA[1:15],display = T)
summary(bm_usa75)

pred_bmusa75<- predict(bm_usa75, newx=c(1:50))
pred.instusa75<- make.instantaneous(pred_bmusa75)

###Comparison between models (instantaneous)
###instantaneous
plot(Production$USA, type= "b",xlab="Year", ylab="Annual Oil production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$USA[c(1,10,20,30,40,51,61)])
lines(pred.instusa75, lwd=2, col=2)
lines(pred.instusa50, lwd=2, col=3)
lines(pred.instusa, lwd=2, col=4)

# GBM with 1 exponantial shock #
GBMe1_USA<- GBM(Production$USA,shock = "exp",nshock = 1,prelimestimates = c(BM(Production$USA, display=FALSE)$Estimate[1,1],
                                                              BM(Production$USA, display=FALSE)$Estimate[2,1],
                                                              BM(Production$USA, display=FALSE)$Estimate[3,1],
                                                              17,-0.1,0.1))
summary(GBMe1_USA)
coef(GBMe1_USA)
predicted_BMe1_USA <- predict(GBMe1_USA,newx=c(1:60))
plot.Dimora(GBMe1_USA)
# GBM with mixed shocks shocks for USA #
GBM_mix_USA<- GBM(Production$USA, shock = "mixed", nshock = 2, 
                         prelimestimates = c(BM_USA$Estimate[1,1],
                                             BM_USA$Estimate[2,1],
                                             BM_USA$Estimate[3,1],
                                             6,-0.1,0.1, 20,30,0.1))
summary(GBM_mix_USA)
GBM_USA_fitted<- fitted(GBM_mix_USA)
coef(GBM_mix_USA)

predictedGBM_mix_USA<- predict(GBM_mix_USA,newx = c(1:60))
plot(GBM_mix_USA, type = "all")
pred.instGBM_USA<- make.instantaneous(predictedGBM_mix_USA)

####residual analysis of GBM for USA #
resGBM_USA<- residuals(GBM_mix_USA)
plot(resGBM_USA, type="b")
Acf(resGBM_USA)
Pacf(resGBM_USA)

### we fit a GGM (better model...but we need to interpret well the parameters)

GGM_USA<- GGM(Production$USA, prelimestimates=c(1.947320e+06, 0.001, 0.01, 2.825896e-03, -8.256157e-04))
summary(GGM_USA)
sum(production_usa)
pred_GGMUSA<- predict(GGM_USA, newx=c(1:60))
pred.instGGMUSA<- make.instantaneous(pred_GGMUSA)

plot(Production$USA, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$USA[c(1,10,20,30,40,51,61)])
lines(pred.instGGMUSA, lwd=2, col=2)


#---------------------Bass standard model on SAUDI ARABIA to get the coefficients-------------------#

# LIKE THE LAB SESSIONS#
# Estimating a simple bass model #
production_sau
BM_SAU <- BM(Production$SAU, display=T)
summary(BM_SAU)
coef(BM_SAU)

###prediction (out-of-sample)
pred_bmsau<- predict(BM_SAU, newx=c(1:60))
pred.instsau<- make.instantaneous(pred_bmsau)

###plot of fitted model 
plot(Production$SAU, type= "b",xlab="Year", ylab="Annual Oil production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$SAU[c(1,10,20,30,40,51,61)])
lines(pred.instsau, lwd=2, col=2)

predicted_BM_SAU <- predict(BM_SAU,newx=c(1:60))
plot(BM_SAU)
pred.instBM_SAU<- make.instantaneous(predicted_BM_SAU)
plot.Dimora(BM_SAU)
BM_SAU_fitted<- fitted(BM_SAU)

###we estimate the model with 50% of the data
bm_sau50<-BM(Production$SAU[1:31],display = T)
summary(bm_sau50)

pred_bmsau50<- predict(bm_sau50, newx=c(1:50))
pred.instsau50<- make.instantaneous(pred_bmsau50)

plot(Production$SAU, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$SAU[c(1,10,20,30,40,51,61)])
lines(pred.instsau50, lwd=2, col=2)

###we estimate the model with 25% of the data
bm_sau75<-BM(Production$SAU[1:15],display = T)
summary(bm_sau75)

pred_bmsau75<- predict(bm_sau75, newx=c(1:50))
pred.instsau75<- make.instantaneous(pred_bmsau75)

###Comparison between models (instantaneous)
###instantaneous
plot(Production$SAU, type= "b",xlab="Year", ylab="Annual Oil production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$SAU[c(1,10,20,30,40,51,61)])
lines(pred.instsau75, lwd=2, col=2)
lines(pred.instsau50, lwd=2, col=3)
lines(pred.instsau, lwd=2, col=4)

# GBM with 1 exponantial shock #
GBMe1_SAU<- GBM(Production$SAU,shock = "exp",nshock = 1,prelimestimates = c(BM(Production$SAU, display=FALSE)$Estimate[1,1],
                                                                            BM(Production$SAU, display=FALSE)$Estimate[2,1],
                                                                            BM(Production$SAU, display=FALSE)$Estimate[3,1],
                                                                            17,-0.1,0.1))
summary(GBMe1_SAU)
coef(GBMe1_SAU)
predicted_BMe1_SAU <- predict(GBMe1_SAU,newx=c(1:60))
plot.Dimora(GBMe1_SAU)
# GBM with mixed shocks shocks for SAUDI ARABIA #
GBM_mix_SAU<- GBM(Production$SAU, shock = "mixed", nshock = 2, 
                  prelimestimates = c(BM_SAU$Estimate[1,1],
                                      BM_SAU$Estimate[2,1],
                                      BM_SAU$Estimate[3,1],
                                      6,-0.1,0.1, 20,30,0.1))
summary(GBM_mix_SAU)
GBM_SAU_fitted<- fitted(GBM_mix_SAU)
coef(GBM_mix_SAU)

predictedGBM_mix_SAU<- predict(GBM_mix_SAU,newx = c(1:60))
plot(GBM_mix_SAU, type = "all")
pred.instGBM_SAU<- make.instantaneous(predictedGBM_mix_SAU)

####residual analysis of GBM for SAUDI ARABIA #
resGBM_SAU<- residuals(GBM_mix_SAU)
plot(resGBM_SAU, type="b")
Acf(resGBM_SAU)
Pacf(resGBM_SAU)

### we fit a GGM (better model...but we need to interpret well the parameters)
coef(BM_SAU)
GGM_SAU<- GGM(Production$SAU, prelimestimates=c(5.221162e+05, 0.001, 0.01, 3.507965e-03, 3.875671e-02))
summary(GGM_SAU)
sum(Production$SAU)
pred_GGMSAU<- predict(GGM_SAU, newx=c(1:60))
pred.instGGMSAU<- make.instantaneous(pred_GGMSAU)

plot(Production$SAU, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$SAU[c(1,10,20,30,40,51,61)])
lines(pred.instGGMSAU, lwd=2, col=2)

#---------------------Bass standard model on IRAN to get the coefficients-------------------#

# LIKE THE LAB SESSIONS#
# Estimating a simple bass model #
production_irn
BM_IRN <- BM(Production$IRN, display=T)
summary(BM_IRN)
coef(BM_IRN)

###prediction (out-of-sample)
pred_bmirn<- predict(BM_IRN, newx=c(1:60))
pred.instirn<- make.instantaneous(pred_bmirn)

###plot of fitted model 
plot(Production$IRN, type= "b",xlab="Year", ylab="Annual Oil production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$IRN[c(1,10,20,30,40,51,61)])
lines(pred.instirn, lwd=2, col=2)

predicted_BM_IRN <- predict(BM_IRN,newx=c(1:60))
plot(BM_IRN)
pred.instBM_IRN<- make.instantaneous(predicted_BM_IRN)
plot.Dimora(BM_IRN)
BM_IRN_fitted<- fitted(BM_IRN)

###we estimate the model with 50% of the data
bm_irn50<-BM(Production$IRN[1:31],display = T)
summary(bm_irn50)

pred_bmirn50<- predict(bm_irn50, newx=c(1:50))
pred.instirn50<- make.instantaneous(pred_bmirn50)

plot(Production$IRN, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$IRN[c(1,10,20,30,40,51,61)])
lines(pred.instirn50, lwd=2, col=2)

###we estimate the model with 25% of the data
bm_irn75<-BM(Production$IRN[1:15],display = T)
summary(bm_irn75)

pred_bmirn75<- predict(bm_irn75, newx=c(1:50))
pred.instirn75<- make.instantaneous(pred_bmirn75)

###Comparison between models (instantaneous)
###instantaneous
plot(Production$IRN, type= "b",xlab="Year", ylab="Annual Oil production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$IRN[c(1,10,20,30,40,51,61)])
lines(pred.instirn75, lwd=2, col=2)
lines(pred.instirn50, lwd=2, col=3)
lines(pred.instirn, lwd=2, col=4)

# GBM with 1 exponantial shock #
GBMe1_IRN<- GBM(Production$IRN,shock = "exp",nshock = 1,prelimestimates = c(BM(Production$IRN, display=FALSE)$Estimate[1,1],
                                                                            BM(Production$IRN, display=FALSE)$Estimate[2,1],
                                                                            BM(Production$IRN, display=FALSE)$Estimate[3,1],
                                                                            17,-0.1,0.1))
summary(GBMe1_IRN)
coef(GBMe1_IRN)
predicted_BMe1_IRN <- predict(GBMe1_IRN,newx=c(1:60))
plot.Dimora(GBMe1_IRN)
# GBM with mixed shocks shocks for IRAN #
GBM_mix_IRN<- GBM(Production$IRN, shock = "mixed", nshock = 2, 
                  prelimestimates = c(BM_IRN$Estimate[1,1],
                                      BM_IRN$Estimate[2,1],
                                      BM_IRN$Estimate[3,1],
                                      6,-0.1,0.1, 20,30,0.1))
summary(GBM_mix_IRN)
GBM_IRN_fitted<- fitted(GBM_mix_IRN)
coef(GBM_mix_IRN)

predictedGBM_mix_IRN<- predict(GBM_mix_IRN,newx = c(1:60))
plot(GBM_mix_IRN, type = "all")
pred.instGBM_IRN<- make.instantaneous(predictedGBM_mix_IRN)

####residual analysis of GBM for IRAN #
resGBM_IRN<- residuals(GBM_mix_IRN)
plot(resGBM_IRN, type="b")
Acf(resGBM_IRN)
Pacf(resGBM_IRN)

### we fit a GGM (better model...but we need to interpret well the parameters)

GGM_IRN<- GGM(Production$IRN, prelimestimates=c(3.835947e+05, 0.001, 0.01, 4.126620e-03, 1.465717e-02))
summary(GGM_IRN)
sum(Production$IRN)
pred_GGMIRN<- predict(GGM_IRN, newx=c(1:60))
pred.instGGMIRN<- make.instantaneous(pred_GGMIRN)

plot(Production$IRN, type= "b",xlab="Year", ylab="Annual oil Production",  pch=16, lty=3, xaxt="n", cex=0.6)
axis(1, at=c(1,10,20,30,40,51,61), labels=Years$IRN[c(1,10,20,30,40,51,61)])
lines(pred.instGGMIRN, lwd=2, col=2)

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
fitted(USA_arima010)
plot(USA_ts[,1])
lines(fitted(USA_arima010), col=2)
# adding 1  AR #
USA_arima110 <- Arima(USA_ts[,1], order = c(1,1,0))
USA_resid110 <- residuals(USA_arima110)
tsdisplay(USA_resid110)
summary(USA_arima110)
fitted(USA_arima110)
plot(USA_ts[,1])
lines(fitted(USA_arima110), col=2)
# adding 1 MA #
USA_arima111 <- Arima(USA_ts[,1], order = c(1,1,1))
USA_resid111 <- residuals(USA_arima111)
tsdisplay(USA_resid111)
summary(USA_arima111)
fitted(USA_arima111)
plot(USA_ts[,1])
lines(fitted(USA_arima111), col=2)
# adding 2 AR #
USA_arima211 <- Arima(USA_ts[,1], order = c(2,1,0))
USA_resid211 <- residuals(USA_arima211)
tsdisplay(USA_resid211)
summary(USA_arima211)
fitted(USA_arima211)
plot(USA_ts[,1])
lines(fitted(USA_arima211), col=2)
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
simple_usa_gam <- gam(production~gdp + consumption + reserves, data=train_usa)
summary(simple_usa_gam)
par(mfrow=c(2,2))
plot(simple_usa_gam, se=T)
AIC(simple_usa_gam)

# 2nd GAM with smoothing splines applied to all the variables #
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

# 3rd GAM with splines applied only to reserves variable #
s_usa_gam <- gam(production~ gdp + consumption + s(reserves), data=train_usa) #BEST
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


