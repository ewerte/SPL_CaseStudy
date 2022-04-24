# Physical Circulation Analysis and Prediction 



#libraries
library(tidyverse)
library(magrittr)
library(stats)
library(forecast)
library(tseries)
library("tfse")
library(ggplot2)




#Read in data
folder <- "C:/Users/Elissa/Documents/DSMS/Capstone_SPL/Data/"
dataMonth <- read.csv(file = paste0(folder, "SPL_Full_Data_Monthly_Final.csv"), sep = ",")

#Exploration----------------------------------------------------------------------

pcts <- ts(data = dataMonth$circ_total_physical, start= c(2018, 1), frequency = 12)
plot(pcts, ylab = "Circulation")

#side by side box plots
options(scipen=100000)

ggplot(dataMonth, aes(x=pandemic_yes, y=circ_total_physical, fill=pandemic_yes)) +
  geom_boxplot() +
  geom_dotplot(binaxis = "y", stackdir = "center", dotsize = 0.5) +
  scale_fill_manual(values = c("#4472C4", "#ED7D31"))+
  labs(
    x = "Pandemic",
    y = "Circulation per Month"
  ) 

# Moving averages
outpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(pcts), max(pcts))
plot(pcts, main="Original time series", ylab = "Circulation")
plot(ma(pcts, 3), main="Centered Moving Averages (k=2)", ylim=ylim, ylab = "Circulation")
plot(ma(pcts, 6), main="Centered Moving Averages (k=6)", ylim=ylim, ylab = "Circulation")
plot(ma(pcts, 12), main="Centered Moving Averages (k=12)", ylim=ylim, ylab = "Circulation")
par(outpar)


# Seasonal Decomposition
mstlpcts <- mstl(pcts)
autoplot(mstlpcts)


# ADF test
adf.test(pcts)

# ACF/PACF
Acf(pcts)
Pacf(pcts)

ndiffs(pcts)

Acf(diff(pcts, 1))
Pacf(diff(pcts, 1))


#Exponential Smoothing n----------------------------------------------------------------------

ets(pcts, model = "ANA")

ets(pcts, model = "AAA")

ets(pcts, model = "ZZZ")

fites <- ets(pcts, model = "ZZZ")
accuracy(fit)

# Predict 6 months
pred <- forecast(fites, 6)

pred
plot(pred, main="Physical Circulation", ylab="Circulation", xlab="Time")

#ARIMA----------------------------------------------------------------------

fitaa <- auto.arima(pcts)

accuracy(fitaa)

# Predict 6 months
pred <- forecast(fitaa, 6)

pred
plot(pred, main="Physical Circulation", ylab="Circulation", xlab="Time")
