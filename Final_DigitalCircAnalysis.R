# OverDrive Circulation Analysis and Prediction 


#libraries
library(tidyverse)
library(magrittr)
library(stats)
library(forecast)
library(tseries)
library(corrplot)
library("tfse")



#Read in data
folder <- "C:/Users/Elissa/Documents/DSMS/Capstone_SPL/Data/"
dataMonth <- read.csv(file = paste0(folder, "SPL_Full_Data_Monthly_Final.csv"), sep = ",")

#Exploration----------------------------------------------------------------------

odts <- ts(data = dataMonth$circ_overdrive_cko , start= c(2018, 1), frequency = 12)
plot(odts, ylab = "Circulation")


# Moving averages
outpar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(odts), max(odts))
plot(odts, main="Original time series", ylab = "Circulation")
plot(ma(odts, 3), main="Centered Moving Averages (k=2)", ylim=ylim, ylab = "Circulation")
plot(ma(odts, 6), main="Centered Moving Averages (k=6)", ylim=ylim, ylab = "Circulation")
plot(ma(odts, 12), main="Centered Moving Averages (k=12)", ylim=ylim, ylab = "Circulation")
par(outpar)


# Seasonal Decomposition
mstlodts <- mstl(odts)
autoplot(mstlodts)


# ADF test
adf.test(odts)


#Exponential Smoothing n----------------------------------------------------------------------

ets(odts, model = "AAN")

ets(odts, model = "AAA")

ets(odts, model = "ZZZ")

fites <- ets(odts, model = "ZZZ")
accuracy(fit)

# Predict 6 months
pred <- forecast(fites, 6)

pred
plot(pred, main="OverDrive Circulation", ylab="Circulation", xlab="Time")

#ARIMA----------------------------------------------------------------------

fitaa <- auto.arima(odts)

accuracy(fitaa)

# Predict 6 months
pred <- forecast(fitaa, 6)

pred
plot(pred, main="OverDrive Circulation", ylab="Circulation", xlab="Time")


# correlation matrix heat map

dataMonthNum <- dataMonth %>%
  select(c(circ_total_physical, circ_overdrive_cko, wifi_tot_connections, traffic_avg, pc_avg_sessions, 
           open_total_hours, 
           curbside_pickup, covid_test_positive_stl, covid_test_hospital_stl, covid_test_deaths_stl, 
           covid_vax_booster_cum, covid_vax_mRNA_1_cum, covid_vax_mRNA_2_cum, covid_vax_JJdose_cum, pandemic_yes))

dataMonthNum$covid_vax_full_cum <- dataMonthNum$covid_vax_mRNA_2_cum + dataMonthNum$covid_vax_JJdose_cum

dataMonthNum <- dataMonthNum[dataMonthNum$pandemic_yes == "Y",]

dataMonthNum %<>% select(-c(covid_vax_mRNA_1_cum,  covid_vax_mRNA_2_cum, covid_vax_JJdose_cum, pandemic_yes))

col<- colorRampPalette(c("blue", "white", "Dark Orange"))(10)
cormat<-rquery.cormat(dataMonthNum, type = "full", col=col)
