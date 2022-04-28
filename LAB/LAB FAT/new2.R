##Dataset_B4.csv
library(tseries)
library(forecast)
library(tidyverse)
rm(list=ls())
setwd("C:/Users/risha/OneDrive/Desktop/Dataset")
nuke <- read.csv("Dataset_B4.csv")
library(forecast)
library(tseries)
#nuke %>% distinct(nuclear.capacity..Megawatts.,.keep_all= TRUE)
head(nuke)
nukets<-ts(nuke$nuclear.capacity..Megawatts., start = as.Date("2019-01-01"), end = as.Date("2021-03-31"), frequency = 365.25)
class(nukets)
plot(nukets)
acf(nukets)
pacf(nukets)
adf.test(nukets) # stationary only if p value <0.05
# To make it stationary, differentiate
nukemodel=auto.arima(nukets, ic='aic', trace = TRUE)
nukef=forecast(nukemodel, level=c(95), h=24)
nukef
plot(nukef)
accuracy(nukemodel)










##Dataset_C1.csv
library(dplyr)
setwd("C:/Users/risha/OneDrive/Desktop/Dataset")
data <- read.csv("Dataset_C1.csv")
str(data)
data$weightLoss<-data$pre.weight-data$post.weight
library(dplyr) # To group the data
group_by(data,Diet) %>% summarise(count = n(),mean = mean(weightLoss, na.rm =                                                          TRUE))
# ANOVA
ANOVA <- aov(weightLoss~as.factor(Diet), data = data)
summary(ANOVA)
TukeyHSD(ANOVA)