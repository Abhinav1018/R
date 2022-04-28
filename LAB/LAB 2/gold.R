
# For Gold price

setwd("C:/Users/Abhinav Vijayakumar/Desktop/VIT Academics/Sem 6/Essentials of Data Analytics/LAB/LAB 2")

gold <- read.csv("gold.csv")
library(forecast)
library(tseries)
View(gold)
goldts<-ts(gold$Price, start = min(gold$Month), end = max(gold$Month), frequency = 1)
class(goldts)
plot(goldts)
acf(goldts)
pacf(goldts)
adf.test(goldts) # stationary only if p value <0.05

# To make it stationary, differentiate
goldmodel=auto.arima(goldts, ic='aic', trace = TRUE)
goldf=forecast(goldmodel, level=c(95), h=24)
goldf
plot(goldf)
accuracy(goldmodel)