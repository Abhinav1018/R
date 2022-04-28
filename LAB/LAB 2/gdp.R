
# For GDP

setwd("C:/Users/Abhinav Vijayakumar/Desktop/VIT Academics/Sem 6/Essentials of Data Analytics/LAB/LAB 2")

gdp <- read.csv("gdp.csv")
library(forecast)
library(tseries)
View(gdp)
gdpts<-ts(gdp$GDP_gr, start = min(gdp$Year), end = max(gdp$Year), frequency = 1)
class(gdpts)
plot(gdpts)
acf(gdpts)
pacf(gdpts)
adf.test(gdpts) # stationary only if p value <0.05

# To make it stationary, differentiate
gdpmodel=auto.arima(gdpts, ic='aic', trace = TRUE)
gdpf=forecast(gdpmodel, level=c(95), h=24)
gdpf
plot(gdpf)
accuracy(gdpmodel)