
setwd("C:/Users/Abhinav Vijayakumar/Desktop/VIT Academics/Sem 6/Essentials of Data Analytics/LAB/LAB 3")
dff=read.csv("weatherHistory2016.csv")

library(forecast)
library(tseries)

data<-ts(dff$Temperature..C.,start = as.Date("2016-10-01"),end = 
           as.Date("2016-12-31"),frequency = 24)
plot(data)
acf(data)
pacf(data)
adf.test(data)
model=auto.arima(data,ic="aic",trace=TRUE)
f=forecast(model,level=c(95),h=720)
f
plot(f)
accuracy(model