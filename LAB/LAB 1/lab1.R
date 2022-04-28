
# On mtcars
rm(list=ls())

library(dplyr)
library(Metrics)
data1 <- mtcars

## 75% of the sample size
smp_size <- floor(0.75 * nrow(mtcars))

#setting  the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(mtcars)), size = smp_size)
train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]


cr<-cor.test(train$wt,train$mpg)
print(cr)

plot(train$wt,train$mpg,xlab = "Wt",ylab = "mpg",main="mpg VS Wt")

## Linear model
lmodel<-lm(mpg~wt,data=train)
abline(lmodel,col="red")

summary(lmodel)

predicted<-predict(lmodel,data=test)
mae(test$mpg,predicted)


# On data.csv

rm(list=ls())
library(dplyr)
library(Metrics)

setwd("C:/Users/Abhinav Vijayakumar/Desktop/VIT Academics/Sem 6/Essentials of Data Analytics/LAB/LAB 1")

data<-read.csv('data.csv')

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))

#setting  the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
cr<-cor.test(train$Height,train$Weight)

print(cr)

plot(train$Weight,train$Height,xlab = "Weight",ylab = "Height",main="Height vs Weight")

##Linear model
lmodel<-lm(Height~Weight,data=train)
abline(lmodel,col="red")
summary(lmodel)


predicted<-predict(lmodel,data=test)
mae(test$Height,predicted)




