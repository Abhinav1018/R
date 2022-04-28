#lab 1
rm(list=ls())
library(dplyr)
setwd("D:/6th_Semester/Data_Analytics_Lab/Lab 1")
data<-read.csv('data.csv')
## 75% of the sample size
smp_size <- floor(0.75 * nrow(data))
#setting  the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
correlation<-cor.test(train$Height,train$Weight)
print(correlation)
plot(train$Weight,train$Height,xlab = "Weight",ylab = "Height",main="Weight vs Height")
##Linear model
lmodel<-lm(Height~Weight,data=train)
abline(lmodel,col="red")
summary(lmodel)
predicted<-predict(lmodel,data=test)
mae(test$Height,predicted)











#lab 2
rm(list=ls())
setwd("D:/6th_Semester/Data_Analytics_Lab/Lab 2")
gold <- read.csv("gold.csv")
library(forecast)
library(tseries)
view(gold)
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










#lab 3
#1.Mulitple Linear Regression Model:
rm(list=ls())
setwd("D:/6th_Semester/Data_Analytics_Lab/Lab 3")
df=read.csv("weatherHistory2016.csv")
head(df)
library(dplyr)
library(tidyr)
library(GGally)
a=sample_n(df,200)
a %>% drop_na()
a <- a[,c(4:9)]
head(a)
cor.test(a$Temperature..C.,a$Apparent.Temperature..C.)
cor.test(a$Temperature..C.,a$Humidity)
cor.test(a$Temperature..C.,a$Wind.Speed..km.h.)
cor.test(a$Temperature..C.,a$Wind.Bearing..degrees.)
cor.test(a$Temperature..C.,a$Pressure..millibars.)
cor.test(a$Temperature..C.,a$Visibility..km.)
ggcorr(a, label = TRUE)
lmodel=lm(a$Temperature..C.~a$Apparent.Temperature..C.+a$Humidity+a$Visibilit
          y..km.)
summary(lmodel)
#2.Time Series Forecasting:
rm(list=ls())
setwd("D:/6th_Semester/Data_Analytics_Lab/Lab 3")
df=read.csv("weatherHistory2016.csv")
library(forecast)
library(tseries)
data<-ts(df$Temperature..C.,start = as.Date("2016-10-01"),end =
           as.Date("2016-12-31"),frequency = 24)
plot(data)
acf(data)
pacf(data)
adf.test(data)
model=auto.arima(data,ic="aic",trace=TRUE)
forecastedVal=forecast(model,level=c(95),h=24)
print(forecastedVal)
plot(forecastedVal)
accuracy(model)






















#lab 4
rm(list=ls())
data <- read.csv("color-anova-example.csv")
library(dplyr) # To group the data
group_by(data,color) %>% summarise(count = n(),mean = mean(response, na.rm =TRUE))
# ANOVA
ANOVA <- aov(response~color, data = data)
summary(ANOVA)
TukeyHSD(ANOVA)











#lab 5
rm(list=ls())
setwd("C:\\Users\\risha\\Desktop\\6thSemester\\EDA\\Lab\\05")
mydata<-read.csv("Social_Network_Ads.csv")
library(caTools)
splitd<-sample.split(mydata,SplitRatio = 0.8)
train=subset(mydata,splitd=="TRUE")
test=subset(mydata,splitd=="FALSE")
train
mydata$Gender<-as.factor(mydata$Gender)
mydata$Purchased<-as.factor(mydata$Purchased)
mymodel <- glm(Purchased ~ Age+Gender+EstimatedSalary, data=train,
               family='binomial')
summary(mymodel)
restrain<-predict(mymodel,train,type='response')
plot(restrain)
restest<-predict(mymodel,test,type='response')
plot(restest,col='red')
par(new=TRUE)
plot(test$Purchased)
cfmatrix<-table(Act=test$Purchased, pred=restest>0.5)
cfmatrix
Acc=(cfmatrix[[1,1]]+cfmatrix[[2,2]])/sum(cfmatrix)
Acc
plot(restest)










#lab 6
rm(list=ls())
library(caTools)
library(class)
data <- read.table(file.choose(), sep = ',')
View(data)
split <- sample.split(data, SplitRatio = 0.8)
train <- subset(data, split=="TRUE")
test <- subset(data, split=="FALSE")
View(train)
norm <- function(x) {((x-min(x))/(max(x)-min(x)))}
normtrain <- as.data.frame(lapply(train[, -5], norm))
normtest <- as.data.frame(lapply(test[, -5], norm))
summary(train[, 1:4])
summary(normtrain[, 1:4])
predict <- knn(normtrain, normtest, train[,5], k = 12)
cf <- table(predict, test[,5])
cf
Acc <- (cf[[1, 1]]+cf[[2, 2]]+cf[[3, 3]])/sum(cf)
Acc
rm(list=ls())
setwd("D:\6th_Semester\Data_Analytics_Lab\New folder")
wdbc<-read.table(file.choose(),sep=',')
view(wdbc)
wdbc<-wdbc[,-1]
mynorm<-function(x){((x-min(x))/(max(x)-min(x)))}
mydata<-as.data.frame(lapply(wdbc[,-1], mynorm))
summary(wdbc[,2:5])
summary(mydata[,1:4])
train<-mydata[1:400,]
test<-mydata[401:569,]
library(class)
pred<-knn(train,test,wdbc[1:400,1],k=21)
cf<-table(pred,wdbc[401:569,1])
cf
acc=(cf[[1,1]]+cf[[2,2]])/sum(cf)
acc










#lab 7
rm(list=ls())
setwd("D:/6th_Semester/Data_Analytics_Lab/lab 7")
data2<-read.csv("USArrests.csv")
view(data2)
data2<-data2[,-1]
df1<-scale(data2)
fit1<-kmeans(df1,centers=2)
fit1$cluster
fit1$size
fit1$withinss
fit1$tot.withinss
Kmax1<-15
wcss1<-rep(NA,Kmax1)
nClust1<- list()
for(i in 1:Kmax1){
  fit1<-kmeans(df1,i)
  wcss1[i]<-fit1$tot.withinss
  nClust1[[i]]<-fit1$size
}
plot(1:Kmax1,wcss1,type="b",pch=19)
fit1<-kmeans(df1,centers=3)
fit1$cluster
fit1$size
fit1$center
library(factoextra)
fviz_nbclust(df1, kmeans, method = "wss")
fviz_cluster(fit1, data2)
library(cluster)
fitm1 <- pam(df1, 3, metric = "manhattan")
fitm1
fitm1$medoids
fviz_cluster(fitm1, data2)













#lab 8
rm(list=ls())
setwd("D:\\EDA\\Lab\\08")
data <- read.csv("iris.csv",row.names=1)
View(data)
df <- scale(data)
View(df)
ed <- dist(df, method = 'euclidean')
hierClust <- hclust(ed, method = 'complete')
plot(hierClust)
cluster <- cutree(hierClust, k = 4)
cluster
rect.hclust(hierClust, k = 4, border = 2:4)

























#lab 9
rm(list=ls())
gd <- function(x1,x2,y,m1,m2,c,alpha,conv_thr,iter)
{
  iterations=0
  Lf=0
  while(iterations<=iter)
  {
    y_pred=m1*x1+m2*x2+c
    Lf_new=0.5*(sum(y_pred-y)^2)
    m1=m1-alpha*sum((y_pred-y)*x1)
    m2=m2-alpha*sum((y_pred-y)*x2)
    c=c-alpha*sum(y_pred-y)
    if(abs(Lf-Lf_new)<conv_thr)
    { break;
    }
    Lf=Lf_new
    iterations=iterations+1
  }
  return(paste("Optimum Slope 1",m1,m2,"Optimum Intercept
",c,"Number of iterations ",iterations,"Loss Funtion ",Lf))
}
data <- mtcars
plot(data$mpg,data$wt,col="red",pch=20)
gd(data$wt,data$hp,data$mpg,-0.2,-
     0.2,32,0.000002,0.000001,1000000)
reg <- lm(data$mpg~data$wt+data$hp)
reg 























#lab 10
#Momentum based Gradient Descent
rm(list=ls())
Mgd<-function(x1,x2,y,m1,m2,c,alpha,gamma,iter){
  iterations=0
  Lf<-0
  u_m1<-0 
  u_m2<-0
  u_c<-0
  while(iterations<=iter){
    y_pred<-m1*x1+m2*x2+c
    Lf_new<-0.5*sum((y_pred-y)^2)
    nu_m1<-gamma*u_m1+alpha*sum((y_pred-y)*x1)
    nu_m2<-gamma*u_m2+alpha*sum((y_pred-y)*x2)
    nu_c<-gamma*u_c+alpha*sum(y_pred-y)
    m1<-m1-nu_m1
    m2<-m2-nu_m2
    c<-c-nu_c
    u_m1<-nu_m1
    u_m2<-nu_m2
    u_c<-nu_c
    Lf<-Lf_new
    iterations=iterations+1
  }
  return(paste("optimal intercept:",c,"optimatl slope:",m1,m2,"Loss
funciton:",Lf,"iterations:",iterations))
}
data1<-mtcars
plot(data1$mpg,data1$wt,col="red",pch=20)
Mgd(data1$wt,data1$hp,data1$mpg,-0.2,-0.2,32,0.000002,0,1000000)
lr<-lm(data1$mpg~data1$hp+data1$wt)
lr

