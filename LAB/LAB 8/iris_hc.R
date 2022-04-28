rm(list=ls())
setwd("C:/Users/Abhinav Vijayakumar/Desktop/VIT Academics/Sem 6/EDA/LAB/LAB 8")

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
