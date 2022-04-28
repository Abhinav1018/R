# To clear the environment
rm(list=ls())

setwd("C:/Users/Abhinav Vijayakumar/Desktop/VIT Academics/Sem 6/EDA/LAB/LAB 4")

data <- read.csv("color-anova-example.csv")

library(dplyr) # To group the data


group_by(data,color) %>% summarise(count = n(),mean = mean(response, na.rm = TRUE))

# ANOVA
ANOVA <- aov(response~color, data = data)
summary(ANOVA)
TukeyHSD(ANOVA)