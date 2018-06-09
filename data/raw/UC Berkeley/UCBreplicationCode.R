########################################
#
#  9/9/17 analysis of RGR data
#  Replication at UC Berkeley
#
########################################

.libPaths("D:/Users/Don/R")  #for Don's home computer
library(readxl)
library(plyr)
library(data.table)
stderr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x))) #define standard error function

setwd("D:/Users/Don/Dropbox/RGR (ManyLabs)/Data") #for home computer
.libPaths("D:/Users/Don/R")

#Do analysis on my data
rgrFull <- read_excel("170914RGR.xlsx", sheet = 1)
attach(rgrFull)

rgr <- subset(rgrFull,DataSource == "One at a time in the lab") #retain only those run one at a time
rgr <- subset(rgr,Exclude == 0) #drop those in the high load condition who report 0 effort or who didn't actually count down
attach(rgr)

mean(Predicted.Likelihood, na.rm = TRUE) #first descriptives
sd(Predicted.Likelihood, na.rm = TRUE)

fit <- aov(Predicted.Likelihood~Cognitive.load*Had.read, data=rgr)
summary(fit)

aggregate(Predicted.Likelihood~Cognitive.load+Had.read,rgr,mean)

#Maya's prepped data
setwd("D:/Users/Don/Dropbox/RGR (ManyLabs)/Data/MayaData") #for home computer
d = read.csv("2017-07-26prepped_UCB.csv", header = TRUE)
attach(d)

mean(lkl, na.rm = TRUE) #first descriptives
sd(lkl, na.rm = TRUE)

fit <- aov(lkl~load*had.read, data=d)
summary(fit)

aggregate(lkl~load+had.read,d,mean)

#My expurgated data
dt = read_excel("170912RGR.expurgated.xlsx", sheet = 1)
attach(dt)

mean(lkl, na.rm = TRUE) #first descriptives
sd(lkl, na.rm = TRUE)

fit <- aov(lkl~load*had.read, data=d)
summary(fit)

aggregate(lkl~load+had.read,dt,mean)
