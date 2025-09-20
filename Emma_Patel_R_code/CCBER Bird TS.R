library(dplyr)
library(plyr)
library(ggplot2)
library(stats)
library(forecast)
setwd("/Users/.../Desktop") #set location of csv
bird <- read.csv("bird_data.csv", header=TRUE,)
north <- bird[,7]
lagoon <- bird[,12]
west_lagoon <- bird[,18]
south_beach <- bird[,23]
lagoon_island <- bird[,28]
campus_point <- bird[,33]
east_margin <- bird[,38]
totals <- bird[,41]
date <- bird$Date

#North Margin Zone
northzone <- aggregate(north, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- northzone$Group.1
total <- northzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
northdiff <- diff(total,differences=1)
plot.ts(northdiff)
var(northdiff)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#Lagoon Zone
lagoonzone <- aggregate(lagoon, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- lagoonzone$Group.1
total <- lagoonzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
lagoondiff <- diff(total, differences=1)
var(lagoondiff)
plot.ts(lagoondiff)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#West Lagoon Park
westlagoonzone <- aggregate(west_lagoon, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- westlagoonzone$Group.1
total <- westlagoonzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#South Beach Zone
southbeachzone <- aggregate(south_beach, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- southbeachzone$Group.1
total <- southbeachzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
southdiff <- diff(total,lag=3)
var(southdiff)
plot.ts(southdiff)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#Lagoon Island Zone
lagoonislandzone <- aggregate(lagoon_island, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- lagoonislandzone$Group.1
total <- lagoonislandzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#Campus Point Zone
campuspointzone <- aggregate(campus_point, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- campuspointzone$Group.1
total <- campuspointzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#East Margin Zone
eastmarginzone <- aggregate(east_margin, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- eastmarginzone$Group.1
total <- eastmarginzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9),ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

#Totals
totalzone <- aggregate(totals, FUN="sum",by=list(as.Date(date, "%d-%b-%y")))
day <- totalzone$Group.1
total <- totalzone$x
mean(total)
var(total)
plot(day,total)
plot.ts(total, frequency = 12, start=c(1996,9), ylab="Number of Birds")
abline(h=mean(total))
acf <- acf(total, lag=20, type="correlation",plot=T)
pacf <- pacf(total,lag=20, plot=T)
hist(total)
fit <- auto.arima(total)
fc <- forecast(fit, h=12)
plot(fc)

