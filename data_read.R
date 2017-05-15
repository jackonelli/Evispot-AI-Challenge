setwd("~/EZW")
library(randomForest)
library(xtable)
library(plot3D)
library(rgl)
library(klaR)
library(caret)
library(plyr)
library(MASS)
#WEEKDAYS
library(lubridate)

# READ ORIGINAL DATA
train <- read.csv2("training_data.csv", sep = ",", header = T)
train <- train[!is.na(train$BIRTH_YEAR),]



N <- dim(train)[1]
p <- dim(train)[2]

# TURN SHITTY DATE FACTOR INTO YEAR; MONTH; DAY; WEEKDAY
day_vec <- rep(0,N)
month_vec<- rep(0,N)
year_vec <- rep(0,N)
wday_vec <- rep(0,N)

for (i in 1:N){
  datetxt <- as.character(train$DATE[i])
  datetxt <- (strsplit(datetxt, split = "/"))
  month_vec[i] <- as.numeric(datetxt[[1]][[1]])
  day_vec[i] <- as.numeric(datetxt[[1]][[2]])
  year_vec[i] <- as.numeric(datetxt[[1]][[3]])
  wday_vec[i] <- wday(sprintf("%d-%d-%d", year_vec[i],month_vec[i],day_vec[i]))
  
}
train$DAY <- day_vec
train$MONTH <- month_vec
train$YEAR <- year_vec
train$WDAY <- wday_vec
train$WKND <- 1 - (train$WDAY > 1) * (train$WDAY < 7)

train$FOREIGN <- train$MRCH_CTRY != "SE"

#REMOVE SHITTY DATE; COUNTRY (~98% SE) AND ONE TOT CORR FACTOR
train <- train[,-c(2, 5, 9)]
# PERMUTE TO MOVE CLASS LAST 
train <- train[,c(1:6,8:13,7)]

