setwd('~/dev/evispot/Evispot-AI-Challenge')
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
train$WKND <- (train$WDAY == 1) | (train$WDAY == 7)

# FOREIGN / ABROAD
train$FOREIGN <- train$MRCH_CTRY != "SE"

# OREN
dec_vec = as.factor(as.numeric(train$TRANS_AMO%%1==0)+1)
train$DEC = dec_vec

# END_9
end_vec <- train$TRANS_AMO %% 10 == 9
train$END9 <- end_vec


#REMOVE SHITTY DATE; COUNTRY (~98% SE) AND ONE TOT CORR FACTOR
train <- train[,-c(2, 5, 9)]

# PERMUTE TO MOVE CLASS LAST 
train <- train[,c(1:6,8:15,7)]
train = droplevels(train)

#REMOVE NA
train <- train[!is.na(train$BIRTH_YEAR),]

#############################################################
# TEST DATA:
#############################################################
test <- read.csv2("test_data_lab.csv", sep = ",", header = T) # With labels.

N <- dim(test)[1]
p <- dim(test)[2]

# TURN SHITTY DATE FACTOR INTO YEAR; MONTH; DAY; WEEKDAY
day_vec <- rep(0,N)
month_vec<- rep(0,N)
year_vec <- rep(0,N)
wday_vec <- rep(0,N)

for (i in 1:N){
  datetxt <- as.character(test$DATE[i])
  datetxt <- (strsplit(datetxt, split = "/"))
  month_vec[i] <- as.numeric(datetxt[[1]][[1]])
  day_vec[i] <- as.numeric(datetxt[[1]][[2]])
  year_vec[i] <- as.numeric(datetxt[[1]][[3]])
  wday_vec[i] <- wday(sprintf("%d-%d-%d", year_vec[i],month_vec[i],day_vec[i]))
  
}
test$DAY <- day_vec
test$MONTH <- month_vec
test$YEAR <- year_vec
test$WDAY <- wday_vec
test$WKND <- (test$WDAY == 1) | (test$WDAY == 7)

# FOREIGN / ABROAD
test$FOREIGN <- test$MRCH_CTRY != "SE"

# OREN
dec_vec = as.factor(as.numeric(test$TRANS_AMO%%1==0)+1)
test$DEC = dec_vec

# END_9
end_vec <- test$TRANS_AMO %% 10 == 9
test$END9 <- end_vec


#REMOVE SHITTY DATE; COUNTRY (~98% SE) AND ONE TOT CORR FACTOR
test <- test[,-c(2, 5, 9)]

# PERMUTE TO MOVE CLASS LAST 
test <- test[,c(1:6,8:16,7)]
test = droplevels(test)

#REMOVE NA
test <- test[!is.na(test$BIRTH_YEAR),]







