setwd("~/EZW")
library(randomForest)
library(xtable)
library(plot3D)
library(rgl)
library(klaR)
library(caret)
library(plyr)
library(MASS)

# READ ORIGINAL DATA
train <- read.csv2("training_data.csv", sep = ",", header = T)

N <- dim(train)[1]
p <- dim(train)[2]

day_vec <- rep(0,N)
month_vec<- day_vec
year_vec <- day_vec

for (i in 1:N){
  datetxt <- as.character(train$DATE[i])
  datetxt <- (strsplit(datetxt, split = "/"))
  month_vec[i] <- as.numeric(datetxt[[1]][[1]])
  day_vec[i] <- as.numeric(datetxt[[1]][[2]])
  year_vec[i] <- as.numeric(datetxt[[1]][[3]])
}
train$DAY <- day_vec
train$MONTH <- month_vec
train$YEAR <- year_vec

train <- train[,-c(2, 5, 9)]
train <- train[,c(1:6,8,9,10,7)]

write.table(train, file = "train_date.csv", sep=" ")

counts <- count(train$KEYWORD)
rel_counts <-counts$freq / N
par(las=2)
plot(counts$x, rel_counts)

na_train <- train[!is.na(train$BIRTH_YEAR),]
na_counts <- count(na_train$KEYWORD)
na_rel_counts <-na_counts$freq / N
par(las=2)
plot(na_counts$x, na_rel_counts)


# RELATIVE COUNTS
counts$freq[counts$x == "grocery_store"] / sum(counts$freq)

names(train)

library(corrplot)
cor(t(as.factor(train)))
train_f <- as.factor(train)

n <- 2
samp_ind <- sample(x = seq(1 : N) , size = n, replace = F)
sub_train <- as.numeric(na_train[samp_ind,])

lda( sub_train[,10] ~ ., data = sub_train)


  fit <- train(KEYWORD ~ ., data=trainSet[trainInd, ], method="knn",tuneLength=15,trControl=ctrl)
  pp <- predict(fit, newdata=gener[-trainInd, -4], type="raw")