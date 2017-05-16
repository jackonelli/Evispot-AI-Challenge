library(MASS)
library(caret)
library(ranger)
library(glmnet)
setwd('~/dev/evispot/Evispot-AI-Challenge')

# USR INDEX
N <- dim(train)[1]
n_use <- length(unique(train$Key_ENGNO))
K <- length(levels(train$KEYWORD))
j <- 1
change_id <- train$Key_ENGNO[1]
usr_idx <- matrix( rep(1,n_use*3), nrow = n_use, ncol = 3)

for (i in 1:N){
  if(change_id != train$Key_ENGNO[i] ){
    usr_idx[j,2] <- i-1
    usr_idx[j+1,1] <- i
    usr_idx[j,3] <- train$Key_ENGNO[i-1]
    j <- j + 1
    change_id <- train$Key_ENGNO[i]
  }
}
usr_idx[j,2] <- N
usr_idx[j,3] <- train$Key_ENGNO[N]

# USR INDEX test
N <- dim(test)[1]
n_use <- length(unique(test$Key_ENGNO))
K <- length(levels(test$KEYWORD))
j <- 1
change_id <- test$Key_ENGNO[1]
usr_idx_test <- matrix( rep(1,n_use*3), nrow = n_use, ncol = 3)

for (i in 1:N){
  if(change_id != test$Key_ENGNO[i] ){
    usr_idx_test[j,2] <- i-1
    usr_idx_test[j+1,1] <- i
    usr_idx_test[j,3] <- test$Key_ENGNO[i-1]
    j <- j + 1
    change_id <- test$Key_ENGNO[i]
  }
}
usr_idx_test[j,2] <- N
usr_idx_test[j,3] <- test$Key_ENGNO[N]
usr_idx_test[4387,3]<-96307
aba <- rbind(usr_idx_test,c(140530,140546,95245))
usr_idx_test <- rbind(aba,c(140547,140602,4120))

subTrain = train
subTrain = subTrain[c(-2,-3,-7,-8,-9)]
subTest = test
subTest = subTest[c(-2,-3,-7,-8,-9,-10)]
lastVal = dim(subTrain)[2]
tryFor =dim(usr_idx_test)[1]
totPred <- rep("", dim(test)[1])

for( i in 1:tryFor){
  startTest = usr_idx_test[i,1]
  stopTest = usr_idx_test[i,2]
  idT = usr_idx_test[i,3]
  idI = which(usr_idx[,3]==idT)
  start = usr_idx[idI,1]
  stop = usr_idx[idI,2]

  forestVec = subTrain[start:stop,]
  forestVecTest = subTest[startTest:stopTest,]

  subTrain.train.x = forestVec[,-lastVal]
  subTrain.train.y = forestVec[,lastVal]
  subTrain.test.x = forestVecTest[,-lastVal]
  subTrain.test.y = forestVecTest[,lastVal]

  subTrain.train.y = droplevels(subTrain.train.y)
  subTrain.test.y = droplevels(subTrain.test.y)

  rf_model<-train(x=subTrain.train.x, y=subTrain.train.y, method="ranger",num.trees=100)
  prediction<-predict(rf_model,subTrain.test.x)
  totPred[startTest:stopTest] <- as.character(prediction)
}
