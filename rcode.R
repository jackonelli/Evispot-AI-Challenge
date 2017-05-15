library(MASS)
library(caret)
library(ranger)
library(glmnet)
setwd('~/dev/evispot/Evispot-AI-Challenge')
#train <-read.table("training_data.csv",sep=",",header=TRUE)
#nearZeroVar(train[,-11])
#x11()
#par(mfrow=c(2,5))
#for(i in 1:10){
#  hist(as.numeric(train[ ,i]))
#}

#nSample = dim(train)[1]
nSample = 50000
sTrain <-sample(seq(1,dim(train)[1]),nSample)
subTrain = train[sTrain,]
subTrainKey = subTrain[,2] 
subTrain = subTrain[c(-2,-3,-7,-8,-9)]
#subTrain[,2] <- as.factor(subTrain[,2])
#subTrain <- subTrain[,c(1,5,7,8,9,10)]
#subTrain <- subTrain[,c(-2,-6)]
#subTrain <- train[sTrain,c(-1,-2,-3,-4,-6)]
#subTrain <- train[sTrain,-6]
#subTrain <- train[sTrain,c(2,5,11)]
#subTrain <- train[sTrain,]

trainQuota <- 2/3
trainSample <- sample(nSample,trainQuota*nSample)

subTrain.train = subTrain[trainSample,]
subTrain.test = subTrain[-trainSample,]
subTrain.testkey = subTrainKey[-trainSample]

magic_label <- dim(subTrain)[2]
x.train = subTrain.train[,-magic_label]
y.train = subTrain.train[,magic_label]
x.test = subTrain.test[,-magic_label]
y.test = subTrain.test[,magic_label]

# Just set deciding on decimal value or not
#shitface = y.test
#shitfaceIND = x.test$DEC=="1"
#shitface[shitfaceIND] = "grocery_store"
#shitfaceIND = x.test$DEC=="2"
#shitface[shitfaceIND] = "grocery_store"
#shitface[shitfaceIND] = "Restaurants_bars_cafes"
#sum(y.test == shitface) / (nSample-trainQuota*nSample)

#cool.rf <- ranger(KEYWORD ~., data=subTrain.train,write.forest = TRUE ,num.trees = 1000,importance="impurity")
#cool.rf.pred <- predict(cool.rf, dat = subTrain.test)
#table(subTrain.test[,-1]$KEYWORD,cool.rf.pred$predictions)
#sum(cool.rf.pred$predictions==subTrain.test$KEYWORD) / dim(subTrain.test)[1]


#cool.nb<-train(x=subTrain.train[,-9],y=subTrain.train[,9],method="nb")
#cool.nb.pred <-predict(cool.nb,dat = subTrain.test[,-1])
#length(cool.nb.pred)
#length(subTrain.test[,-1]$KEYWORD)
#sum(cool.nb.pred==subTrain.test[,-1]$KEYWORD) / dim(subTrain.test)[1]

#

fitControl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE)
#
# ccnb<-train(x=x.train,y=y.train,method="nb",trControl = fitControl)
# pp<-predict(ccnb,x.test,type="prob")
# pp$obs<-y.test
# pp$pred<-predict(ccnb,x.test)
# AccN<-multiClassSummary(pp,lev=levels(pp$obs))
# print(AccN)
 
ccrf<-train(x=x.train,y=y.train,method="ranger",trControl = fitControl,num.trees=100)
pp<-predict(ccrf,x.test,type="prob")
ppYOLO<-predict(ccrf,x.test,type="prob")
pp$obs<-y.test
pp$pred<-predict(ccrf,x.test)
AccR<-multiClassSummary(pp,lev=levels(pp$obs))
print(AccR)

#ccknn<-train(x=x.train,y=y.train,method="knn",trControl = fitControl)
#pp<-predict(ccknn,x.test,type="prob")
#pp$obs<-y.test
#pp$pred<-predict(ccknn,x.test)
#AccK<-multiClassSummary(pp,lev=levels(pp$obs))
#print(AccK)

#ccknn<-train(x=x.train,y=y.train,method="nnet",trControl = fitControl)
#pp<-predict(ccknn,x.test,type="prob")
#pp$obs<-y.test
#pp$pred<-predict(ccknn,x.test)
#AccGLM<-multiClassSummary(pp,lev=levels(pp$obs))
#print(AccGLM)

#magic_lasso <- cv.glmnet(as.matrix(x.train),as.numeric(y.train))
#magic_lasso <- cv.glmnet(as.matrix(x.train),as.numeric(y.train))
#plot(magic_lasso)

#true_dec = as.numeric(train$TRANS_AMO%%1==0)
#cool_matrix = matrix(nrow = dim(train)[1],ncol=14)
#for(i in 1:14){
#  cool_matrix[,i]= as.numeric(train$KEYWORD==levels(train$KEYWORD)[i])
#  print(sum(cool_matrix[,i]==true_dec) / dim(train)[1])
#}

#true_dec = as.numeric(train$BIRTH_YEAR<=1930)
#cool_matrix = matrix(nrow = dim(train)[1],ncol=14)
#for(i in 1:14){
# cool_matrix[,i]= as.numeric(train$KEYWORD==levels(train$KEYWORD)[i])
# print(sum(cool_matrix[,i]==true_dec) / dim(train)[1])
#}


# USR INDEX
N <- dim(train)[1] 
n_use <- length(unique(train$Key_ENGNO))
K <- length(levels(train$KEYWORD))
key_dist <- matrix( rep(0,n_use*K), nrow = n_use, ncol = K)
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


# KEYWORD DIST PER USER

for (i in 1:n_use){
  aba <- table(train$KEYWORD[usr_idx[i,1]:usr_idx[i,2]])
  key_dist[i,] <- aba / (usr_idx[i,2]-usr_idx[i,1] + 1 )
}
obs_user <- rep(0,ceiling((1-trainQuota)*nSample))
for (i in 1:ceiling((1-trainQuota)*nSample)){
  if(i!=1835)
obs_user[i] <-which(usr_idx[,3] == subTrain.testkey[i])
}

bestRes = 0
tmp = 0
ind = 0
for(i in seq(0.01,1,0.01)){
result <- (i*key_dist[obs_user,]+(1-i)*ppYOLO)
maxRes <- apply(result,1,which.max)
tmp = sum(maxRes==as.numeric(y.test)) / length(y.test)
if(tmp > bestRes){
  bestRes = tmp
  ind = i
  print(bestRes)
}
}

#sheissVec = cbind(key_dist[obs_user,],as.matrix(ppYOLO))
#ccnbASDASD<-train(x=sheissVec,y=y.test,method="ranger",trControl = fitControl)
