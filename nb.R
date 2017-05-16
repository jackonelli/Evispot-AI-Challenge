n <- 30000
N <- dim(test)[1]
test_idx <- sample(N,n)
pred_keyword <- rep(0,n)
sub_test <- test[test_idx,-c(1,3,4,5,6,7,8,9,10,15)]
# nb_train <- train[-test_idx,]
err <- 0
for (i in 1:n){
  test_usr <- sub_test$Key_ENGNO[i] 
  usr_n <- which(usr_idx[,3] == test_usr)
  train_X <- train[usr_idx[usr_n,1]:usr_idx[usr_n,2], -c(1:10) ]
  
  #P_k <- (table(user_X$KEYWORD))/ (usr_idx[usr_n,2] - usr_idx[usr_n,1] +1)
  #X <- test[i, -c(1,2,3,4,6,7,8,9,10) ]
  test_X <- sub_test[i,-1]
  
  
  model <- naiveBayes(KEYWORD ~ ., data = train_X, na.action = na.pass)
  result = tryCatch({
    predict(model, test_X, type ="class")
  }, error = function(e) {
    predict(ccrf,test[i,c(1,4,5,6,10:14)])
    err <- err + 1
  })
  pred_keyword[i] <- levels(train$KEYWORD)[result]
  if (i%%500 == 0){
    print(i)
  }
}
print(i)
sum(pred_keyword[1:i-1] == (test$KEYWORD[test_idx])[1:i-1]) / i


model
