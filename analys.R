# GET TOP TRANS FOR EACH USER

top_trans_vec <- rep(0,N)
top_transratio_vec <- rep(0,N)
users <- split(train, train$Key_ENGNO)
n_use <- length(users)
n_transac <- cbind(rep(0,n_use),rep(0,n_use))

for (i in 1:n_use){
  usr_idx <- train$Key_ENGNO == names(users)[i]
  aba <- count(users[[i]][[11]])
  # POSSIBLY USE # OF TRANSACTIONS BUT NOT NOW
  n_transac[i,] <- c( (dim(users[[i]])[1]), max(aba$freq) )
  top_trans_vec[usr_idx] <- aba$x[which.max(aba$freq)]
  top_transratio_vec[usr_idx] <-  n_transac[i,2] / n_transac[i,1]  
}

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

# TEST USR IDX
N <- dim(test)[1] 
n_use <- length(unique(test$Key_ENGNO)) +2
K <- length(levels(test$KEYWORD))

j <- 1
change_id <- test$Key_ENGNO[1]
usr_idx_test <- matrix( rep(1,((n_use)*3)), nrow = n_use, ncol = 3)

for (i in 1:N){
  if(change_id != test$Key_ENGNO[i] ){
    usr_idx_test[j,2] <- i-1
    usr_idx_test[j+1,1] <- i
    usr_idx_test[j,3] <- test$Key_ENGNO[i-1]
    j <- j + 1
    change_id <- test$Key_ENGNO[i]
  }
}


for (i in 1:dim(aba)[1]){
  a <- aba[i,1]
  b <- aba[i,2]
  if(test$Key_ENGNO[a] != test$Key_ENGNO[b])
    print(a)
}

#usr_idx_test[j,2] <- N
#usr_idx_test[j,3] <- test$Key_ENGNO[N]


# KEYWORD DIST PER USER
key_dist <- matrix( rep(0,n_use*K), nrow = n_use, ncol = K)
for (i in 1:n_use){
  aba <- table(train$KEYWORD[usr_idx[i,1]:usr_idx[i,2]])
  key_dist[i,] <- aba / (usr_idx[i,2]-usr_idx[i,1] + 1 )
}


hist(train$TRANS_AMO[-c(which.min(train$TRANS_AMO),which.max(train$TRANS_AMO))], 1000)



min(train$TRANS_AMO)
max(train$TRANS_AMO)



counts <- count(train$KEYWORD)
rel_counts <- counts$freq / sum(counts$freq)
rel_counts[6] + rel_counts[12]

sum( train$MRCH_CITY == " ")
users[2][1]

counts <- count(train$Key_ENGNO)
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

N <- dim(train)[1] 
n <- 10000
samp_ind <- sample(x = seq(1 : N) , size = n, replace = F)
# Adding shitmix electronics fucker
samp_ind(1) <- which(top_trans_vec == 5)[1]
sub_train <-  (train$KEYWORD[samp_ind])
pred1 <- sub_train
pred1[1:2] <- "home" 
pred <- as.factor( (levels(train$KEYWORD)[top_trans_vec[samp_ind]]) )


counts <- count(train$Key_ENGNO)
rel_counts <-counts$freq / N
par(las=2)
plot(counts$x, rel_counts)
counts <- count(train$KEYWORD)
rel_counts <-counts$freq / N
par(las=2)
plot(counts$x, rel_counts)

# HOME vs. ABROAD
par(mfrow = c(1,2))

se_key <- train$KEYWORD[!train$FOREIGN]
counts <- count(se_key)
rel_counts <-counts$freq / sum(counts$freq)
par(las=2)
plot(counts$x, rel_counts, main = "Sweden")

foreign_key <- train$KEYWORD[train$FOREIGN]
counts <- count(foreign_key)
rel_counts <-counts$freq / sum(counts$freq)
par(las=2)
plot(counts$x, rel_counts, main = "Abroad")

# WEEKDAY / WEEKEND
par(mfrow = c(1,2))

helg_key <- train$KEYWORD[train$WKND]
counts <- count(helg_key)
rel_counts <-counts$freq / sum(counts$freq)
par(las=2)
plot(counts$x, rel_counts, main = "Helg")

vardag_key <- train$KEYWORD[!train$WKND]
counts <- count(vardag_key)
rel_counts <-counts$freq / sum(counts$freq)
par(las=2)
plot(counts$x, rel_counts, main = "Vardag")

mix_vec <- train$WKND & train$FOREIGN

# KRONOR / OREN
par(mfrow = c(1,2))

dec_bool <- as.numeric(train$DEC) > 1.5

kronor_key <- train$KEYWORD[dec_bool]
counts <- count(kronor_key)
rel_counts <-counts$freq / sum(counts$freq)
par(las=2)
plot(counts$x, rel_counts, main = "Keyword distr, integer amounts")

oren_key <- train$KEYWORD[!dec_bool]
counts <- count(oren_key)
rel_counts <-counts$freq / sum(counts$freq)
par(las=2)
plot(counts$x, rel_counts, main = "Keyword distr, amount w/ decimal")

mix_vec <- train$WKND & train$FOREIGN

# END_9
par(mfrow = c(1,2))

dec_bool <- train$DEC > 1.5

end9_key <- train$KEYWORD[end_vec]
counts <- count(end9_key)
rel_counts <-counts$freq / sum(counts$freq)
n_9 <- rel_counts[4]
par(las=2)
plot(counts$x, rel_counts, main = "...9:-")

end0_key <- train$KEYWORD[!end_vec]
counts <- count(end0_key)
rel_counts <-counts$freq / sum(counts$freq)
n_0 <- rel_counts[4]
par(las=2)
plot(counts$x, rel_counts, main = "Inte 9")

mix_vec <- train$WKND & train$FOREIGN


##
par(mfrow = c(1,2))
salary_vec <- ( (train$DAY == 25)* !train$WKND ) > 0.5
sal_key <- train$KEYWORD[salary_vec]
aba <- count(sal_key)
plot(aba$x, aba$freq/sum(aba$freq))

par(mfrow = c(1,2))
aba <- count(train$KEYWORD)
plot(aba$x, aba$freq/sum(aba$freq))
