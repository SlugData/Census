library(gbm)

load("data/train.RData")
train.orig <- train
train.all <- train

# Remove columns that contain duplicated info (e.g. state names and state codes)
# Also remove columns that are margin of errors
excl <- !(1:ncol(train.all) %in% c(1, 3, 5, 8, 9, grep(".*MOE.*", names(train.all))))
train.all <- train.all[, excl]

# Reserve 1/5 as test data
k <- 5
n <- floor(nrow(train.all)/k)
s <- sample(1:nrow(train.all), nrow(train.all), replace=F)
train <- train.all[s[1:((k-1)*n)], ]
test <- train.all[s[((k-1)*n+1):length(s)], ]
test_Tot_Population_CEN_2010 <- test$Tot_Population_CEN_2010

# Regularize data
for (col in names(train)[1:(ncol(train)-2)]) {
  if (length(unique(train[, col])) <= 51)
    next
  # Train set
  train[, col] <- (train[, col] - mean(train[, col], na.rm=T))/sd(train[, col], na.rm=T)
  train[is.na(train[,col]), col] <- 0
  # Test set
  test[, col] <- (test[, col] - mean(test[, col], na.rm=T))/sd(test[, col], na.rm=T)
  test[is.na(test[,col]), col] <- 0
}

#choose some columns to use:
indices_to_use <- 1:(ncol(train)-2)

M <- 100 # number of iterations, i.e., n.trees
nu <- 0.1 # shrinkage
eta <- 0.2 # bag.fraction

bt <- gbm.fit(distribution = "laplace",
	  x = train[, indices_to_use],
	  y = train$Mail_Return_Rate_CEN_2010,
	  n.trees = M,
	  shrinkage = nu,
	  bag.fraction = eta,
	  verbose = TRUE)
bt_preds <- predict(bt, test[, indices_to_use], n.trees=M)

# Evaluate weighted MAE of the test set
wmae <- function(prediction, data, weights) {
  weighted.mean(abs(prediction - data), weights)
}

wmae(bt_preds, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)

# Plot prediction vs actual
# library(ggplot2)
# ggplot(data.frame(obs=test$Mail_Return_Rate_CEN_2010, pred=rf_preds), aes(obs, pred)) + geom_abline() + geom_point(color="blue", alpha=0.2)
