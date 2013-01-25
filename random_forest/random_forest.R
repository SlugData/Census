library(randomForest)

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

# Impute NAs with mean of column
# for (col in names(train)) {
#   if (mode(train[, col]) != "numeric")
#     next
#   # Train set
#   train[is.na(train[,col]), col] <- mean(train[, col], na.rm=T)
#   # Test set
#   test[is.na(test[,col]), col] <- mean(test[, col], na.rm=T)
# }

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
# indices_to_use = apply(array(1:(ncol(train)-2)), 1, function(i) sum(is.na(train[,i])) == 0 & sum(is.na(test[,i])) == 0)
# indices_to_use = apply(array(1:170), 1, function(i) sum(is.na(train[,i])) == 0)
indices_to_use <- 1:(ncol(train)-2)

# rf = randomForest(train[,which(indices_to_use)], y=train$Mail_Return_Rate_CEN_2010, ntree=50, do.trace=T, sampsize=5000)
rf <- randomForest(train[, indices_to_use], y=train$Mail_Return_Rate_CEN_2010, 
                  xtest=test[, indices_to_use],
                  ntree=500, do.trace=T, sampsize=10000, keep.forest=T)
rf_preds <- predict(rf, test[, indices_to_use])

# Evaluate weighted MAE of the test set
wmae <- function(prediction, data, weights) {
  weighted.mean(abs(prediction - data), weights)
}

wmae(rf_preds, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)

# Plot prediction vs actual
library(ggplot2)
ggplot(data.frame(obs=test$Mail_Return_Rate_CEN_2010, pred=rf_preds), aes(obs, pred)) + geom_abline() + geom_point(color="blue", alpha=0.2)