library(randomForest)
library(gbm)
library(glmnet)

load("../data/train.RData")
train.orig <- train
train.all <- train

# Remove columns that contain duplicated info (e.g. state names and state codes)
# Also remove columns that are margin of errors
excl <- !(1:ncol(train.all) %in% c(1, 3, 5, 8, 9, grep(".*MOE.*", names(train.all))))
train.all <- train.all[, excl]

# Reserve 1/10 as test data
set.seed(20130207) # Set random seed for reproducible results
k <- 10
n <- floor(nrow(train.all)/k)
s <- sample(1:nrow(train.all), nrow(train.all), replace=F)
train <- train.all[s[1:((k-2)*n)], ]
blend <- train.all[s[((k-2)*n+1):((k-1)*n)], ]
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

# ---- Regularize data ----
for (col in names(train)[1:(ncol(train)-2)]) {
  if (length(unique(train[, col])) <= 51)
    next
  # Train set
  train[, col] <- (train[, col] - mean(train[, col], na.rm=T))/sd(train[, col], na.rm=T)
  train[is.na(train[,col]), col] <- 0
  # Test set
  test[, col] <- (test[, col] - mean(test[, col], na.rm=T))/sd(test[, col], na.rm=T)
  test[is.na(test[,col]), col] <- 0
  # Blend set
  blend[, col] <- (blend[, col] - mean(blend[, col], na.rm=T))/sd(blend[, col], na.rm=T)
  blend[is.na(blend[,col]), col] <- 0  
}

#choose some columns to use:
# indices_to_use = apply(array(1:(ncol(train)-2)), 1, function(i) sum(is.na(train[,i])) == 0 & sum(is.na(test[,i])) == 0)
# indices_to_use = apply(array(1:170), 1, function(i) sum(is.na(train[,i])) == 0)
indices_to_use <- 1:(ncol(train)-2)

# ---- Random Forest ----
# rf = randomForest(train[,which(indices_to_use)], y=train$Mail_Return_Rate_CEN_2010, ntree=50, do.trace=T, sampsize=5000)
rf <- randomForest(train[, indices_to_use], y=train$Mail_Return_Rate_CEN_2010,
                  ntree=100, do.trace=T, sampsize=10000, keep.forest=T)
rf_preds <- predict(rf, blend[, indices_to_use])
rf_preds.test <- predict(rf, test[, indices_to_use])

# ---- GBM Models ----
M <- 500 # number of iterations, i.e., n.trees
nu <- 0.1 # shrinkage
eta <- 0.2 # bag.fraction

bt <- gbm.fit(distribution = "laplace",
              x = train[, indices_to_use],
              y = train$Mail_Return_Rate_CEN_2010,
              n.trees = M,
              shrinkage = nu,
              bag.fraction = eta,
              verbose = TRUE)
bt_preds <- predict(bt, blend[, indices_to_use], n.trees=M)
bt_preds.test <- predict(bt, test[, indices_to_use], n.trees=M)

# ---- GLM Lasso ----
gl <- glmnet(as.matrix(train[, indices_to_use]), as.vector(train$Mail_Return_Rate_CEN_2010), family="gaussian")
gl_preds <- predict(gl, as.matrix(blend[, indices_to_use]), s=0.01)
gl_preds.test <- predict(gl, as.matrix(test[, indices_to_use]), s=0.01)

# ---- Blend models ----
blend.data <- data.frame(y = blend$Mail_Return_Rate_CEN_2010,
                         x1 = rf_preds,
                         x2 = bt_preds,
                         x3 = as.vector(gl_preds))
blend.mod <- lm(y ~ x1 + x2 + x3, blend.data)

test.input <- data.frame(x1 = predict(rf, test[, indices_to_use]),
                         x2 = predict(bt, test[, indices_to_use], n.trees=M),
                         x3 = as.vector(predict(gl, as.matrix(test[, indices_to_use]), s=0.01)))
test.output <- predict(blend.mod, test.input)

# ---- Evaluate weighted MAE of the test set ----
wmae <- function(prediction, data, weights) {
  weighted.mean(abs(prediction - data), weights)
}

wmae(test.output, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)
wmae(rf_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)
wmae(bt_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)
wmae(gl_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)

# ---- Plot prediction vs actual ----
# library(ggplot2)
# ggplot(data.frame(obs=test$Mail_Return_Rate_CEN_2010, pred=test.output), aes(obs, pred)) + geom_abline() + geom_point(color="blue", alpha=0.2)
# plot(blend.data)