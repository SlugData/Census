library(randomForest)
library(gbm)
library(glmnet)
library(lars)
library(BayesTree)

wmae <- function(prediction, data, weights) {
  weighted.mean(abs(prediction - data), weights)
}

load("data/processed_data.RData")
train.all <- processed.data
train.all[is.na(train.all)] <- 0
for (col in names(train.all)) {
  if (class(train.all[, col]) != "character")
    train.all[is.infinite(train.all[, col]), col] <- 0  
}

# Reserve 1/10 as test data
set.seed(20130207) # Set random seed for reproducible results
k <- 10
n <- floor(nrow(train.all)/k)
s <- sample(1:nrow(train.all), nrow(train.all), replace=F)
train <- train.all[s[1:((k-2)*n)], ]
blend <- train.all[s[((k-2)*n+1):((k-1)*n)], ]
test <- train.all[s[((k-1)*n+1):length(s)], ]
test_Tot_Population_CEN_2010 <- test$Tot_Population_CEN_2010


#choose some columns to use:
# indices_to_use = apply(array(1:(ncol(train)-2)), 1, function(i) sum(is.na(train[,i])) == 0 & sum(is.na(test[,i])) == 0)
# indices_to_use = apply(array(1:170), 1, function(i) sum(is.na(train[,i])) == 0)
# indices_to_use <- 1:(ncol(train)-2)

indices_to_use_no_str <- 5:(ncol(train)-2)
indices_to_use <- c(1L, indices_to_use_no_str) # Add State

# ---- Random Forest ----
# rf = randomForest(train[,which(indices_to_use)], y=train$Mail_Return_Rate_CEN_2010, ntree=50, do.trace=T, sampsize=5000)
rf <- randomForest(train[, indices_to_use_no_str], y=train$Mail_Return_Rate_CEN_2010,
                  ntree=2000, do.trace=T, sampsize=10000, keep.forest=T)
rf_preds <- predict(rf, blend[, indices_to_use_no_str])
rf_preds.test <- predict(rf, test[, indices_to_use_no_str])

# ---- GBM Models ----
M <- 2000 # number of iterations, i.e., n.trees
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
# gl <- glmnet(as.matrix(train[, indices_to_use]), as.vector(train$Mail_Return_Rate_CEN_2010), family="gaussian")
# gl_preds <- predict(gl, as.matrix(blend[, indices_to_use]), s=0.01)
# gl_preds.test <- predict(gl, as.matrix(test[, indices_to_use]), s=0.01)

# ---- LARS Lasso ----
las <- lars(as.matrix(train[, indices_to_use_no_str]), as.vector(train$Mail_Return_Rate_CEN_2010))
las_preds <- predict(las, as.matrix(blend[, indices_to_use_no_str]))
las_preds.test <- predict(las, as.matrix(test[, indices_to_use_no_str]))
ind <- which.min(sapply(1:ncol(las_preds.test$fit), function(i) 
  wmae(las_preds.test$fit[,i], test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)))

# ---- BART ----
brt <- bart(train[, indices_to_use], train$Mail_Return_Rate_CEN_2010, 
            x.test=rbind(blend[, indices_to_use], test[, indices_to_use]), ntree=500)
brt_preds <- colMeans(brt$yhat.test[, 1:nrow(blend)])
brt_preds.test <- colMeans(brt$yhat.test[, (nrow(blend)+1):ncol(brt$yhat.test)])

# ---- Blend models ----
blend.data <- data.frame(y = blend$Mail_Return_Rate_CEN_2010,
                         x1 = rf_preds,
                         x2 = bt_preds,
                         x3 = las_preds$fit[, ind],
                         x4 = brt_preds)
blend.mod <- lm(y ~ x1 + x2 + x3 + x4, blend.data)

test.input <- data.frame(x1 = predict(rf, test[, indices_to_use_no_str]),
                         x2 = predict(bt, test[, indices_to_use], n.trees=M),
                         x3 = las_preds.test$fit[, ind],
                         x4 = brt_preds.test)
test.output <- predict(blend.mod, test.input)

# blend.data <- data.frame(y = blend$Mail_Return_Rate_CEN_2010,
#                          x1 = rf_preds,
#                          x2 = bt_preds,
#                          x3 = las_preds$fit[, ind])
# blend.mod <- lm(y ~ x1 + x2 + x3, blend.data)
# 
# test.input <- data.frame(x1 = predict(rf, test[, indices_to_use]),
#                          x2 = predict(bt, test[, indices_to_use], n.trees=M),
#                          x3 = las_preds.test$fit[, ind])
# test.output <- predict(blend.mod, test.input)

# ---- Evaluate weighted MAE of the test set ----
cat(sprintf("%-15s: %7.5f\n", "Random Forest", wmae(rf_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)))
cat(sprintf("%-15s: %7.5f\n", "GBM", wmae(bt_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)))
cat(sprintf("%-15s: %7.5f\n", "LASSO", wmae(las_preds.test$fit[, ind], test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)))
cat(sprintf("%-15s: %7.5f\n", "BART", wmae(brt_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)))
cat(sprintf("%-15s: %7.5f\n", "All Blend", wmae(test.output, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)))
# wmae(gl_preds.test, test$Mail_Return_Rate_CEN_2010, test_Tot_Population_CEN_2010)
