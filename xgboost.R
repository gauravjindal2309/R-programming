xgboost(data = X, 
        booster = "gbtree", 
        objective = "binary:logistic", 
        max.depth = 5, 
        eta = 0.5, 
        nthread = 2, 
        nround = 2, 
        min_child_weight = 1, 
        subsample = 0.5, 
        colsample_bytree = 1, 
        num_parallel_tree = 1)



xgboost(data = X, 
        booster = "gblinear", 
        objective = "binary:logistic", 
        max.depth = 5, 
        nround = 2, 
        lambda = 0, 
        lambda_bias = 0, 
        alpha = 0)


df <- df[-grep('foo', colnames(df))]
df = subset(mydata, select = -c(x,z) )

xgb <- xgboost(data = data.matrix(X), 
                  label = y, 
                  eta = 0.1,
                  max_depth = 15, 
                  nround=25, 
                  subsample = 0.5,
                  colsample_bytree = 0.5,
                  seed = 1,
                  eval_metric = "merror",
                  objective = "multi:softprob",
                  num_class = 12,
                  nthread = 3
)

# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-1]))