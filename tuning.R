# xgboost task parameters
nrounds <- 100
folds <- 10
obj <- 'reg:linear'
eval <- 'RMSE'

# Parameter grid to search
params <- list(
  eval_metric = eval,
  objective = obj,
  eta = c(0.1,0.01),
  max_depth = c(4,6,8,10),
  max_delta_step = c(0,1),
  subsample = 1,
  scale_pos_weight = 1
)


  fit <- xgb.cv(
    data = data.matrix(train_x), 
    label = label, 
    param =params, 
    missing = NA, 
    nfold = folds, 
    prediction = FALSE,
    early.stop.round = 50,
    maximize = FALSE,
    nrounds = nrounds
  
    
    
  #train_caret 
    cv.ctrl <- trainControl(method = "repeatedcv", repeats = 1,number = 3, 
                            #summaryFunction = twoClassSummary,
                            classProbs = TRUE,
                            allowParallel=T)
    
    xgb.grid <- expand.grid(nrounds = 100,
                            eta = c(0.01,0.05,0.1),
                            max_depth = c(2,4,6,8,10,14),
                            colsample_bytree = NULL, min_child_weight =NULL, subsample = NULL)
    set.seed(45)
    
    

    library(caret) # for dummyVars
    library(RCurl) # download https data
    library(Metrics) # calculate errors
    library(xgboost) # model
    
    outcomeName <- c('Item_Outlet_Sales')
    # list of features
    predictors <- names(train)[!names(train) %in% outcomeName]
    
    trainPortion <- floor(nrow(train)*0.5)
    trainSet <- train[ 1:floor(trainPortion/2),]
    testSet <- train[(floor(trainPortion/2)+1):trainPortion,]
    
    smallestError <- 100
    for (depth in seq(1,10,1)) {
      for (rounds in seq(1,20,1)) {
        
        # train
        bst <- xgboost(data = as.matrix(trainSet[,predictors]),
                       label = trainSet[,outcomeName],
                       max.depth=depth, nround=rounds,
                       objective = "reg:linear", verbose=0)
        gc()
        
        # predict
        predictions <- predict(bst, as.matrix(testSet[,predictors]), outputmargin=TRUE)
        err <- rmse(as.numeric(testSet[,outcomeName]), as.numeric(predictions))
        
        if (err < smallestError) {
          smallestError = err
          print(paste(depth,rounds,err))
        }     
      }
    }  
    
    
    