train=read.csv("Train_UWu5bXk.csv")
test=read.csv("Test_u94Q5KV.csv")

summary(train)
train$Item_Identifier =NULL
train$Outlet_Identifier = NULL
Item_Identifier = test$Item_Identifier
Outlet_Identifier = test$Outlet_Identifier
test$Item_Identifier = NULL
test$Outlet_Identifier =NULL

train$Item_Fat_Content = as.numeric(train$Item_Fat_Content)
test$Item_Fat_Content = as.numeric(test$Item_Fat_Content)
train$Outlet_Size[train$Outlet_Size ==""]=NA
test$Outlet_Size[test$Outlet_Size ==""]=NA
train$Outlet_Size = as.numeric(train$Outlet_Size)-1
test$Outlet_Size = as.numeric(test$Outlet_Size) -1
train$Outlet_Location_Type = as.numeric(train$Outlet_Location_Type)
test$Outlet_Location_Type = as.numeric(test$Outlet_Location_Type)
train$Outlet_Type = as.numeric(train$Outlet_Type)
test$Outlet_Type = as.numeric(test$Outlet_Type)


require(mice)
set.seed(145)
train=complete(mice(train))
test=complete(mice(test))

Item_type_train = train$Item_Type
Item_type_test = test$Item_Type
test$Item_Type = NULL
train$Item_Type = NULL

# linear regression
# score = 1271.663247
linear = lm(Item_Outlet_Sales ~., data= train)
pred_linear = predict(linear, newdata =test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_linear)
write.csv(Mysubmission,"Submission_linear.csv", row.names = FALSE)

# tree
# score = 1174.337015
require(rpart)
require(rpart.plot)
tree = rpart(Item_Outlet_Sales ~., data= train,minbucket=5,cp=.01)
prp(tree)
pred_tree=predict(tree,newdata = test,type = "class")
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_tree)
write.csv(Mysubmission,"Submission_tree.csv", row.names = FALSE)

#randomforest 
# score = 1166.72676
require(randomForest)
forest=randomForest(Item_Outlet_Sales~.,data=train,ntree=100,nodesize=21,cp=.39)
pred_rf=predict(forest,newdata = test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_rf)
write.csv(Mysubmission,"Submission_rf.csv", row.names = FALSE)

#SVM 
# score = 1167.79197
require(e1071)
svm=svm(Item_Outlet_Sales~.,data=train,kernel="radial")
pred_svm=predict(svm,newdata=test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_svm)
write.csv(Mysubmission,"Submission_svm.csv", row.names = FALSE)

#XGB 
# score = 1427.58821
train_x =train
train_x$item_Outlet = NULL
label=train$Item_Outlet_Sales
label = as.numeric(label)
bst=xgboost(data = as.matrix(train_x),label = label,max.depth = 20, eta = 1, nthread = 4, nround = 50, objective = "reg:linear")
pred_xgb=predict(bst,as.matrix(test))
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_xgb)
write.csv(Mysubmission,"Submission_xgb.csv", row.names = FALSE)

# Cross validation
install.packages("DAAG")   # if not already installed 
library(DAAG)
cv_linear =cv.lm(df = train, form.lm = formula(Item_Outlet_Sales ~ .))

install.packages("caret")
library(caret)
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
rf_cv <- train(Item_Outlet_Sales ~., data= train, trControl=train_control, method="rf")
pred_rf_cv=predict(rf_cv,newdata = test)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_rf_cv)
write.csv(Mysubmission,"Submission_rf_cv.csv", row.names = FALSE)


inTraining <- createDataPartition(train$Item_Outlet_Sales, p = .75, list = FALSE)
training <- train[ inTraining,]
testing  <- train[-inTraining,]
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(825)
linear_cv <- train(Item_Outlet_Sales ~., data= training, trControl=train_control, method="xgbLinear")
pred_linear_cv=predict(linear_cv,newdata = testing)
Mysubmission = data.frame(Item_Identifier = Item_Identifier, Outlet_Identifier = Outlet_Identifier, Item_Outlet_Sales = pred_linear_cv)
write.csv(Mysubmission,"Submission_linear_cv.csv", row.names = FALSE)

RSME = sqrt(sum((testing$Item_Outlet_Sales-pred_linear_cv)^2)/nrow(testing))
RSME

1089.947  ntree=100,nodesize=21,cp=0.39
1088.134  ntree=100,nodesize=10,cp=0.39
1084.825  ntree=100,nodesize=10,cp=0.3

gbm1 <- gbm(Class ~ ., data = training,                  # dataset
    
    distribution="gaussian",     # see the help for other choices
    n.trees=100,                # number of trees
    shrinkage=0.05,              # shrinkage or learning rate,
    # 0.001 to 0.1 usually work
    interaction.depth=1,         # 1: additive model, 2: two-way interactions, etc.
    bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
    train.fraction = 0.5,        # fraction of data for training,
    # first train.fraction*N used for training
    n.minobsinnode = 10,         # minimum total weight needed in each node
    cv.folds = 3,                # do 3-fold cross-validation
    keep.data=TRUE,              # keep a copy of the dataset with the object
    verbose=FALSE,               # don't print out progress
    n.cores=1)
