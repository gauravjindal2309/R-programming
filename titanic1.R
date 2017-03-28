train <- read.csv("~/R programming/Analytics Vidhya/Titanic/train.csv")
>   View(train)
> test <- read.csv("~/R programming/Analytics Vidhya/Titanic/test.csv")
>   View(test)
> genderclassmodel <- read.csv("~/R programming/Analytics Vidhya/Titanic/genderclassmodel.csv")
>   View(genderclassmodel)
> gendermodel <- read.csv("~/R programming/Analytics Vidhya/Titanic/gendermodel.csv")
>   View(gendermodel)
> train$PassengerId = NULL
> Passenger_ID = test$PassengerId
> test$PassengerId = NULL
train$Name =NULL
> train$Ticket =NULL
> train$Cabin = NULL
> test$Name =NULL
> test$Ticket =NULL
> test$Cabin = NULL
train$Sex = as.numeric(train$Sex)
> train$Embarked[train$Embarked ==""] = NA
> train$Embarked = as.numeric(train$Embarked)-1



test$Sex = as.numeric(test$Sex)
test$Embarked[test$Embarked ==""] = NA
test$Embarked = as.numeric(test$Embarked)

set.seed(145)
train=complete(mice(train))
test=complete(mice(test))

logit = glm(Survived ~.,data=train, family = binomial)
predlog = predict(logit, newdata = test)
pred_log=rep("1",nrow(test))
for (i in 1:nrow(test))
  + {
    +     if(predlog[i]>=.73)
      +     {next()}
    +     else 
      +     {
        +         pred_log[i]="N"
         }}
table(pred_log)
MySubmission = data.frame(PassengerId = Passenger_ID,Survived=pred_log)

write.csv(MySubmission, "Submissionlog.csv", row.names=FALSE)

install.packages("e1071")
require(e1071)
svm=svm(Survived ~.,data=train,kernel="radial")
pred_svm=predict(svm,newdata=test)
table(pred_svm)
MySubmission = data.frame(PassengerId = Passenger_ID,Survived=pred_svm)
write.csv(MySubmission, "SubmissionSVM.csv", row.names=FALSE)


predsvm=rep("1",nrow(test))
for (i in 1:nrow(test))
  + {
    +     if(pred_svm[i]>=.5)
      +     {next()}
    +     else 
      +     {
        +         predsvm[i]="0"
      
        +  }
    +  }
table(predsvm)

require(randomForest)
forest=randomForest(Survived ~.,data=train,ntree=100,nodesize=21,cp=.39)
pred=predict(forest,newdata = test)
table(pred)
MySubmission = data.frame(PassengerId = Passenger_ID,Survived=pred)
write.csv(MySubmission, "SubmissionForest.csv", row.names=FALSE)
pred_f = rep("1",nrow(test))
for (i in 1:nrow(test))
  + {if(pred[i]>=.5)
    + {next()}
    +     else
      +         {pred_f[i]="0"}}
> table(pred_f)
MySubmission = data.frame(PassengerId = Passenger_ID,Survived=pred_f)
write.csv(MySubmission, "SubmissionForest.csv", row.names=FALSE)



train_x =train[,-1]
label=train$Survived

bst=xgboost(data = as.matrix(train_x),label = label,max.depth = 20, eta = 1, nthread = 4, nround = 10, objective = "binary:logistic")
pred=predict(bst,as.matrix(test))
pred_bst = as.numeric(pred > 0.31)
table(pred_bst)
pred_bst=factor(pred_bst,levels = c(0,1),labels = c("N","Y"))
table(pred_bst)
MySubmission = data.frame(PassengerId = Passenger_ID,Survived=pred_bst)
write.csv(MySubmission, "SubmissionXG.csv", row.names=FALSE)



aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
