#Missing value treatment
#method1
library(mice)
miceMod <- mice(train[, !names(X) %in% "Loan_Status"], method="rf")
miceOutput <- complete(miceMod)
anyNA(miceOutput)
#> FALSE

#method2
library(DMwR)
knnOutput <- knnImputation(BostonHousing[, !names(BostonHousing) %in% "medv"])  # perform knn imputation.
anyNA(knnOutput)
#> FALSE

#method3
library(Hmisc)
impute(BostonHousing$ptratio, mean)  # replace with mean
impute(BostonHousing$ptratio, median)  # median
impute(BostonHousing$ptratio, 20)  # replace specific number
# or if you want to impute manually
BostonHousing$ptratio[is.na(BostonHousing$ptratio)] <- mean(BostonHousing$ptratio, na.rm = T)  # not run