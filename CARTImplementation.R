#########################################################################################################
############################################# MAIN ######################################################
#########################################################################################################

# get the preProcessedData
# data_clean <- read.csv(file="/Users/paurakhrajbhandary/Documents/autumn2014/CS229ML/Project/clean_data.csv",head=TRUE,sep=",")
source('/Users/paurakhrajbhandary/Documents/autumn2014/CS229ML/Project/PreProcessData.R')
source('/Users/paurakhrajbhandary/Documents/autumn2014/CS229ML/Project/functionsList.R')

# recursive partition CART algorithm
pload("rpart")
# http://cran.r-project.org/web/packages/rpart/vignettes/longintro.pdf
# http://cran.r-project.org/web/packages/rpart/rpart.pdf

# split the data into training and test
idx = test_train_indicies(nrow(data_clean), 2)
d_train = data_clean[idx$train,]
d_test  = data_clean[idx$test,]

#-----------------------------------------------------------------------------------------
# set modeling parameter and construct model

# set r part cv parameters
rc_cv = rpart.control(xval = 10, cp = .0001)

# construct model with CV
rpart.cv.info = rpart(V280~., data = data_clean,  control = rc_cv, parms = list(split = "information"))
plot_rpart(rpart.cv.info, d_test)

rpart.cv.info = rpart(V280~., data = data_train,  control = rc_cv, parms = list(split = "information"))
a <- as.matrix(predict(rpart.cv.info, d_test, type="vector")) # HERE ARE YOUR PREDICTIONS

error_test = as.numeric(predict(rpart.cv.info, d_test, type="vector")) - as.numeric(d_test[, ncol(d_test)])
mse(error_test)
# [1] 7.309735

error_train = as.numeric(predict(rpart.cv.info, d_train, type="vector")) - as.numeric(d_train[, ncol(d_train)])
mse(error_train)
# [1] 11.73894





get_cp_miss(rpart.cv.info, d_test)


# also try using the GINI index these are measures of "node purity"
rpart.cv.gini = rpart(
  ANNUAL_INCOME~.,
  data = d_train, 
  control = rc_cv,
  parms = list(split = "gini")
)

#----------------k-fold cross validation for Linear kLab SVM---------------
#data_clean[,c(1,266)] <- data_clean[, c(266,1)]
library(caret)
ctrl <- trainControl(method = "cv", savePred = T, classProb = F)
# mod <- train(Species ~ . , data = iris, method = "svmLinear", trControl = ctrl)
mod <- train(V280 ~ . , data = data_clean, method = "svmLinear", trControl = ctrl)

head(mod$pred)

accuracy = sum(mod$pred$pred == mod$pred$obs) / nrow(as.matrix(mod$pred$pred)) #45 #nrow(data_clean)
print (accuracy)
mod

