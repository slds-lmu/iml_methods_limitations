

## load packages 
library(mlr)


## Wrapper for training Random Forest model with mlr package
## Input: 
## target: corresponds to reponse variable in the model
## data: full dataset, including response variable and all explanatory features
## training_size: specify the share of data use for training the algorithm: defined between 0 and 1
## n_tree: specify the number of tree used for fitting the random forest model
## Output:
## function returns a list of values: test, train, mod
## test: returns the test data
## training: returns the training data
## mod: returns the algorithm, obtained by fitting the model on the data
train_mlr_model <- function(target, data, training_size, n_tree){
  
  lrn <- makeLearner("regr.randomForest", ntree = n_tree)
  # Create Task: 
  task <- makeRegrTask(data = data, target = target)
  # Create indices for train and test data
  n <- getTaskSize(task)
  train.ind <- sample(n, size = training_size*n)
  test.ind <- setdiff(1:n, train.ind)
  
  # Get test and train data  
  test <- getTaskData(task, subset = test.ind)
  train <- getTaskData(task, subset = train.ind)
  
  # Train algorithm with mlr
  mod <- train(lrn, task)
  mod_train <- train(lrn, task, subset = train.ind)
  getLearnerModel(mod)
  summary(mod)
  parameter_list <- list(test = test, train = train, mod = mod)
  return(parameter_list)
  
}

## Wrapper function to calculate the PFI for all features of interest
## Input: 
## data: corresponds to the whole dataset that was used when training and validating the algorithm
## features: specify the features for which the Feature Importance needs to be calculated
## target: specify the response variable from the data, must be the same as the repsonse defined for fitting the algorithm
## mod: corresponds to the model fitted. Derived by function train_mlr_model
## mid: performance measure. Based on that the Feature Importance is evaluated. Per default: "mse"
## local: TRUE/FALSE => specifies whether the local or global Feature Importance shall be outputted. Default: TRUE
## replace.ids: specify the grid points used => randomly sample number of observations
## Output: 
## returns the Feature Importance for each feature

calculate_PFI <- function(data, features, target, mod, mid, local = TRUE, replace.ids){
  
  
  pfi <- lapply(features, function(feat){
    imp <- featureImportance(mod, data = data, features = list(feat), target = target, mid = mid, local = TRUE, replace.ids = obs.id)
    summary(imp)
    imp$importance
  })
  
  pfi <- setNames(pfi, features)
  pfi <- rbindlist(pfi)
  
  return(pfi)
  
}



## Function to identify the observation with the median mse contribution to a feature's Importance

whichmedian <- function(x) which.min(abs(x - median(x)))
