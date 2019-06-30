## Interpretable Machine Learning: Wrapper Functions 

## Wrapper for training Random Forest model with mlr package

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

## Function to calculate the PFI for all features of interest

calculate_PFI <- function(data, features, target, mod, mid, local = TRUE, replace.ids){
  
  
  pfi <- lapply(features, function(feat){
    imp <- featureImportance(mod, data = data, features = list(feat), target = target, mid = "mse", local = TRUE, replace.ids = obs.id)
    summary(imp)
    imp$importance
  })
  
  pfi <- setNames(pfi, features)
  pfi <- rbindlist(pfi)
  
  return(pfi)
  
}



## Function to identify the observation with the median mse contribution to a feature's Importance

whichmedian <- function(x) which.min(abs(x - median(x)))