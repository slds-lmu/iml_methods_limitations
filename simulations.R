## Simulation Data

# specify data
set.seed(1)
sig = diag(4)
n = 10000
generateY_sim1 = function(X) {
  eps = rnorm(nrow(X), sd = 0.5)
  form = ~ V1 + V2 + V3 + V4 + V1:V2 + V2:V4 - 1
  #V1 + V2 + ifelse(V3 == 0, I(10*V1), I(10*V2)) - 1
  mat = model.matrix(form, data = X)
  rowSums(mat) + eps
}
X_sim1 = as.data.frame(mvrnorm(n, mu = rep(0, ncol(sig)), Sigma = sig))
#X$V3 = rbinom(n, size = 1, prob = 0.5)
X_sim1$y = generateY(X_sim1)

##
generateY_sim2 <- function(X){
  eps = rnorm(nrow(X), sd = 0.5)
  form = ~ V1 + V2 + ifelse(V3 == 0, I(10*V1), I(10*V2)) - 1
  mat = model.matrix(form, data = X)
  rowSums(mat) + eps
}



## calculate PFI for each simulation data, as this is the initial start of everything

## 1. step: train model
#### Herefore, build a mlr wrappper, including all relevant steps
## 2. step: Calculate PFI for each feature

## Create Learner: Specify the machine learning algorithm with the mlr package


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
  
  # Train algorithm with mlr
  mod <- train(lrn, task)
  mod_train <- train(lrn, task, subset = train.ind)
  getLearnerModel(mod)
  summary(mod)
  
  return(mod)
  
}


## Computes the Permutation Feature Importance for features [inlucdes also local feature importance]
calculate_PFI <- function(data, features, target, mod, mid, local = TRUE, replace.ids){
  
  
  pfi <- lapply(feat, function(features){
    imp <- featureImportance(mod, data = test, features = list(features), target = target, mid = "mse", local = TRUE, replace.ids = obs.id)
    
    return(imp$importance)
  })
  
  
}


