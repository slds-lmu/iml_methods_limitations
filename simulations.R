## Simulation Data

library(MASS)

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


########## Simulation for subchapter 1 
set.seed(1)
n <- 1000
sig <- diag(2)


X_1 <- rnorm(n, mean = 0, sd = 1)
X_2 <- rnorm(n, mean = 2, sd = 4)
X_3 <- rbinom(n, size = 1, prob = 0.5)
eps <- rnorm(nrow(X), sd = 0.5)

X_sim1 <- as.data.frame(cbind(X_1, X_2, X_3))
X_sim1$y <- X_1 + 5*X_2 + ifelse(X_2 > 4, ifelse(X_3 == 0, 5*X_2, -10*X_2), 0) + eps

target_sim1 <- "y"
mlr_sim1 <- train_mlr_model(target_sim1, X_sim1, training_size = 0.8, n_tree = 100)
test_sim1 <- mlr_sim1[["test"]]
mod_sim1 <- mlr_sim1[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim1 <- calculate_PFI(test_sim1, c("X_1", "X_2"), mod_sim1, target = 'y', mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim1)
pfi_sim1$feature.value


## Plot 

pi.curve_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_sim1, ici.curves_sim1, nrow = 1)









#####################################################################################


X_1 <- rnorm(n, mean = 0, sd = 1)
X_2 <- rnorm(n, mean = 2, sd = 4)
X_3 <- rbinom(n, size = 1, prob = 0.5)
epsilon <- rnorm(n, mean = 0 ,sd = 0.5)

y <- X_1 + 5*X_2 + ifelse(X_3 == 0 & X_2 > 4, I(5*X_2), I(-10*X_2)) + epsilon


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
  parameter_list <- list(test = test, mod = mod)
  return(parameter_list)
  
}


## Computes the Permutation Feature Importance for features [includes also local feature importance]
calculate_PFI <- function(data, features, target, mod, mid, local = TRUE, replace.ids){
  
  
  pfi <- lapply(feat, function(features){
    imp <- featureImportance(mod, data = test, features = list(features), target = target, mid = "mse", local = TRUE, replace.ids = obs.id)
    imp$importance
  })
  
  pfi <- setNames(pfi, feat)
  pfi <- rbindlist(pfi)
  
  return(pfi)
  
}


calculate_conditional_PFI <- function(data, feature, group.var, threshold){
  ## include assertions
  ##if (getOption("warn") > 0) {
  ##  stopifnot(
  ##    is.data.frame(data),
  ##    
  ##    
  ##  )
  ##}
  
  
  ##subset dataset on each category based on threshold
  ##Input of 
  for(value in threshold){
    paste0("use", value) <- data[features == group.var & feature.value == value, unique(replace.id)]
    
  
  }
  
}


use0 <- pfi[features == "X_3" & feature.value == 0, unique(replace.id)]
length(use0) ## 13

use1 <- pfi[features == "X_3" & feature.value == 1, unique(replace.id)]
length(use1) ## 7 

iciX_1 <- subset(pfi, features == "X_1")
iciX_1[["X_3"]] <- as.factor(as.numeric(ici$row.id %in% use0))
pi_X_1_0 <- subset(iciX_1, row.id %in% use0 & replace.id %in% use0)
pi_X_1_1 <- subset(iciX_1, row.id %in% use1 & replace.id %in% use1)

iciX_2 <- subset(pfi, features == "X_2")
iciX_2[["X_3"]] <- as.factor(as.numeric(ici$row.id %in% use1))
pi_X_2_0 <- subset(iciX_1, row.id %in% use0 & replace.id %in% use0)
pi_X_2_1 <- subset(iciX_1, row.id %in% use1 & replace.id %in% use1)

