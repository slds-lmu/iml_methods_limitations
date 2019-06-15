## Identification of Interaction Effects in Feature Importance models
## install packages
install.packages("rpart")
install.packages("party")
install.packages("partykit", repos = "http://R-Forge.R-project.org")

## load packages
library(data.table)
library(mlr)
library(featureImportance)
library(mlbench)
library(ggplot2)
library(gridExtra)
library(rpart)
library(party)
library(partykit)
library(randomForest)

## Input: 
## pfi: individual/local Feature Importance of variables
## set of covariates, explaining the feature importance of variable X
## type of model used: 1) Decision Tree (one stump) or Random Forest
## model recommendation: 
## Decision Tree, if only one variable seems to be interacting
## Random Forest, if several variable seem to interact 
## explain different use case. 
## Return:
## Estimation results from Decision Tree or Random Forest

PFI_interaction_identifier <- function(pfi, data, features, model = "Decision Tree", tree_depth, n_tree){
  ## include assertions
  ##if (getOption("warn") > 0) {
  ##  stopifnot(
  ##    is.data.frame(pfi),
  ##    is.data.frame(data),
  ##    check for model input ("DecisionTree" or "RandomForest")
  ##    is.numeric(tree_depth),
  ##    a > 0,
  ##    length(a) == 1,
  ##    is.character(b),
      # Other requirements go here
  ##  )
  ##}
  

  ## calculate individual, aggregated feature importance
  ## merge feature importance on test data to run interact effects detection model 
  for(feat in features){
    ici <- subset(pfi, features == feat)
    ici.integral <- ici[, lapply(.SD, mean, na.rm = TRUE), .SDcols = mid, by = "row.id"]
    data[, paste0("imp", feat)] <- ici.integral$mse
  }
  
  data <- data
  print(colnames(data))
  print(is.data.frame(data))
  
  n_features <- length(features)
  formular <- list(length(n_features))
  model_results <- list(length(n_features))
  
  ## Different models can be applied
  if(model == "Decision Tree"){
    
    
    ## loop through formular
    ## and fit model for each feature respectively
    for(feat in features){
      
      var_imp <- paste0("imp", feat)
      var_index_pfi <- which(unique(pfi$features) == feat)
      var_predict <- paste(unique(pfi$features)[-var_index_pfi], collapse = " + ")
      
      
      formular[[feat]] <- paste(var_imp, " ~ ", var_predict)
      model_results[[feat]] <- rpart(formular[[feat]], data = data, maxdepth = tree_depth)
      
    }
    
   
  }
  
  if(model == "RandomForest"){
    
    for(feat in features){
    
    var_index_pfi <- which(unique(pfi$features) == feat) 
    var_predict <- as.vector(unique(pfi$features)[-var_index_pfi])
    var_imp <- paste0("imp", feat)
    data_variables <- c(var_predict, var_imp)
    data_RF <- data[, data_variables]
      ## Create Learner: Specify the machine learning algorithm with the mlr package
      
    lrn <- makeLearner("regr.randomForest", ntree = n_tree)
      
    ## Create Task: Create regression task for mlr
      
    task <- makeRegrTask(data = data_RF, target = var_imp)
      
    mod <- train(lrn, task, subset = NULL)
    
    task.pred <- predict(mod, task = task)
    performance <- performance(task.pred)
      
    }
    
  }
  
  
  print(head(data))
  print(formular)
  print(model_results)
  print(mod)
  print(task.pred)
  print(performance)
}

### test code snippets






#### test line

PFI_interaction_identifier(pfi, data = test, features = c("X_1", "X_2"), model = "Decision Tree", tree_depth = 1)
PFI_interaction_identifier(pfi, data = test, features = c("X_1", "X_2"), model = "RandomForest", n_tree = 10)


## plot results from PFI_interaction_identifier
plot_interaction_detect <- function(data, method){
  
  
  plot(as.party(rp))
}









