## Identification of Interaction Effects in Feature Importance models
## install packages
## install.packages("rpart")
## install.packages("party")
## install.packages("partykit", repos = "http://R-Forge.R-project.org")

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
library(sfsmisc)

#### Interaction Identifier with randomForest package instead of mlr package
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

## The function PFI_interaction_identifier_2() reveals between which features interaction takes place
## Therefore, fit the remaining features on the individual importance measures of the feature of interest
## The result will then indicate the feature with the highest explanatory power => reveals the interacting feature
## Input:
## pfi: data containing the pfi for each feature of interest
## data: test data of the original dataset
## mid: performance metric; per default "mse"
## model: if only one interaction effect => "Decision Tree", otherwise "RandomForest
## tree_depth: depth of the tree => if model ="Decision Tree" => tree_depth = 1 recommended
## n_tree: if model = "RandomForest" => number of trees to be specified
## Output:
## For model = "Decision Tree":
## formula: prints the formula with which the importance of the respective feature was calculated
## model_results: returns the results, which can be used for visualization
## For model= "RandomForest":
## importance: returns the variable importance for predictions with Random Forest

PFI_interaction_identifier_2 <- function(pfi, data, features, mid = "mse", model = c("Decision Tree", "RandomForest"), tree_depth, n_tree){
 
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
  
  
  print(head(data))
  
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
    
    print(head(data))
    print(formular)
    print(model_results)
    
    
    return(c(model_results = model_results, formular = formular))
    
  }
  
  if(model == "RandomForest"){
    
  importance <- list(length(n_features))
  importance_plot <- list(length(n_features))
    
    for(feat in features){
      var_imp <- paste0("imp", feat)
      var_index_pfi <- which(unique(pfi$features) == feat)
      var_predict <- paste(unique(pfi$features)[-var_index_pfi], collapse = " + ")
      
      
      formular[feat] <- paste(var_imp, " ~ ", var_predict)
      model_results[feat] <- randomForest(formular[feat], data = data, n_tree = n_tree, keep.forest =  FALSE,  importance = TRUE)
      importance[feat] <- importance(model_results[feat])
      importance_plot[feat] <- varImpPlot(model_results[feat])
      
    }
  
    print(formular)
    print(model_results)
    return(importance)
    #return(model_results)
  
  }
    

  
}


