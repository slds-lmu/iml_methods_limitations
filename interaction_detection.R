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
library(sfsmisc)

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

PFI_interaction_identifier <- function(pfi, data, features, mid = "mse", model = "Decision Tree", tree_depth, n_tree){
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
    
    print(head(data))
    print(formular)
    print(model_results)
   
    
    return(c(model_results = model_results, formular = formular))
   
  }
  
  if(model == "RandomForest"){
    
    for(feat in features){
    
    var_index_pfi <- which(unique(pfi$features) == feat) 
    var_predict <- as.vector(unique(pfi$features)[-var_index_pfi])
    var_imp <- paste0("imp", feat)ml
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
  
  print(head(data))
  print(mod)
  print(task.pred)
  print(performance)
  
  return(c(mod = mod, task.pred = task.pred, performance = performance))  
  
  }

  
}


#### Interaction Identifier with randomForest package instead of mlr package

PFI_interaction_identifier_2 <- function(pfi, data, features, mid = "mse", model = c("Decision Tree", "RandomForest"), tree_depth, n_tree){
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
    
    for(feat in features){
      var_imp <- paste0("imp", feat)
      var_index_pfi <- which(unique(pfi$features) == feat)
      var_predict <- paste(unique(pfi$features)[-var_index_pfi], collapse = " + ")
      
      
      formular[[feat]] <- paste(var_imp, " ~ ", var_predict)
      model_results[[feat]] <- randomForest(formular[[feat]], data = data, n_tree = n_tree, importance = TRUE)
      importance[[feat]] <- importance(model_results[[feat]])
      
    }
  
  print(var_predict)
  print(model_results)
  print(importance)
  return(model_results)
  
  }
    

  
}





### test code snippets






#### test line

PFI_interaction_identifier(pfi, data = test, features = c("X_1", "X_2"), model = "Decision Tree", tree_depth = 1)
PFI_interaction_identifier(pfi, data = test, features = c("X_1", "X_2"), model = "RandomForest", n_tree = 10)


## plot results from PFI_interaction_identifier
plot_interaction_detect <- function(data, feature, method){
  
  if(method = "Decision Tree"){
    
    interaction_plot <- plot(as.party(data$model_results.feature))
    
    
  }
  
  
  
}
  
  
  plot(as.party(rp))





###############################################################################################################
## Calculate conditional PFI
  
## code snippets
  
## Why does the obs.id split from 20 to 13 and 7 ?
  
  
use0 <- pfi[features == "X_3" & feature.value == 0, unique(replace.id)]
length(use0) ## 13
  
use1 <- pfi[features == "X_3" & feature.value == 1, unique(replace.id)]
length(use1) ## 7 

  
use_test0 <- pfi[features == "X_3" & feature.value == 0,]
use_test1 <- pfi[features == "X_3" & feature.value == 1,]
  
  
iciX_1 <- subset(pfi, features == "X_1")
iciX_1[["X_3"]] <- as.factor(as.numeric(ici$row.id %in% use1))
pi_X_1_0 <- subset(iciX_1, row.id %in% use0 & replace.id %in% use0)
pi_X_1_1 <- subset(iciX_1, row.id %in% use1 & replace.id %in% use1)
  
iciX_2 <- subset(pfi, features == "X_2")
iciX_2[["X_3"]] <- as.factor(as.numeric(ici$row.id %in% use1))
pi_X_2_0 <- subset(iciX_1, row.id %in% use0 & replace.id %in% use0)
pi_X_2_1 <- subset(iciX_1, row.id %in% use1 & replace.id %in% use1)
  

## only works for binary thresholds per group.var
conditional_PFI <- function(data, feature, group.var = list(), threshold = list()){

## assert input
if(length(group.var) != length(threshold)) stop("Number of group.var must be equal to number of thresholds")
  
## subset data = pfi by feature
  
ici_data <- subset(data, features == feature)
print(ici_data)

   
### index row.ids by values of interactions
n_group_var <- length(group.var)
list_of_indeces <- list(length(group.var))


### define index names: e.g.: with two groupvars one should yield 4 index variables

index_names <- vector("list", 2*length(group.var))
  

for(entry in 1:length(group.var)){
  
  index_names[floor(entry*1.5)] <- paste0("index", feature, "_", entry, "_", 1)
  index_names[2*entry] <- paste0("index", feature, "_", entry, "_", 2)
} 

print(index_names)

### assign index_names to actual indices. 

for(index in 1:length(index_names)){
  if(index %% 2 == 1){
    index_names[[index]] <- data[features == group.var[[ceiling(index*0.5)]] & feature.value < threshold[[ceiling(index*0.5)]], unique(replace.id)]
    
  } else{
    index_names[[index]] <- data[features == group.var[[ceiling(index*0.5)]] & feature.value >= threshold[[ceiling(index*0.5)]], unique(replace.id)]
  }
}

print(index_names)
print(length(index_names))

### calculate the conditional PFIs based on indeces from index_names 
### 1) define conditional_PFI names

cond_PFI <- vector("list", 2*length(group.var))

for(condition in 1:length(group.var)){
  
  cond_PFI[floor(condition*1.5)] <- paste0("pi", feature, "_", condition, "_", 1)
  cond_PFI[2*condition] <- paste0("pi", feature, "_", condition, "_", 2)
  
}

print(cond_PFI)
print(length(cond_PFI))
## 2) assign the indicated pfis to the conditional_PFI names


for(cond_name in 1:length(cond_PFI)){
  
  cond_PFI[[cond_name]] <- subset(ici_data, row.id %in% index_names[[cond_name]] & replace.id %in% index_names[[cond_name]])
}

print(cond_PFI)
return(cond_PFI)

}

## test line 
group.var_test <- list("X_1", "X_3")
index_name_test <- vector("list", 2*length(group.var_test))

for(entry in 1:length(group.var_test)){
  
  index_name_test[floor(entry*1.5)] <- paste0("index", "X_2", "_", entry, "_", 1)
  index_name_test[2*entry] <- paste0("index", "X_2", "_", entry, "_", 2)
} 



### test lines function

pfi_cond_sim1 <- conditional_PFI(pfi_sim1, feature = "X_2", group.var = list("X_3", "X_2"), threshold = list(1, 2))





treshold_test <- list(X_1 = 1, X_2 = 2)
names(treshold_test[1])
treshold_test[[1]]

group.var_test <- list("X_1", "X_3")
length(group.var_test)


name_test <- paste0("index", 1, "_", 1)

##### test lines

ici_sim1_test <- subset(pfi_sim1, features == "X_2")
use0_test <- pfi_sim1[features == "X_3" & feature.value == 0, unique(replace.id)]
use1_test <- pfi_sim1[features == "X_3" & feature.value == 1, unique(replace.id)]
pi_sim_x_2_0 <- subset(ici_sim1_test, row.id %in% use0_test & replace.id %in% use0_test)

ici_sim1_test[["X_3"]] <- as.factor(as.numeric(ici_sim1_test$row.id %in% use1_test))


iciX_2[["X_3"]] <- as.factor(as.numeric(ici$row.id %in% use1))
pi_X_2_0 <- subset(iciX_1, row.id %in% use0 & replace.id %in% use0)
pi_X_2_1 <- subset(iciX_1, row.id %in% use1 & replace.id %in% use1)


