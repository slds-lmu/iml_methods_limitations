
#### Feature Importance and derivative Individual Conditional Expectation Plots (dICI)
#### Question 1: Is this method/concept even applicable to ICI ?
#### Question 2: If so, what is the optimal simulation method to illustrate the concept: Study "Visualizing Feature Effects..."
#### Steps:
#### 1) calculate for each row.id the 
#### 2) 
#### 3)
#### 4)
#### 5) 




## install packages
install.packages("tidyverse")

## Load packages

library(MASS)
library(tidyverse)
library(mlr)
library(featureImportance)

source("dICE.R")

## Load Boston Housing Data
require(MASS)
data(BostonHousing, package = "mlbench")
str(BostonHousing)

## Specify and train ML algorithm 
#### Random Forest model

boston.task = makeRegrTask(data = BostonHousing, target = "medv")

# Specify the machine learning algorithm with the mlr package
lrn = makeLearner("regr.randomForest", ntree = 100)

# Create indices for train and test data
n = getTaskSize(boston.task)
train.ind = sample(n, size = 0.6*n)
test.ind = setdiff(1:n, train.ind)

## Create test data using test indices
test = getTaskData(boston.task, subset = test.ind)

## Fit model on train data using train indices
mod = train(lrn, boston.task, subset = train.ind)


## Use feature values of 20 randomly chosen observations from test data to plot the importance curves
obs.id = sample(1:nrow(test), 1000)

## Measure feature importance on test data
imp = featureImportance(mod, data = test, replace.ids = obs.id, local = TRUE)
summary(imp)

dataBoston_dICI <- imp$importance
dataBoston_dICI <- as_tibble(dataBoston_dICI)
dataBoston_dICI %>% arrange(features, row.id)

dataBoston_dICI_age <- filter(dataBoston_dICI, features %in% "age") %>% arrange(features, row.id)
dataBoston_dICI_age <- mutate(dataBoston_dICI_age, delta_f = finite.differences(dataBoston_dICI_age))








## Derive Feature Importance on local scale 



#### Transform data.frame to tibble
data_dICI <- imp$importance
data_dICI <- as_tibble(data_dICI)
data_dICI %>% arrange(features, row.id)


#### Subset data and order by feature and row.id 
data_dICI_X_1 <- filter(data_dICI, features %in% "X_1") %>% arrange(features, row.id)
data_dICI_X_2 <- filter(data_dICI, features %in% "X_2") %>% arrange(features, row.id)
data_dICI_X_3 <- filter(data_dICI, features %in% "X_3") %>% arrange(features, row.id)



#######




#######

dICI <- function(data = pfi, feature, measure){
  
  ## Include assertions
  ## data must be of type dataframe, feature must be list() of characters, measure (i.e. "mse")
  
  ## Produces n list objects (n = number of features) 
  data_dICI <- as_tibble(data)
  data_dICI %>% map(feature, filter(data_dICI, data_dICI$features %in% feature)
                      %>% arrange(features, row.id)
                      %>% mutate(finite.differences(data_dICI))
                      %>% paste0(data_dICI, "_", feature)
                      )
  
  return(data_dICI)
  
}


#### Formular/Function for numerical approximation of derivative 

finite.differences <- function(data, measure){
  
  
  if(length(data$feature.value) != length(data$measure)){
    stop('x and y must be equal length')
  }
  
  
  n_all <- length(data$feature.value)
  n_row_unique <- length(unique(data$row.id))

  list_fdx <- list()
  length(list_fdx) <-  n_row_unique
  
  for(row in unique(data$row.id)){
    data_by_row.id <- filter(data, row.id %in% row)
    feature <- data_by_row.id[, "feature.value"]
    target <- data_by_row.id[, "mse"]
    n <- nrow(feature)
    fdx <- vector(length = n)
    for(i in 2:n){
      fdx[i-1] <- (target[i-1,] - target[i,])/(feature[i-1,] - feature[i,])
    }
    # For the last value, since we are unable to perform the forward differencing method 
    # as only the first n values are known, we use the backward differencing approach
    # instead. Note this will essentially give the same value as the last iteration 
    # in the forward differencing method, but it is used as an approximation as we 
    # don't have any more information
    fdx[n] <- (target[n,] - target[n - 1,]) / (feature[n,] - feature[n - 1,])
  
  list_fdx[[row]] <- fdx
      
  }

  print(list_fdx)
  fdx_all <- unlist(list_fdx)
  print(fdx_all)
  fdx_final <- unlist(fdx_all)
  print(fdx_final)
  return(fdx_final)
}

###### test finite.differences function

unique(dataBoston_dICI_age$row.id)
data_by_row.id_test <- filter(dataBoston_dICI_age, row.id %in% 1)

feature_test2 <- data_by_row.id_test[, "feature.value"]

data_by_row.id <- filter(dataBoston_dICI_age, row.id %in% 1)

#### testlines for finite.differences
data_dICI_X_1 <- mutate(data_dICI_X_1 ,delta_f = finite.differences(data_dICI_X_1))
data_dICI_X_2 <- mutate(data_dICI_X_2 ,delta_f = finite.differences(data_dICI_X_2))
dataBoston_dICI_age <- mutate(dataBoston_dICI_age, delta_f = finite.differences(dataBoston_dICI_age))


##########




## input: list of tibble data 
dICI_plot <- function(data, feature){
  ## Assertions here
  
  ## number of features => determines number of rows for grid.arrange
  
  ## ggplot per feature again with lapply ?
  pp1 <- ggplot(data, aes(feature.value, delta_f)) +
    geom_line(group = row.id)
  pp2 <- ggplot() +
    geom_line()
  
  ## the amount of rows and columns should be specified based on the number of features
  grid.arrange(pp1, pp2, nrow = 1) ## amount of rows to be specified!!
  
}


p_X_1 <- ggplot(data_dICI_X_1, aes(feature.value, delta_f)) +
  geom_line(aes(group = row.id))

p_X_2 <- ggplot(data_dICI_X_2, aes(feature.value, delta_f)) +
  geom_line(aes(group = row.id))

grid.arrange(p_X_1, p_X_2, nrow = 1)

## Plot BostonHousing data

pp_dataBoston_dICI_age <- ggplot(dataBoston_dICI_age, aes(feature.value, delta_f)) +
  geom_line(aes(group = row.id))

pp_dataBoston_dICI_age





######### Code snippets

finite.differences_raw <- function(x, y) {
  if (length(x) != length(y)) {
    stop('x and y vectors must have equal length')
  }
  
  n <- length(x)
  
  # Initialize a vector of length n to enter the derivative approximations
  fdx <- vector(length = n)
  
  # Iterate through the values using the forward differencing method
  for (i in 2:n) {
    fdx[i-1] <- (y[i-1] - y[i]) / (x[i-1] - x[i])
  }
  
  # For the last value, since we are unable to perform the forward differencing method 
  # as only the first n values are known, we use the backward differencing approach
  # instead. Note this will essentially give the same value as the last iteration 
  # in the forward differencing method, but it is used as an approximation as we 
  # don't have any more information
  fdx[n] <- (y[n] - y[n - 1]) / (x[n] - x[n - 1])
  
  return(fdx)
}




