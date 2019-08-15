## Load packages

library(MASS)
library(tidyverse)
library(mlr)
library(featureImportance)
library(sfsmisc)

## derivative-ICI function:
## calculates the approximated derivatives for each ICI curve,
## For that, it relies on the function finite.differences()
## finite.differences() is an algorithm which calculates the approx. derivatives of function values through linear interpolation
## Input: 
## data: Permutation Feature Importance derived from calculate_PFI()
## feat: specify the feature for which the d-ICI shall be calculated
## measure: default "mse", 
## Output: 
## returns a dataset which merged the respective derivatives for each observations to the data

dICI <- function(data = pfi, feat, measure){
  
  data_dICI <- as_tibble(data)
  data_dICI %>% lapply(feat, filter(data_dICI, features %in% feat)
                      %>% arrange(features, row.id)
                      %>% mutate(finite.differences(data_dICI))
                      %>% paste0(data_dICI, "_", feat)
                      )
  return(data_dICI)
  
}


## Function that calculates the numerical approx. derivatives of function values
## Input:
## Data: pfi data sorted by the input values of the feature for which the derivatives shall be approximated
## Ouput:
## fdx_final: returns the approximated derivatives for each observation

finite.differences <- function(data){
  
  if(length(data$feature.value) != length(data$mse)){
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


## dICI_2 function is equivalent to dICI(), however it relies on an different algorithm for calculating the derivatives
## For that, it relies on the function finite.differences_2()
## finite.differences_2() is an algorithm which calculates the approx. first order derivatives via the function D1ss() 
## D1ss: uses cubic smoothing splines to to estimate first order derivatives
## Input:
## data: Permutation Feature Importance derived from calculate_PFI()
## feat: specify the feature for which the d-ICI shall be calculated
## measure: default "mse",
## Output:
## returns a dataset which merged the respective derivatives for each observations to the data

dICI_2 <- function(data = pfi, feature, measure){
  
  data_dICI_test <- as_tibble(data)
  data_dICI_test_filtered <- filter(data_dICI_test ,features %in% feature)
  data_dICI_test_arranged <- arrange(data_dICI_test_filtered, features, row.id, feature.value)
  data_dICI_test_mutated <- mutate(data_dICI_test_arranged, delta_f = finite.differences_2(data_dICI_test_arranged))
  
  return(data_dICI_test_mutated)
  
}


## Function that calculates the numerical approx. derivatives of function values
## Input:
## Data: pfi data sorted by the input values of the feature for which the derivatives shall be approximated
## Ouput:
## fdx_final: returns the approximated derivatives for each observation

finite.differences_2 <- function(data, measure){
  
  if(length(data$feature.value) != length(data$mse)){
    stop('x and y must be equal length')
  }
  
  n_all <- length(data$feature.value)
  n_row_unique <- length(unique(data$row.id))
  
  list_fdx <- list()
  length(list_fdx) <-  n_row_unique
  for(row in unique(data$row.id)){
    data_by_row.id <- filter(data, row.id %in% row)
    feature <- data_by_row.id$feature.value
    target <- data_by_row.id$mse
    n <- nrow(feature)
    list_fdx[[row]] <- D1ss(feature, target)
    
  }
  
  fdx_final <- unlist(list_fdx)
  return(fdx_final)
  
}


## dICI_plot function: visualizes the approximated derivates for the respective feature
## Input: 
## data: data, containing the approx. derivatives for each observations, derived through dICI() or dICI_2()
## feature: specify the feature for which the derivatives should be visualized
## Output:
## derivative plots

dICI_plot <- function(data, feature){

  pp1 <- ggplot(data, aes(feature.value, delta_f)) +
    geom_line(group = data$row.id) + 
    ggtitle(paste("Approximated Numerical Derivative of", feature))
  
  return(pp1)
  
}




