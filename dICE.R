
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
library(sfsmisc)
source("dICE.R")


## derivative-ICI function:
## calculates the approximated derivatives for each ICI curve,
## For that, it relies on the function finite.differences()
## finite.differences() is an algorithm which calculates the approx. derivatives of function values through interpolation
## Input: 
## data: Permutation Feature Importance derived from calculate_PFI()
## feat: specify the feature for which the d-ICI shall be calculated
## measure: default "mse", 
## Output: 
## returns a dataset which merges a column with the respective derivatives to the data

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
## fdx_final: 

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






## input: list of tibble data 
dICI_plot <- function(data, feature){
  ## Assertions here
  
  ## number of features => determines number of rows for grid.arrange
  
  ## ggplot per feature again with lapply ?
  pp1 <- ggplot(data, aes(feature.value, delta_f)) +
            geom_line(group = data$row.id) + 
            ggtitle(paste("Approximated Numerical Derivative of", feature))
  return(pp1)
  
  ## the amount of rows and columns should be specified based on the number of features
  ##grid.arrange(pp1, pp2, nrow = 1) ## amount of rows to be specified!!
  
}





## dICI_2 function is equivalent to dICI)(), however it relies on an different algorithm for calculating the derivatives
## Input:
## data: Permutation Feature Importance derived from calculate_PFI()
## feat: specify the feature for which the d-ICI shall be calculated
## measure: default "mse",
## Output: 


dICI_2 <- function(data = pfi, feature, measure){
  
  ## Include assertions
  ## data must be of type dataframe, feature must be list() of characters, measure (i.e. "mse")
  data_dICI_test <- as_tibble(data)
  data_dICI_test_filtered <- filter(data_dICI_test ,features %in% feature)
  data_dICI_test_arranged <- arrange(data_dICI_test_filtered, features, row.id, feature.value)
  data_dICI_test_mutated <- mutate(data_dICI_test_arranged, delta_f = finite.differences_2(data_dICI_test_arranged))
  
  return(data_dICI_test_mutated)
  
}


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



## Assertions here

## number of features => determines number of rows for grid.arrange

## ggplot per feature again with lapply ?

## the amount of rows and columns should be specified based on the number of features
##grid.arrange(pp1, pp2, nrow = 1) ## amount of rows to be specified!!

## input: list of tibble data 
dICI_plot <- function(data, feature){
 
  pp1 <- ggplot(data, aes(feature.value, delta_f)) +
    geom_line(group = data$row.id) + 
    ggtitle(paste("Approximated Numerical Derivative of", feature))
  return(pp1)

}

