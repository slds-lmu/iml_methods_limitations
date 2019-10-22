required_packages <- c("devtools", 
                       "mlr", 
                       "ggplot2",
                       "reshape2",
                       "gridExtra",
                       "dplyr",
                       "readr",
                       "randomForest",
                       "earth", 
                       "MASS",
                       "foreach",
                       "doRNG",
                       "doParallel",
                       "grDevices")

install_these <- 
  required_packages[!(required_packages %in% installed.packages())]
install.packages(install_these)
if ("lime" %in% installed.packages()) {
  warning(paste("You already have the lime installed.", 
                "Make sure to use a current github version."))
} else {
  library(devtools)
  install_github("https://github.com/thomasp85/lime")
}

library(mlr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(readr)
library(randomForest)
library(earth)
library(MASS)
library(lime)
library(foreach)
library(doRNG)
library(doParallel)
library(grDevices)

explain <- lime::explain
