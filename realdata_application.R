### Real Data Application 
### Boston Housing Data

library(MASS)
library(tidyverse)
library(mlr)
library(featureImportance)

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
obs.id = sample(1:nrow(test), 100)

BH_features <- as.vector(names(BostonHousing))
BH_predictors <- BH_features[1: (length(BH_features)-1)]

pfi_BH <- calculate_PFI(data = test, features = BH_predictors, target = "medv", mod = mod, mid ="mse", local = TRUE, replace.ids = obs.id)


## plot Partial Importance and Individual Conditional Importance

pi.curve_BH_rm <- plotImportance(pfi_BH, feat ="rm", mid = "mse", individual = FALSE, hline = TRUE) +
  expand_limits(c(5,8))
ici.curves_BH_rm <- plotImportance(pfi_BH, feat ="rm", mid = "mse", individual = TRUE, hline = FALSE) +
  expand_limits(c(5,8))
grid.arrange(pi.curve_BH_rm, ici.curves_BH_rm, nrow = 1)

pi.curve_BH_age <- plotImportance(pfi_BH, feat ="age", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_age <- plotImportance(pfi_BH, feat ="age", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_age, ici.curves_BH_age, nrow = 1)


## calculate derivative
d_ICI_BH_lstat <- dICI(data = pfi_BH, feature = "lstat", measure = "mse")
d_ICI_BH_rm <- dICI(data = pfi_BH, feature = "rm", measure = "mse")
d_ICI_BH_age <- dICI(data = pfi_BH, feature = "age", measure = "mse")


## plot dICI

d_ICI_BH_lstat_plot <- dICI_plot(data = d_ICI_BH_lstat, feature = "lstat")
d_ICI_BH_rm_plot <- dICI_plot(data = d_ICI_BH_rm, feature = "rm")
d_ICI_BH_age_plot <- dICI_plot(data = d_ICI_BH_age, feature = "age")


### test for Interaction Detection

ici_BH_test <- subset(pfi_BH, features == "lstat")
ici_BH_test.integral <- ici_BH_test[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]

## Interaction Detection 

interaction_BH <- PFI_interaction_identifier(pfi = pfi_BH, data = test, mid = "mse", features = c("lstat", "rm", "age"), model = "Decision Tree", tree_depth = 1)
interaction_BH_rf <- PFI_interaction_identifier(pfi = pfi_BH, data = test, mid = "mse", features = c("lstat", "rm", "age"), model = "RandomForest", n_tree = 50)

## plot 

inter_BH_age_model <- interaction_BH$model_results["age"]
plot(as.party(inter_BH_age_model))
plot(as.party(interaction_BH$model_results["age"]))

