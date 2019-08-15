### Real Data Application 
### Boston Housing Data

## load libraries
library(MASS)
library(tidyverse)
library(mlr)
library(featureImportance)
library(gridExtra)

## set seed
set.seed(1234568919)

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


## Use feature values of 150 randomly chosen observations from test data to plot the importance curves
obs.id = sample(1:nrow(test), 150)

BH_features <- as.vector(names(BostonHousing))
BH_predictors <- BH_features[1: (length(BH_features)-1)]


## Calculate the Permutation Feature Importance for the predictors
pfi_BH <- calculate_PFI(data = test, features = BH_predictors, target = "medv", mod = mod, mid ="mse", local = TRUE, replace.ids = obs.id)


## plot Partial Importance and Individual Conditional Importance for lstat
pi.curve_BH_lstat <- plotImportance(pfi_BH, feat ="lstat", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_lstat <- plotImportance(pfi_BH, feat ="lstat", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_lstat, ici.curves_BH_lstat, nrow = 1)




## calculate the approximated derivatives of the ICI curves for feature lstat
d_ICI_BH_lstat <- dICI_2(data = pfi_BH, feature = "lstat", measure = "mse")

d_ICI_BH_lstat_cropped <- filter(d_ICI_BH_lstat, feature.value > 3)


## plot the approx. derivatives of the ICI curves for feature lstat
d_ICI_BH_lstat_plot <- dICI_plot(data = d_ICI_BH_lstat_cropped, feature = "lstat")



## Interaction Detection 
interaction_BH <- PFI_interaction_identifier_2(pfi = pfi_BH, data = test, mid = "mse", features = c("lstat", "rm", "age"), model = "Decision Tree", tree_depth = 1)


## Plot interaction detection

inter_BH_lstat_model <- interaction_BH$model_results.lstat
plot(as.party(inter_BH_lstat_model))


### Conditionl Feature Importance for "lstat" and "dis"

ici_BH_lstat <- subset(pfi_BH, features == "lstat")
use0_BH_lstat <- pfi_BH[features == "dis" & feature.value >= 2.165, unique(replace.id)]
use1_BH_lstat <- pfi_BH[features == "dis" & feature.value < 2.165, unique(replace.id)]


ici_BH_lstat[["dis"]] <- as.factor(as.numeric(ici_BH_lstat$row.id %in% use1_BH_lstat))


pi_BH_lstat_0 <- subset(ici_BH_lstat, row.id %in% use0_BH_lstat & replace.id %in% use0_BH_lstat)
pi_BH_lstat_1 <- subset(ici_BH_lstat, row.id %in% use1_BH_lstat & replace.id %in% use1_BH_lstat)

by <- c("replace.id", "features", "feature.value", "dis")
pi_BH_lstat_0 <- pi_BH_lstat_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_BH_lstat_1 <- pi_BH_lstat_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]


## Plot Conditional Feature Importance for lstat conditioned on dis
cond_plot_aggr_BH_lstat <- plotImportance(pfi_BH, feat = "lstat", mid = "mse", hline = FALSE) +
  geom_line(data = pi_BH_lstat_0, aes(color = dis)) +
  geom_point(data = pi_BH_lstat_0, aes(color = dis)) +
  geom_line(data = pi_BH_lstat_1, aes(color = dis)) +
  geom_point(data = pi_BH_lstat_1, aes(color = dis)) +
  theme(legend.position = "bottom", legend.box = "vertical")

