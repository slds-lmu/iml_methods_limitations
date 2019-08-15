## Simulation Data
library(mlr)
library(MASS)
library(tidyverse)
library(dplyr)
library(featureImportance)
library(randomForest)
library(RColorBrewer)
library(sfsmisc)
library(ggplot2)
library(grid)
library(gridExtra)
library(rpart)

## set seed
set.seed(12345)


## Simulations
## Simulation 1: analyze heterogeneity in Feature Importance without interaction effect and with linear relationship
## Simulation 2: analyze heterogeneity in Feature Importance with interaction effect and with linear relationship
## Simulation 3: analyze heterogeneity in Feature Importance with interaction effect and non-linear relationship


## Simulation 1: analyze heterogeneity in Feature Importance without interaction effect and with linear relationship

## define number of observations to be sampled
n_sim1 <- 1000

## define the distributional properties of each feature
## X_1: normally distributed with mu = 0 and sigma = 1
## X_2: normally distributed with mu = 0 and sigma = 1
## X_3: binomially distributed with prob = 0.5
## epsilon: normally distributed with mu = 0 and sigma = 1

X_1 <- rnorm(n_sim1, mean = 0, sd = 1)
X_2 <- rnorm(n_sim1, mean = 0, sd = 1)
X_3 <- rbinom(n_sim1, size = 1, prob = 0.5)
eps_sim1 <- rnorm(n_sim1, mean = 0, sd = 1)

## define functional relationship between the features (X_1, X_2 and X_3) and y (response)
X_sim1 <- as.data.frame(cbind(X_1, X_2, X_3))
X_sim1$y_sim1 <- 5*X_1 + 5*X_2 + X_3 + eps_sim1
target_sim1 <- "y_sim1"

## train a random Forest on the training data
mlr_sim1 <- train_mlr_model(target_sim1, X_sim1, training_size = 0.8, n_tree = 100)
test_sim1 <- mlr_sim1[["test"]]
train_sim1 <- mlr_sim1[["train"]]
mod_sim1 <- mlr_sim1[["mod"]]

## calcuathe PFI for feature X_1, X_2 and X_3
obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim1 <- calculate_PFI(data = test_sim1, c("X_1","X_2", "X_3"), target = target_sim1, mod = mod_sim1, mid = "mse", local = TRUE, replace.ids = obs.id)


## Plot Simulation 1

ici_sim1 <- subset(pfi_sim1, features == "X_2")
ici.area_sim1 <- ici_sim1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim1 <- c(which.min(ici.area_sim1$mse), whichmedian(ici.area_sim1$mse) ,which.max(ici.area_sim1$mse))
ici.obs_sim1 <- subset(ici_sim1, row.id %in% ici.area_sim1$row.id[ind_sim1])
ici.obs_sim1_test <- ici.obs_sim1[order(ici.obs_sim1$row.id)]

## plot the PI curve and the ICI curves for feature X_2, respectively
pi.curve_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = TRUE, grid.points = FALSE ,hline = FALSE) +
  geom_line(data = ici.obs_sim1, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim1, ici.curves_sim1, nrow = 1)


## calculate the approximated derivatives of the ICI curves for feature X_2
dICI_sim1_dev2_test <- dICI_2(data = pfi_sim1, feature = "X_2", measure = "mse")

## plot the approx. derivatives of the ICI curves for feature X_2
dICI_sim1_dev2_plot_test <- dICI_plot(dICI_sim1_dev2_test, feature = "X_2")





## Simulation 2: analyze heterogeneity in Feature Importance with interaction effect and with linear relationship

n_sim2 <- 1000

## define the distributional properties of each feature
## X_1: normally distributed with mu = 0 and sigma = 1
## X_2: normally distributed with mu = 1 and sigma = 4
## X_3: binomially distributed with prob = 0.5
## epsilon: normally distributed with mu = 0 and sigma = 1

X_1 <- rnorm(n_sim2, mean = 0, sd = 1)
X_2 <- rnorm(n_sim2, mean = 1, sd = 4)
X_3 <- rbinom(n_sim2, size = 1, prob = 0.5)
eps_sim2 <- rnorm(n_sim2, sd = 0.5)

## define functional relationship between the features (X_1, X_2 and X_3) and y (response)
X_sim2 <- as.data.frame(cbind(X_1, X_2, X_3))
X_sim2$y <- X_1 + 5*X_2 + ifelse(X_2 > 2, ifelse(X_3 == 0, 5*X_2, 0), 0) + eps_sim2



target_sim2 <- "y"
mlr_sim2 <- train_mlr_model(target_sim2, X_sim2, training_size = 0.8, n_tree = 100)
test_sim2 <- mlr_sim2[["test"]]
mod_sim2 <- mlr_sim2[["mod"]]

obs.id <- sample(1:nrow(test_sim2), 100)
pfi_sim2 <- calculate_PFI(test_sim2, c("X_1", "X_2", "X_3"), mod_sim2, target = 'y',mid = "mse", local = TRUE, replace.ids = obs.id)



## Plot Simulation 2

ici_sim2 <- subset(pfi_sim2, features == "X_2")
ici.area_sim2 <- ici_sim2[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim2 <- c(which.min(ici.area_sim2$mse), whichmedian(ici.area_sim2$mse) ,which.max(ici.area_sim2$mse))
ici.obs_sim2 <- subset(ici_sim2, row.id %in% ici.area_sim2$row.id[ind_sim2])

## plot the PI curve and the ICI curves for feature X_2, respectively
pi.curve_sim2 <- plotImportance(pfi_sim2, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim2 <- plotImportance(pfi_sim2, feat ="X_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim2, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim2, ici.curves_sim2, nrow = 1)




## calculate the approximated derivatives of the ICI curves for feature X_2
dICI_sim2_dev2_test <- dICI_2(data = pfi_sim2, feature = "X_2", measure = "mse")

## plot the approx. derivatives of the ICI curves for feature X_2
dICI_sim2_dev2_plot_test <- dICI_plot(dICI_sim2_dev2_test, feature = "X_2")


## Identify/Explain Interactions between X_2 and X_3
Inter_sim2 <- PFI_interaction_identifier_2(pfi_sim2, test_sim2, features = "X_2", mid = "mse", model = "Decision Tree", tree_depth = 1)

## Plot results from interaction identifier
plot(as.party(Inter_sim2$model_results.X_2))





## Based on the results from the PFI_interaction_identifier_2(), calculate and visualize now the Conditional Feature Importance
## Calculate Conditional Feature Importance

ici_sim2 <- subset(pfi_sim2, features == "X_2")
use0_sim2 <- pfi_sim2[features == "X_3" & feature.value == 0, unique(replace.id)]
use1_sim2 <- pfi_sim2[features == "X_3" & feature.value == 1, unique(replace.id)]


ici_sim2[["X_3"]] <- as.factor(as.numeric(ici_sim2$row.id %in% use1_sim2))


pi_X_2_0 <- subset(ici_sim2, row.id %in% use0_sim2 & replace.id %in% use0_sim2)
pi_X_2_1 <- subset(ici_sim2, row.id %in% use1_sim2 & replace.id %in% use1_sim2)

by <- c("replace.id", "features", "feature.value", "X_3")
pi_X_2_0 <- pi_X_2_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_X_2_1 <- pi_X_2_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]


## Plot the conditional Feature Importance
cond_plot_aggr_sim2 <- plotImportance(pfi_sim2, feat = "X_2", mid = "mse", hline = FALSE) +
                        geom_line(data = pi_X_2_0, aes(color = X_3)) +
                        geom_point(data = pi_X_2_0, aes(color = X_3)) +
                        geom_line(data = pi_X_2_1, aes(color = X_3)) +
                        geom_point(data = pi_X_2_1, aes(color = X_3)) +
                        theme(legend.position = "bottom", legend.box = "vertical")
                       



## Simulation 3: non-linear relationship with interaction effect

n_sim3 <- 1000

X_1 <- rnorm(n_sim3, mean = 0, sd = 1)
X_2 <- rnorm(n_sim3, mean = 1, sd = 4)
X_3 <- rbinom(n_sim3, size = 1, prob = 0.5)
eps_sim3 <- rnorm(n_sim3, mean = 0, sd = 1)
X_sim3 <- as.data.frame(cbind(X_1, X_2, X_3))
X_sim3$y_sim3 <- X_1 + 5*sin(X_2) + X_3 + ifelse(X_2 > 2, ifelse(X_3 == 0, 5*X_2, 0), 0) + eps_sim3
target_sim3 <- "y_sim3"

mlr_sim3 <- train_mlr_model(target_sim3, X_sim3, training_size = 0.8, n_tree = 100)
test_sim3 <- mlr_sim3[["test"]]
train_sim3 <- mlr_sim3[["train"]]
mod_sim3 <- mlr_sim3[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim3 <- calculate_PFI(test_sim3, c("X_1", "X_2", "X_3"), mod_sim3, target = target_sim3 ,mid = "mse", local = TRUE, replace.ids = obs.id)



## Plots: simulation 3


ici_sim3 <- subset(pfi_sim3, features == "X_2")
ici.area_sim3 <- ici_sim3[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim3 <- c(which.min(ici.area_sim3$mse), whichmedian(ici.area_sim3$mse) ,which.max(ici.area_sim3$mse))
ici.obs_sim3 <- subset(ici_sim3, row.id %in% ici.area_sim3$row.id[ind_sim3])

## plot the PI curve and the ICI curves for feature X_2, respectively
pi.curve_sim3 <- plotImportance(pfi_sim3, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim3 <- plotImportance(pfi_sim3, feat ="X_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim3, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim3, ici.curves_sim3, nrow = 1)





## calculate the approximated derivatives of the ICI curves for feature X_2
dICI_sim3_dev2 <- dICI_2(data = pfi_sim3, feature = "X_2", measure = "mse")

## plot the approx. derivatives of the ICI curves for feature X_2
dICI_sim3_dev2_plot <- dICI_plot(dICI_sim3_dev2, feature = "X_2")




