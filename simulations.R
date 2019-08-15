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


## Simulation 1:

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


obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim1 <- calculate_PFI(data = test_sim1, c("X_1","X_2", "X_3"), target = target_sim1, mod = mod_sim1, mid = "mse", local = TRUE, replace.ids = obs.id)


ici_sim1 <- subset(pfi_sim1, features == "X_2")
ici.area_sim1 <- ici_sim1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim1 <- c(which.min(ici.area_sim1$mse), whichmedian(ici.area_sim1$mse) ,which.max(ici.area_sim1$mse))
ici.obs_sim1 <- subset(ici_sim1, row.id %in% ici.area_sim1$row.id[ind_sim1])
ici.obs_sim1_test <- ici.obs_sim1[order(ici.obs_sim1$row.id)]

pi.curve_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = TRUE, grid.points = FALSE ,hline = FALSE) +
  geom_line(data = ici.obs_sim1, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim1, ici.curves_sim1, nrow = 1)



dICI_sim1_dev2_test <- dICI_2(data = pfi_sim1, feature = "X_2", measure = "mse")
dICI_sim1_dev2_plot_test <- dICI_plot(dICI_sim1_dev2_test, feature = "X_2")





## Simulation 2:


n_sim2 <- 1000


X_1 <- rnorm(n_sim2, mean = 0, sd = 1)
X_2 <- rnorm(n_sim2, mean = 1, sd = 4)
X_3 <- rbinom(n_sim2, size = 1, prob = 0.5)
eps_sim2 <- rnorm(n_sim2, sd = 0.5)

X_sim2 <- as.data.frame(cbind(X_1, X_2, X_3))
X_sim2$y <- X_1 + 5*X_2 + ifelse(X_2 > 2, ifelse(X_3 == 0, 5*X_2, 0), 0) + eps_sim2
#X_sim1$y <- X_1 + X_2 + ifelse(X_2 > 1, X_2*X_3, 0) + eps_sim1

target_sim2 <- "y"
mlr_sim2 <- train_mlr_model(target_sim2, X_sim2, training_size = 0.8, n_tree = 100)
test_sim2 <- mlr_sim2[["test"]]
mod_sim2 <- mlr_sim2[["mod"]]

obs.id <- sample(1:nrow(test_sim2), 100)
pfi_sim2 <- calculate_PFI(test_sim2, c("X_1", "X_2", "X_3"), mod_sim2, target = 'y',mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim2)
pfi_sim2$feature.value


## Plot Simulation 2

ici_sim2 <- subset(pfi_sim2, features == "X_2")
ici.area_sim2 <- ici_sim2[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim2 <- c(which.min(ici.area_sim2$mse), whichmedian(ici.area_sim2$mse) ,which.max(ici.area_sim2$mse))
ici.obs_sim2 <- subset(ici_sim2, row.id %in% ici.area_sim2$row.id[ind_sim2])


pi.curve_sim2 <- plotImportance(pfi_sim2, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim2 <- plotImportance(pfi_sim2, feat ="X_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim2, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim2, ici.curves_sim2, nrow = 1)




## Calculate the derivatives of the ICI curves

dICI_sim2_dev2_test <- dICI_2(data = pfi_sim2, feature = "X_2", measure = "mse")


##plot dICI curves
dICI_sim2_plot_test <- dICI_plot(dICI_sim2, feature = "X_2")
dICI_sim2_dev2_plot_test <- dICI_plot(dICI_sim2_dev2_test, feature = "X_2")


## Identify/Explain Interactions

Inter_sim2_rf <- PFI_interaction_identifier_2(pfi = pfi_sim2, data = test_sim2, mid = "mse", features = "X_2", model = "RandomForest", n_tree = 50)

varimp_sim2_plot <- varImpPlot(Inter_sim2_rf$mod.learner.model)

Inter_sim2 <- PFI_interaction_identifier_2(pfi_sim2, test_sim2, features = "X_2", mid = "mse", model = "Decision Tree", tree_depth = 1)


## Plot results from interaction identifier

plot(as.party(Inter_sim2$model_results.X_2))
printcp(Inter_sim1$model_results.X_2)
summary(Inter_sim1$model_results.X_2)





## Calculate Conditional Feature Importance

ici_sim1 <- subset(pfi_sim1, features == "X_2")
use0_sim1 <- pfi_sim1[features == "X_3" & feature.value == 0, unique(replace.id)]
use1_sim1 <- pfi_sim1[features == "X_3" & feature.value == 1, unique(replace.id)]


ici_sim1[["X_3"]] <- as.factor(as.numeric(ici_sim1$row.id %in% use1_sim1))


pi_X_2_0 <- subset(ici_sim1, row.id %in% use0_sim1 & replace.id %in% use0_sim1)
pi_X_2_1 <- subset(ici_sim1, row.id %in% use1_sim1 & replace.id %in% use1_sim1)

by <- c("replace.id", "features", "feature.value", "X_3")
pi_X_2_0 <- pi_X_2_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_X_2_1 <- pi_X_2_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]



cond_plot_aggr_sim1 <- plotImportance(pfi_sim1, feat = "X_2", mid = "mse", hline = FALSE) +
                        geom_line(data = pi_X_2_0, aes(color = X_3)) +
                        geom_point(data = pi_X_2_0, aes(color = X_3)) +
                        geom_line(data = pi_X_2_1, aes(color = X_3)) +
                        geom_point(data = pi_X_2_1, aes(color = X_3)) +
                        theme(legend.position = "bottom", legend.box = "vertical")
                       


### add the condition that X_2 > 0

use00_sim1 <- pfi_sim1[features == "X_2" & feature.value > 2, unique(replace.id)]
use01_sim1 <- pfi_sim1[features == "X_2" & feature.value <= 2, unique(replace.id)]

ici_sim1[["X_2_cond"]] <- as.factor(as.numeric(ici_sim1$row.id %in% use01_sim1))
ici_sim1[["factorC"]] <- with(ici_sim1, interaction(X_3,  X_2_cond))

ici_sim1[["factorC"]] <- as.factor(as.numeric(ici_sim1$factorC) - 1)

## factor 0: X_2 > 2 & X_3 = 0
## factor 1: X_2 <= 2 & X_3 = 1
## factor 2: X_2 <= 2 & X_3 = 0
## factor 3: X_2 

pi_X_2_0_0 <- subset(ici_sim1, row.id %in% use0_sim1 & replace.id %in% use0_sim1 & row.id %in% use00_sim1)
pi_X_2_0_1 <- subset(ici_sim1, row.id %in% use0_sim1 & replace.id %in% use0_sim1 & row.id %in% use01_sim1)
pi_X_2_1_0 <- subset(ici_sim1, row.id %in% use1_sim1 & replace.id %in% use1_sim1 & row.id %in% use00_sim1)
pi_X_2_1_1 <- subset(ici_sim1, row.id %in% use1_sim1 & replace.id %in% use1_sim1 & row.id %in% use01_sim1)


by <- c("replace.id", "features", "feature.value", "factorC")
pi_X_2_0_0 <- pi_X_2_0_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_X_2_0_1 <- pi_X_2_0_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_X_2_1_0 <- pi_X_2_1_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_X_2_1_1 <- pi_X_2_1_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]

cond_plot_dis_sim1 <- plotImportance(pfi_sim1, feat = "X_2", mid = "mse", hline = FALSE) +
                    geom_line(data = pi_X_2_0_0, aes(color = factorC)) +
                    geom_point(data = pi_X_2_0_0, aes(color = factorC)) +
                    geom_line(data = pi_X_2_0_1, aes(color = factorC)) +
                    geom_point(data = pi_X_2_0_1, aes(color = factorC)) +
                    geom_line(data = pi_X_2_1_0, aes(color = factorC)) +
                    geom_point(data = pi_X_2_1_0, aes(color = factorC)) +
                    geom_line(data = pi_X_2_1_1, aes(color = factorC)) +
                    geom_point(data = pi_X_2_1_1, aes(color = factorC)) +
                    theme(legend.position = "bottom", legend.box = "vertical") +
                    scale_fill_manual(values = c("#YIOrRd5", "#YIGnBu4", "#YIOrd6", "#YIGnBu5"))


cond_plot_sim1_arranged <- grid.arrange(cond_plot_aggr_sim1, cond_plot_dis_sim1, nrow = 1)



## Simulation 3: non-linear relationship with interaction effect



n_sim6 <- 1000

T_1 <- rnorm(n_sim6, mean = 0, sd = 1)
T_2 <- rnorm(n_sim6, mean = 1, sd = 4)
T_3 <- rbinom(n_sim6, size = 1, prob = 0.5)
eps_sim6 <- rnorm(n_sim6, mean = 0, sd = 1)
X_sim6 <- as.data.frame(cbind(T_1, T_2, T_3))
X_sim6$y_sim6 <- T_1 + 5*sin(T_2) + T_3 + ifelse(T_2 > 2, ifelse(T_3 == 0, 5*T_2, 0), 0) + eps_sim6
target_sim6 <- "y_sim6"

mlr_sim6 <- train_mlr_model(target_sim6, X_sim6, training_size = 0.8, n_tree = 100)
test_sim6 <- mlr_sim6[["test"]]
train_sim6 <- mlr_sim6[["train"]]
mod_sim6 <- mlr_sim6[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim6 <- calculate_PFI(test_sim6, c("T_1", "T_2", "T_3"), mod_sim6, target = target_sim6 ,mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim6)
pfi_sim6$feature.value


## Plot PI and ICI from simulation 5


ici_sim6 <- subset(pfi_sim6, features == "T_2")
ici.area_sim6 <- ici_sim6[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim6 <- c(which.min(ici.area_sim6$mse), whichmedian(ici.area_sim6$mse) ,which.max(ici.area_sim6$mse))
ici.obs_sim6 <- subset(ici_sim6, row.id %in% ici.area_sim6$row.id[ind_sim6])


pi.curve_sim6 <- plotImportance(pfi_sim6, feat ="T_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim6 <- plotImportance(pfi_sim6, feat ="T_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim6, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim6, ici.curves_sim6, nrow = 1)



## plot d-ICI curves


dICI_sim6_dev2 <- dICI_2(data = pfi_sim6, feature = "T_2", measure = "mse")
dICI_sim6_dev2_plot <- dICI_plot(dICI_sim6_dev2, feature = "T_2")



sin_test_list <- c(seq(-12, 12, 0.5))

sin_test_values <- unlist(lapply(sin_test_list, function(x) sin(x)))


sin_plot_data <- cbind(sin_test_list, sin_test_values)
sin_plot_data <- as.data.frame(sin_plot_data)

ggplot(sin_plot_data, aes(x = sin_test_list, y = sin_test_values)) +
  geom_point() + 
  geom_point(data = sin_plot_data, aes(sin_test_list = 5.0), color = "blue") +
  geom_line() + 
  geom_vline(xintercept = 5, linetype = "dotted", color = "blue") + 
  geom_vline(xintercept = 11.02, linetype = "dotted", color = "red")
 

