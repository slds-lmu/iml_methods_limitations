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
# specify data

sig = diag(4)
n = 10000
generateY_sim1 = function(X) {
  eps = rnorm(nrow(X), sd = 0.5)
  form = ~ V1 + V2 + V3 + V4 + V1:V2 + V2:V4 - 1
  #V1 + V2 + ifelse(V3 == 0, I(10*V1), I(10*V2)) - 1
  mat = model.matrix(form, data = X)
  rowSums(mat) + eps
}
X_sim1 = as.data.frame(mvrnorm(n, mu = rep(0, ncol(sig)), Sigma = sig))
#X$V3 = rbinom(n, size = 1, prob = 0.5)
X_sim1$y = generateY(X_sim1)

##
generateY_sim2 <- function(X){
  eps = rnorm(nrow(X), sd = 0.5)
  form = ~ V1 + V2 + ifelse(V3 == 0, I(10*V1), I(10*V2)) - 1
  mat = model.matrix(form, data = X)
  rowSums(mat) + eps
}



######## Simulation 1.1 for subchapter 1:

n_sim1_1 <- 1000
Z_1 <- rnorm(n = n_sim1_1, mean = 0, sd = 1)
Z_2 <- rnorm(n = n_sim1_1, mean = 0, sd = 2)
Z_3 <- rbinom(n = n_sim1_1, size = 1, prob = 0.5)
Z_4 <- rnorm(n = n_sim1_1, mean = 0, sd = 1)
eps_sim1_1 <- rnorm(n_sim1_1, sd = 0.5)

X_sim1_1 <- as.data.frame(cbind(Z_1, Z_2, Z_3, Z_4))
X_sim1_1$y_sim1_1 <- Z_1 + Z_2 + Z_3 + Z_4 + ifelse(Z_3 == 0, 10*Z_2, 10*Z_1) + eps_sim1_1



target_sim1_1 <- "y_sim1_1"

mlr_sim1_1 <- train_mlr_model(target_sim1_1, X_sim1_1, training_size = 0.8, n_tree = 100)
test_sim1_1 <- mlr_sim1_1[["test"]]
mod_sim1_1 <- mlr_sim1_1[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim1_1 <- calculate_PFI(test_sim1_1, c("Z_1", "Z_2", "Z_3", "Z_4"), mod_sim1_1, target = target_sim1_1, mid = "mse", local = TRUE, replace.ids = obs.id)


## Plot Simulation 1_1

pi.curve_sim1_1 <- plotImportance(pfi_sim1_1, feat ="Z_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim1_1 <- plotImportance(pfi_sim1_1, feat ="Z_2", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_sim1_1, ici.curves_sim1_1, nrow = 1)

## Plot dICI curves

dICI_sim1_1_dev2 <- dICI_2(data = pfi_sim1_1, feature = "Z_2", measure = "mse")
dICI_sim1_1_dev2_plot <- dICI_plot(dICI_sim1_1_dev2, feature = "Z_2")


## Explain Interaction Effects

Inter_sim1_1 <- PFI_interaction_identifier(pfi_sim1_1, test_sim1_1, features = "Z_2", mid = "mse", model = "Decision Tree", tree_depth = 2)
Inter_sim1_1_2 <- PFI_interaction_identifier_2(pfi_sim1_1, test_sim1_1, features = "Z_2", mid = "mse", model = "Random Forest", n_tree = 100)


## Plot results from interaction identifier

plot(as.party(Inter_sim1_1$model_results.Z_2))
printcp(Inter_sim1_1$model_results.X_2)
summary(Inter_sim1_1$model_results.X_2)


########## Simulation 1 for subchapter 1 

n_sim1 <- 1000


X_1 <- rnorm(n_sim1, mean = 0, sd = 1)
X_2 <- rnorm(n_sim1, mean = 1, sd = 4)
X_3 <- rbinom(n_sim1, size = 1, prob = 0.5)
eps_sim1 <- rnorm(n_sim1, sd = 0.5)

X_sim1 <- as.data.frame(cbind(X_1, X_2, X_3))
X_sim1$y <- X_1 + 5*X_2 + ifelse(X_2 > 2, ifelse(X_3 == 0, 5*X_2, 0), 0) + eps_sim1
#X_sim1$y <- X_1 + X_2 + ifelse(X_2 > 1, X_2*X_3, 0) + eps_sim1

target_sim1 <- "y"
mlr_sim1 <- train_mlr_model(target_sim1, X_sim1, training_size = 0.8, n_tree = 100)
test_sim1 <- mlr_sim1[["test"]]
mod_sim1 <- mlr_sim1[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim1 <- calculate_PFI(test_sim1, c("X_1", "X_2", "X_3"), mod_sim1, target = 'y',mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim1)
pfi_sim1$feature.value


## Plot Simulation 1

ici_sim1 <- subset(pfi_sim1, features == "X_2")
ici.area_sim1 <- ici_sim1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim1 <- c(which.min(ici.area_sim1$mse), whichmedian(ici.area_sim1$mse) ,which.max(ici.area_sim1$mse))
ici.obs_sim1 <- subset(ici_sim1, row.id %in% ici.area_sim1$row.id[ind_sim1])


pi.curve_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim1 <- plotImportance(pfi_sim1, feat ="X_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim1, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim1, ici.curves_sim1, nrow = 1)




## Calculate derivative ICI
dICI_sim1 <- dICI(data = pfi_sim1, feature = "X_2", measure = "mse")

dICI_sim_dev2_test <- dICI_2(data = pfi_sim1, feature = "X_2", measure = "mse")


##plot dICI
dICI_sim1_plot_test <- dICI_plot(dICI_sim1, feature = "X_2")
dICI_sim1_dev2_plot_test <- dICI_plot(dICI_sim_dev2_test, feature = "X_2")


## Identify/Explain Interactions

Inter_sim1_rf <- PFI_interaction_identifier(pfi = pfi_sim1, data = test_sim1, mid = "mse", features = "X_2", model = "RandomForest", n_tree = 50)

varimp_sim1_plot <- varImpPlot(Inter_sim1_rf$mod.learner.model)

Inter_sim1 <- PFI_interaction_identifier(pfi_sim1, test_sim1, features = "X_2", mid = "mse", model = "Decision Tree", tree_depth = 1)


## Plot results from interaction identifier

plot(as.party(Inter_sim1$model_results.X_2))
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

############ Simulation 3 for subchapter 2 (formula form Casalicchio et al. 2018)

n_sim3 <- 1000

V_1 <- rnorm(n_sim3, mean = 0, sd = 1)
V_2 <- rnorm(n_sim3, mean = 0, sd = 1)
V_3 <- rbinom(n_sim3, size = 1, prob = 0.5)
eps_sim3 <- rnorm(n_sim3, mean = 0, sd = 1)
X_sim3 <- as.data.frame(cbind(V_1, V_2, V_3))
X_sim3$y_sim3 <- V_1 + V_2 + ifelse(V_3 == 0, 10*V_1, 10*V_2) + eps_sim3
target_sim3 <- "y_sim3"

mlr_sim3 <- train_mlr_model(target_sim3, X_sim3, training_size = 0.8, n_tree = 100)
test_sim3 <- mlr_sim3[["test"]]
train_sim3 <- mlr_sim3[["train"]]
mod_sim3 <- mlr_sim3[["mod"]]

pfi_sim3 <- calculate_PFI(data = test_sim3, c("V_1","V_2", "V_3"), target = target_sim3, mod = mod_sim3, mid = "mse", local = TRUE, replace.ids = obs.id)

## Plot Simulation 3

pi.curve_sim3 <- plotImportance(pfi_sim3, feat ="V_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim3 <- plotImportance(pfi_sim3, feat ="V_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(aes(color = factor("V_3"), group = "V_3"))

grid.arrange(pi.curve_sim3, ici.curves_sim3, nrow = 1)

## Plot Conditional PFI for X_2

## Calculate Conditional Feature Importance

ici_sim3 <- subset(pfi_sim3, features == "V_2")
use0_sim3 <- pfi_sim3[features == "V_3" & feature.value == 0, unique(replace.id)]
use1_sim3 <- pfi_sim3[features == "V_3" & feature.value == 1, unique(replace.id)]


ici_sim3[["V_3"]] <- as.factor(as.numeric(ici_sim3$row.id %in% use1_sim3))


pi_V_2_0_sim3 <- subset(ici_sim3, row.id %in% use0_sim3 & replace.id %in% use0_sim3)
pi_V_2_1_sim3 <- subset(ici_sim3, row.id %in% use1_sim3 & replace.id %in% use1_sim3)

by <- c("replace.id", "features", "feature.value", "V_3")
pi_V_2_0_sim3 <- pi_V_2_0_sim3[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_V_2_1_sim3 <- pi_V_2_1_sim3[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]



cond_plot_aggr_sim3 <- plotImportance(pfi_sim3, feat = "V_2", mid = "mse", hline = FALSE) +
                          geom_line(data = pi_V_2_0_sim3, aes(color = V_3)) +
                          geom_point(data = pi_V_2_0_sim3, aes(color = V_3)) +
                          geom_line(data = pi_V_2_1_sim3, aes(color = V_3)) +
                          geom_point(data = pi_V_2_1_sim3, aes(color = V_3)) +
                          theme(legend.position = "bottom", legend.box = "vertical")


## Simulation 4


n_sim4 <- 1000

W_1 <- rnorm(n_sim4, mean = 0, sd = 1)
W_2 <- rnorm(n_sim4, mean = 0, sd = 1)
W_3 <- rbinom(n_sim4, size = 1, prob = 0.5)
eps_sim4 <- rnorm(n_sim4, mean = 0, sd = 1)
X_sim4 <- as.data.frame(cbind(W_1, W_2, W_3))
X_sim4$y_sim4 <- 5*W_1 + 5*W_2 + W_3 + eps_sim4
target_sim4 <- "y_sim4"

mlr_sim4 <- train_mlr_model(target_sim4, X_sim4, training_size = 0.8, n_tree = 100)
test_sim4 <- mlr_sim4[["test"]]
train_sim4 <- mlr_sim4[["train"]]
mod_sim4 <- mlr_sim4[["mod"]]

pfi_sim4 <- calculate_PFI(data = test_sim4, c("W_1","W_2", "W_3"), target = target_sim4, mod = mod_sim4, mid = "mse", local = TRUE, replace.ids = obs.id)


ici_sim4 <- subset(pfi_sim4, features == "W_2")
ici.area_sim4 <- ici_sim4[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim4 <- c(which.min(ici.area_sim4$mse), whichmedian(ici.area_sim4$mse) ,which.max(ici.area_sim4$mse))
ici.obs_sim4 <- subset(ici_sim4, row.id %in% ici.area_sim4$row.id[ind_sim4])
ici.obs_sim4_test <- ici.obs_sim4[order(ici.obs_sim4$row.id)]

pi.curve_sim4 <- plotImportance(pfi_sim4, feat ="W_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim4 <- plotImportance(pfi_sim4, feat ="W_2", mid = "mse", individual = TRUE, grid.points = FALSE ,hline = FALSE) +
  geom_line(data = ici.obs_sim4, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim4, ici.curves_sim4, nrow = 1)



dICI_sim4_dev2_test <- dICI_2(data = pfi_sim4, feature = "W_2", measure = "mse")
dICI_sim4_dev2_plot_test <- dICI_plot(dICI_sim4_dev2_test, feature = "W_2")


########## Simulation with non-linear relationship and interaction effect


n_sim5 <- 1000

U_1 <- rnorm(n_sim5, mean = 0, sd = 1)
U_2 <- rnorm(n_sim5, mean = 1, sd = 4)
U_3 <- rbinom(n_sim5, size = 1, prob = 0.5)
eps_sim5 <- rnorm(n_sim5, mean = 0, sd = 1)
X_sim5 <- as.data.frame(cbind(U_1, U_2, U_3))
X_sim5$y_sim5 <- U_1 + sin(U_2) + ifelse(U_2 > 3, ifelse(U_3 == 0, U_2, 0), 0) + eps_sim5
target_sim5 <- "y_sim5"

mlr_sim5 <- train_mlr_model(target_sim5, X_sim5, training_size = 0.8, n_tree = 100)
test_sim5 <- mlr_sim5[["test"]]
train_sim5 <- mlr_sim5[["train"]]
mod_sim5 <- mlr_sim5[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim5 <- calculate_PFI(test_sim5, c("U_1", "U_2", "U_3"), mod_sim5, target = target_sim5 ,mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim5)
pfi_sim5$feature.value

test_list <- seq(0, 6, 0.5)
sin_test_list <- sapply(test_list, function(x) sin(x))
rbind(test_list, sin_test_list)
## Plot PI and ICI from simulation 5


ici_sim5 <- subset(pfi_sim5, features == "U_2")
ici.area_sim5 <- ici_sim5[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim5 <- c(which.min(ici.area_sim5$mse), whichmedian(ici.area_sim5$mse) ,which.max(ici.area_sim5$mse))
ici.obs_sim5 <- subset(ici_sim5, row.id %in% ici.area_sim5$row.id[ind_sim5])


pi.curve_sim5 <- plotImportance(pfi_sim5, feat ="U_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim5 <- plotImportance(pfi_sim5, feat ="U_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim5, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim5, ici.curves_sim5, nrow = 1)



## d-ICI plot

dICI_sim5_dev2 <- dICI_2(data = pfi_sim5, feature = "U_2", measure = "mse")
dICI_sim5_dev2_plot <- dICI_plot(dICI_sim5_dev2, feature = "U_2")



## Simulation 5: non-linear relationship without interaction effect



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
 

 ########## Simulation 7 with non-linear relationship (example 2) and interaction effect


n_sim7 <- 1000

S_1 <- rnorm(n_sim7, mean = 0, sd = 1)
S_2 <- rnorm(n_sim7, mean = 1, sd = 4)
S_3 <- rbinom(n_sim7, size = 1, prob = 0.5)
eps_sim7 <- rnorm(n_sim7, mean = 0, sd = 1)
X_sim7 <- as.data.frame(cbind(S_1, S_2, S_3))
X_sim7$y_sim7 <- S_1 + 5*S_2 +(S_2)^2 + ifelse(S_2 > 2, ifelse(S_3 == 0, 5*S_2, 0), 0) + eps_sim7
target_sim7 <- "y_sim7"

mlr_sim7 <- train_mlr_model(target_sim7, X_sim7, training_size = 0.8, n_tree = 100)
test_sim7 <- mlr_sim7[["test"]]
train_sim7 <- mlr_sim7[["train"]]
mod_sim7 <- mlr_sim7[["mod"]]

obs.id <- sample(1:nrow(test_sim1), 100)
pfi_sim7 <- calculate_PFI(test_sim7, c("S_1", "S_2", "S_3"), mod_sim7, target = target_sim7, mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim7)
pfi_sim7$feature.value


## Plot PI and ICI from simulation 5


ici_sim7 <- subset(pfi_sim7, features == "S_2")
ici.area_sim7 <- ici_sim7[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
ind_sim7 <- c(which.min(ici.area_sim7$mse), whichmedian(ici.area_sim7$mse) ,which.max(ici.area_sim7$mse))
ici.obs_sim7 <- subset(ici_sim7, row.id %in% ici.area_sim7$row.id[ind_sim7])


pi.curve_sim7 <- plotImportance(pfi_sim7, feat ="S_2", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_sim7 <- plotImportance(pfi_sim7, feat ="S_2", mid = "mse", individual = TRUE, hline = FALSE) +
  geom_line(data = ici.obs_sim7, aes(color = factor(row.id), group = row.id)) +
  theme(legend.position = "none")
grid.arrange(pi.curve_sim7, ici.curves_sim7, nrow = 1)


dICI_sim7_dev2 <- dICI_2(data = pfi_sim7, feature = "S_2", measure = "mse")
dICI_sim7_dev2_plot <- dICI_plot(dICI_sim7_dev2, feature = "S_2")



## Simulation 8: non-linear relationship ( example 2) without interaction effect



# #n_sim6 <- 1000
# 
# R_1 <- rnorm(n_sim6, mean = 0, sd = 1)
# R_2 <- rnorm(n_sim6, mean = 1, sd = 4)
# R_3 <- rbinom(n_sim6, size = 1, prob = 0.5)
# eps_sim6 <- rnorm(n_sim6, mean = 0, sd = 1)
# X_sim6 <- as.data.frame(cbind(T_1, T_2, T_3))
# X_sim6$y_sim6 <- T_1 + 5*sin(T_2) + T_3 + eps_sim6
# target_sim6 <- "y_sim6"
# 
# mlr_sim6 <- train_mlr_model(target_sim6, X_sim6, training_size = 0.8, n_tree = 100)
# test_sim6 <- mlr_sim6[["test"]]
# train_sim6 <- mlr_sim6[["train"]]
# mod_sim6 <- mlr_sim6[["mod"]]
# 
# obs.id <- sample(1:nrow(test_sim1), 100)

# pfi_sim6 <- calculate_PFI(test_sim6, c("T_1", "T_2", "T_3"), mod_sim6, target = target_sim6 ,mid = "mse", local = TRUE, replace.ids = obs.id)
# summary(pfi_sim6)
# pfi_sim6$feature.value
# 
# 
# ## Plot PI and ICI from simulation 5
# 
# 
# ici_sim6 <- subset(pfi_sim6, features == "T_2")
# ici.area_sim6 <- ici_sim6[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]
# ind_sim6 <- c(which.min(ici.area_sim6$mse), whichmedian(ici.area_sim6$mse) ,which.max(ici.area_sim6$mse))
# ici.obs_sim6 <- subset(ici_sim6, row.id %in% ici.area_sim6$row.id[ind_sim6])
# 
# 
# pi.curve_sim6 <- plotImportance(pfi_sim6, feat ="T_2", mid = "mse", individual = FALSE, hline = TRUE)
# ici.curves_sim6 <- plotImportance(pfi_sim6, feat ="T_2", mid = "mse", individual = TRUE, hline = FALSE) +
#   geom_line(data = ici.obs_sim6, aes(color = factor(row.id), group = row.id)) +
#   theme(legend.position = "none")
# grid.arrange(pi.curve_sim6, ici.curves_sim6, nrow = 1)


## Visualization of Interaction Effects. 

n_sim7 <- 1000

Q_1 <- rnorm(n_sim7, mean = 0, sd = 1)
Q_2 <- rnorm(n_sim7, mean = 1, sd = 4)
Q_3 <- rbinom(n_sim7, size = 1, prob = 0.5)
Q_4 <- rnorm(n_sim7, mean  = 0, sd = 1)
eps_sim7 <- rnorm(n_sim7, mean = 0, sd = 1)
X_sim7 <- as.data.frame(cbind(Q_1, Q_2, Q_3, Q_4))
X_sim7$y_sim7 <- Q_1 + 5*Q_2 + ifelse(Q_3  == 0, 5*Q_2, 0) + Q_2*Q_4 + eps_sim7
target_sim6 <- "y_sim7"

mlr_sim7 <- train_mlr_model(target_sim7, X_sim7, training_size = 0.8, n_tree = 100)
test_sim7 <- mlr_sim7[["test"]]
train_sim7 <- mlr_sim7[["train"]]
mod_sim7 <- mlr_sim7[["mod"]]

obs.id <- sample(1:nrow(test_sim7), 100)
pfi_sim7 <- calculate_PFI(test_sim7, c("Q_1", "Q_2", "Q_3", "Q_4"), mod_sim7, target = target_sim7 ,mid = "mse", local = TRUE, replace.ids = obs.id)
summary(pfi_sim7)
pfi_sim7$feature.value




interaction_detection_sim7 <- PFI_interaction_identifier_2(pfi_sim7, X_sim7, features = "Q_2", mid = "mse", model = "RandomForest", n_tree = 100)
