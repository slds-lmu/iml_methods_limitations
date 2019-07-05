remove.packages("lime")
library(mlr)
library(devtools)
install_github("https://github.com/pkopper/lime/tree/gower")
library(dplyr)
library(lime)
explain <- lime::explain
library(MASS)
library(ggplot2)
library(reshape2)

library(readr)
day <- read_csv("datasets/day.csv")
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day <- day[, c(3, 6, 8:10, 12:13, 16)]
day$season <- as.factor(day$season)
day$weathersit <- as.factor(day$weathersit)
library(data.table)
library(mltools)
day <- as.data.frame(day)
data_set <- make_split(day, 0.85)

##################### rf ############################

task <- makeRegrTask(data = data_set$train, target = "cnt")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 50)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)
ranking <- rank_predictions(task_pred)
sum(ranking)

# fit not as good as before but not that important because we don't know truth.
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)

explainer <- lime(data_set$train[, 1:7], black_box, bin_continuous = FALSE,
                  use_density = FALSE)

kernel_widths <- c(seq(0.07, 0.15, 0.02), seq(0.275, 1, 0.075),
                   seq(1.15, 1.75, 0.15), seq(1.75, 5.75, 1), 20)

km_real <- vector(mode = "list", length = 5)
km_real_abs <- vector(mode = "list", length = 5)
for (i in 1:length(km_real)) {
  km_real[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[i, 1:7], 
                                                        explainer,
                                                        n_features = 3, 
                                                        n_permutations = 6500, 
                                                        dist_fun = "euclidean",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "auto",
                                                        iterations = 48))
}
for (i in 1:length(km_real_abs)) {
  km_real_abs[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[i, 1:7], 
                                                        explainer,
                                                        n_features = 3, 
                                                        n_permutations = 6500, 
                                                        dist_fun = "euclidean",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "highest_weights",
                                                        iterations = 48))
}
panels <- vector(mode = "list", length = 3)
j <- 0
for (i in c(1, 3, 5)) {
  j <- j + 1
  panels[[j]] <- plot_pseudo_stability_paths(kernel_widths, 
                                    stability_paths = km_real[[i]][[2]][, 2:8],
                                    2, title = paste("Observation", 
                                                     as.character(i)))
}
png("04-09-14.png", width = 2600, height = 800)
grid.arrange(panels[[1]], panels[[2]], panels[[3]], nrow = 1)
dev.off()

##################### earth ############################

task <- makeRegrTask(data = data_set$train, target = "cnt")
### Define the learner (mlr)
learner <- makeLearner("regr.earth")
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)
ranking <- rank_predictions(task_pred)
sum(ranking)

# fit not as good as before but not that important because we don't know truth.
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)


explainer <- lime(data_set$train[, 1:8], black_box, bin_continuous = FALSE,
                  use_density = FALSE)

kernel_widths <- c(seq(0.07, 0.15, 0.02), seq(0.275, 1, 0.075),
                   seq(1.15, 1.75, 0.15), seq(1.75, 5.75, 1), 20)

km_real2 <- vector(mode = "list", length = 8)

for (i in 1:length(km_real2)) {
  km_real2[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                         data_set$test[i, 1:8], 
                                                         explainer,
                                                         n_features = 4, 
                                                         n_permutations = 8000, 
                                                         dist_fun = "euclidean",
                                                         seed = 1,
                                                         ci = TRUE,
                                                         feature_select = 
                                                           "auto",
                                                         iterations = 48))
}

panels <- vector(mode = "list", length = 3)
j <- 0
for (i in c(1, 3, 5)) {
  j <- j + 1
  panels[[j]] <- plot_pseudo_stability_paths(kernel_widths, 
                                             stability_paths = km_real2[[i]][[2]][, 2:8],
                                             2, title = paste("Observation", 
                                                              as.character(i)))
}
png("04-09-15.png", width = 2600, height = 800)
grid.arrange(panels[[1]], panels[[2]], panels[[3]], nrow = 1)
dev.off()

