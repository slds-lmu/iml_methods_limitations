remove.packages("lime")
library(mlr)
library(devtools)
install_github("https://github.com/pkopper/lime")
library(dplyr)
library(lime)
explain <- lime::explain
library(MASS)
df <- Boston[, 1:10]
data_set <- make_split(df, 0.85)
data_set <- scale_data(data_set)
data_set <- data_set[[1]]
scales <- data_set[[2]]
### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "crim")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 90)
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

#explainer <- lime(data_set$train[, 2:14], black_box,
 #                 bin_continuous = FALSE, use_density = FALSE) ## BUG!!!!!!
explainer <- lime(data_set$train[, 2:10], black_box, bin_continuous = FALSE,
                  use_density =  TRUE)
explain(data_set$train[1, 2:10], explainer, n_features = 3, 
        n_permutations = 5000, kernel_width = 0.01)

kernel_widths <- c(seq(0.03, 0.15, 0.025), seq(0.25, 1, 0.1),
                   seq(1.2, 2.5, 0.2))

km_real <- vector(mode = "list", length = 1)

for (i in 1:length(km_real)) {
  km_real[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[2, 2:10], 
                                                        explainer,
                                                        n_features = 3, 
                                                        n_permutations = 3000, 
                                                        dist_fun = "euclidean",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "auto"))
}

plot_stability_paths(kernel_widths, 
                     stability_paths = km_real[[1]][[2]][, 2:10],
                     3)
## without 'global' ones
plot_stability_paths(kernel_widths, 
                     stability_paths = km_real[[1]][[2]][, c(2:3, 5, 6, 8, 10)],
                     2)
## wir sehen klare Lokalität von nox sonst diagreement
