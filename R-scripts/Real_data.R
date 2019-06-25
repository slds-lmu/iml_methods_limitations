remove.packages("lime")
library(mlr)
library(devtools)
install_github("https://github.com/pkopper/lime")
library(dplyr)
library(lime)
explain <- lime::explain
library(MASS)
df <- Boston[, c(1:3, 5:14)]
data_set <- make_split(df, 0.975)
data_set <- scale_data(data_set)
data_set <- data_set[[1]]
scales <- data_set[[2]]
### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "crim")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 500)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)

# fit not as good as before but not that important because we don't know truth.
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35))

#explainer <- lime(data_set$train[, 2:14], black_box,
 #                 bin_continuous = FALSE, use_density = FALSE) ## BUG!!!!!!
explainer <- lime(data_set$train[, 2:13], black_box, bin_continuous = FALSE,
                  use_density =  TRUE)
explain(data_set$train[1, 2:13], explainer, n_features = 3)

kernel_widths <- c(seq(0.075, 1, 0.075), 
                   seq(1, 2, 0.125), seq(2, 5, 0.5), seq(5, 45, 3))

km_real <- vector(mode = "list", length = 1)

for (i in 1:length(km_real)) {
  km_real[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[i, 2:13], 
                                                        explainer,
                                                        n_features = 5, 
                                                        n_permutations = 1000, 
                                                        dist_fun = "euclidean",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "auto"))
}

plot_stability_paths(kernel_widths, 
                     stability_paths = km_real[[1]][[2]][, 2:13],
                     20)

