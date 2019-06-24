data_set <- make_split(Boston, 0.975)
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

explainer <- lime(data_set$train[, 2:14], black_box,
                  bin_continuous = FALSE, use_density = FALSE) ## BUG!!!!!!

kernel_widths <- c(seq(0.15, 1, 0.075), 
                   seq(1, 2, 0.125), seq(2, 5, 0.75))

km_real <- vector(mode = "list", length = nrow(data_set$test))

for (i in 1:length(km_real)) {
  km_real[[i]] <- analyse_multivariate_kernel_width(kernel_widths,
                                                    data_set$test[3, 2:14], 
                                                    explainer,
                                                    n_features = 5, 
                                                    n_permutations = 1000, 
                                                    dist_fun = "euclidean",
                                                    seed = 1,
                                                    ci = TRUE,
                                                    feature_select = 
                                                      "lasso_path")
}

plot_stability_paths(kernel_widths, stability_paths = km_real[[2]][[2]][, 2:14])
