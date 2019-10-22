set.seed(1)
### Data simulation based on function out of utils.R
### nonlinear_intervals argument must be handed over in this fashion.
### We have a piece-wise linear model as ground truth.
### We have break points or knots at 4 and 6; Up to 4 the true slope is -4;
### then between 4 and 6 the slope equals to 6 and afterwards to -3.
nonlinear_intervals = list(
  NULL, list(knots = c(4, 6), coefs = c(-4, 6, -3)),
  NULL)

data_set <- simulate_data(3000, 
                          3,
                          nonlinear_intervals = nonlinear_intervals, 
                          seed = 2, 
                          mu = c(5, 5, 5), 
                          Sigma = matrix(
                            c(0.6, 0.2, 0.1, 0.2, 0.8, 0.1, 0.1, 0.1, 0.6),
                            ncol = 3, nrow = 3, byrow = TRUE), 
                          true_coefficients = c(5, NA, 3), 
                          intercept = 2.5) %>% make_split(share = 0.9)

### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
### Define the learner (mlr)
learner <- makeLearner("regr.earth")
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)
### mse 
mean((task_pred$data$truth - task_pred$data$response)^2)
### graphical sanity check.
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)
### explainer (LIME)
explainer <- lime(data_set$train[ , 2:4], black_box,
                  bin_continuous = FALSE, use_density = FALSE)

### epsilon grants reasonable margin to knots.
epsilon <- 0.6
set.seed(2)
### We select representative observations (one per bin).
# x_2 < 4
candidates <- data_set$test[data_set$test$x2 < 4 - epsilon, ]
obs_1 <- candidates[sample(1:nrow(candidates), 1), ]
# x_2 > 4 & x_2 < 6
candidates <- data_set$test[data_set$test$x2 > 4 + epsilon &
                              data_set$test$x2 < 6 + epsilon, ]
obs_2 <- candidates[sample(1:nrow(candidates), 1), ]
# x_2 > 6
candidates <- data_set$test[data_set$test$x2 > 6 + epsilon, ]
obs_3 <- candidates[sample(1:nrow(candidates), 1), ]

### The grid of kernel widths which we want to analyse.
kernel_widths <- c(seq(0.03, 0.1, 0.0075), seq(0.1, 0.75, 0.05), 
                   seq(0.8, 2, 0.2), seq(2, 4, 1))

### The first observation.
km_1 <- analyse_multivariate_kernel_width(kernel_widths,
                                          obs_1[, 2:4], 
                                          explainer,
                                          n_features = 3, 
                                          n_permutations = 10000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE,
                                          seed = 1)

### The second observation.
km_2 <- analyse_multivariate_kernel_width(kernel_widths,
                                          obs_2[, 2:4], 
                                          explainer,
                                          n_features = 3, 
                                          n_permutations = 10000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE, 
                                          seed = 2)

### The third observation.
km_3 <- analyse_multivariate_kernel_width(kernel_widths,
                                          obs_3[, 2:4], 
                                          explainer,
                                          n_features = 3, 
                                          n_permutations = 10000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE,
                                          seed = 3)

### Save for reproducibility and plots.

saveRDS(km_1, 
        file = "R-results/LIME/Neighbourhood/kernelmatrix-global_nonlinear1.RDS")
saveRDS(km_2, 
        file = "R-results/LIME/Neighbourhood/kernelmatrix-global_nonlinear2.RDS")
saveRDS(km_3, 
        file = "R-results/LIME/Neighbourhood/kernelmatrix-global_nonlinear3.RDS")
saveRDS(kernel_widths, 
        file = "R-results/LIME/Neighbourhood/kw_global_nonlinear.RDS")

### For figure 16
### We show that explanations using Gower dissimilarty are poor.
gower_model <- analyse_multivariate_kernel_width(1,
                                                 obs_2[, 2:4], 
                                                 explainer,
                                                 n_features = 3, 
                                                 n_permutations = 6500, 
                                                 dist_fun = "gower",
                                                 seed = 1,
                                                 ci = TRUE,
                                                 feature_select = "auto",
                                                 iterations = 48)[[1]]

frame <- data.frame(coefficient = km_2[[1]][, 3],
                    gower = rep(gower_model[[3]], length(kernel_widths)),
                    true = rep(6, length(kernel_widths)))
melted_frame <- data.frame(kernel = rep(kernel_widths, 3), melt(frame))

### Save for reproducibility and plots.

saveRDS(melted_frame, 
        file = "R-results/LIME/Neighbourhood/gower_comparison.RDS")
