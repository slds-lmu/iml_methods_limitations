set.seed(1)
### Data simulation based on function out of utils.R
### The piece_wise_intervals argument must be handed over in this fashion.
### From -10 to 5 the coefficinet of x1 equals 5; then afterwards 0.
data_set <- simulate_data(2500, 
                          3,
                          piece_wise_intervals = list(
                            list(lower = -10, upper = 5), NULL,
                            NULL), 
                          seed = 1, 
                          mu = c(5, 5, 5), 
                          Sigma = matrix(
                            c(0.6, 0.2, 0.1, 0.2, 0.8, 0.1, 0.1, 0.1, 0.6),
                            ncol = 3, nrow = 3, byrow = TRUE), 
                          true_coefficients = c(5, -4, 3), 
                          intercept = 2.5) %>% make_split(share = 0.9)

### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
### Define the learner (mlr)
learner <- makeLearner("regr.earth")
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)
saveRDS(task_pred, file = "R-results/LIME/Neighbourhood/task_pred_mars1.RDS")

### LIME explainer
explainer <- lime(data_set$train[ , 2:4], black_box,
                  bin_continuous = FALSE, use_density = FALSE)

### epsilon grants reasonable margin to knots.
epsilon <- 0.75
set.seed(1)
### We select representative observations.
# x_1 < 6
candidates <- data_set$test[data_set$test$x1 < 6 - epsilon, ]
obs_1 <- candidates[sample(1:nrow(candidates), 2), ]
# x_1 > 6
candidates <- data_set$test[data_set$test$x1 > 6 + 0.5 * epsilon, ]
obs_2 <- candidates[sample(1:nrow(candidates), 2), ]

### The grid of kernel widths which we want to analyse.
kernel_widths <- c(seq(0.02, 0.1, 0.0075), seq(0.1, 0.75, 0.05), 
                   seq(0.8, 2, 0.2))

### First observation of the first "bin".
km_1_1 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_1[1, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE,
                                            seed = 1)

### Second observation of the first "bin".
km_1_2 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_1[2, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE, 
                                            seed = 2)

### First observation of the second "bin".
km_2_1 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_2[1, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE,
                                            seed = 3)

### Second observation of the second "bin".
km_2_2 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_2[2, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE,
                                            seed = 4)

### Save for reproducibility and plots.
saveRDS(km_1_1, 
        file = "R-results/LIME/Neighbourhood/kernelmatrix-local_linear1_1.RDS")
saveRDS(km_1_2,
        file = "R-results/LIME/Neighbourhood/kernelmatrix-local_linear1_2.RDS")
saveRDS(km_2_1, 
        file = "R-results/LIME/Neighbourhood/kernelmatrix-local_linear2_1.RDS")
saveRDS(km_2_2, 
        file = "R-results/LIME/Neighbourhood/kernelmatrix-local_linear2_2.RDS")
saveRDS(kernel_widths, 
        file = "R-results/LIME/Neighbourhood/kw_local_linear.RDS")
