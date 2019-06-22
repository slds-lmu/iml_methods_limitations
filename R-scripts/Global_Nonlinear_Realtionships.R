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

plot(data_set$train$x2, data_set$train$y)

### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 1000)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)

ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35))

explainer <- lime(data_set$train[ , 2:4], black_box,
                  bin_continuous = FALSE, use_density = FALSE)

## representative
epsilon <- 0.6
set.seed(2)
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


kernel_widths <- c(seq(0.03, 0.1, 0.0075), seq(0.1, 0.75, 0.05), 
                   seq(0.8, 2, 0.2), seq(2, 4, 1))

km_1 <- analyse_multivariate_kernel_width(kernel_widths,
                                          obs_1[, 2:4], 
                                          explainer,
                                          n_features = 3, 
                                          n_permutations = 5000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE,
                                          seed = 1)

panel1 <- plot_kernels(km_1[[1]], 
                       kernel_widths, 
                       true_coefficients = c(5, -4, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x2 is -4.")

km_2 <- analyse_multivariate_kernel_width(kernel_widths,
                                          obs_2[, 2:4], 
                                          explainer,
                                          n_features = 3, 
                                          n_permutations = 5000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE, 
                                          seed = 2)

panel2 <- plot_kernels(km_2[[1]], 
                       kernel_widths, 
                       true_coefficients = c(5, 6, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x2 is 6.")

km_3 <- analyse_multivariate_kernel_width(kernel_widths,
                                          obs_3[, 2:4], 
                                          explainer,
                                          n_features = 3, 
                                          n_permutations = 5000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE,
                                          seed = 3)

panel3 <- plot_kernels(km_3[[1]], 
                       kernel_widths, 
                       true_coefficients = c(5, -3, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x2 is -3.")

png("04-09-12.png", width = 1000, height = 2800)
grid.arrange(panel1, panel2, panel3, nrow = 3)
dev.off()
