source("R-scripts/packages.R")
source("R-scripts/utils.R")

data_set <- simulate_data(2500, 
                          3, 
                          seed = 1, 
                          mu = c(5, 5, 5), 
                          Sigma = matrix(
                            c(1, 0.3, 0.4, 0.3, 0.9, 0.2, 0.4, 0.2, 0.8), 
                            ncol = 3, nrow = 3, byrow = TRUE),
                          true_coefficients = c(4, -3, 5), 
                          intercept = 5) %>% make_split(0.9)

### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
### Define the learner (mlr)
learner <- makeLearner("regr.glm", family = "gaussian")
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)

png("04-09-07.png", width = 1000, height = 848)
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35))
dev.off()

# Set up LIME explainer
explainer <- lime(data_set$train[ , 2:4], black_box,
                  bin_continuous = FALSE, use_density = FALSE)

# obs 1 .- plot 8a
kernel_widths <- seq(0.1, 3, 0.1)
kernel_widths <- c(0.025, 0.05, kernel_widths)
kernel_matrix1 <- analyse_multivariate_kernel_width(kernel_widths,
                                                    data_set$test[1, 2:4], 
                                                    explainer,
                                                    n_features = 3, 
                                                    n_permutations = 1000, 
                                                    dist_fun = "euclidean",
                                                    seed = 1)

panel1 <- plot_kernels(kernel_matrix1, kernel_widths, c(4, -3, 5), "",
                       ymin = -7, ymax = 13)

# obs 21 .- plot 8b
kernel_matrix2 <- analyse_multivariate_kernel_width(kernel_widths,
                                                    data_set$test[21, 2:4], 
                                                    explainer,
                                                    n_features = 3, 
                                                    n_permutations = 1000, 
                                                    dist_fun = "euclidean",
                                                    seed = 1)

panel2 <- plot_kernels(kernel_matrix2, kernel_widths, c(4, -3, 5), "",
                       ymin = -5, ymax = 8)

# obs 33 - plot 8c
kernel_matrix3 <- analyse_multivariate_kernel_width(kernel_widths,
                                                    data_set$test[33, 2:4], 
                                                    explainer,
                                                    n_features = 3, 
                                                    n_permutations = 1000, 
                                                    dist_fun = "euclidean",
                                                    seed = 1)

panel3 <- plot_kernels(kernel_matrix3, kernel_widths, c(4, -3, 5), "",
                       ymin = -5, ymax = 8)

# obs 103 - plot 8d
kernel_matrix4 <- analyse_multivariate_kernel_width(kernel_widths,
                                                   data_set$test[103, 2:4], 
                                                   explainer,
                                                   n_features = 3, 
                                                   n_permutations = 1000, 
                                                   dist_fun = "euclidean",
                                                   seed = 1)


panel4 <- plot_kernels(kernel_matrix4, kernel_widths, c(4, -3, 5), "",
                       ymin = -5, ymax = 8)

png("04-09-08.png", width = 2000, height = 1700)
grid.arrange(panel1, panel2, panel3, panel4, ncol = 2)
dev.off()

png("04-09-08a.png", width = 1000, height = 848)
panel1
dev.off()

png("04-09-08b.png", width = 1000, height = 848)
panel2
dev.off()

png("04-09-08c.png", width = 1000, height = 848)
panel3
dev.off()

png("04-09-08d.png", width = 1000, height = 848)
panel4
dev.off()

saveRDS(kernel_matrix1, file = "R-results/kernelmatrix-global_linear1.RDS")
saveRDS(kernel_matrix2, file = "R-results/kernelmatrix-global_linear2.RDS")
saveRDS(kernel_matrix3, file = "R-results/kernelmatrix-global_linear3.RDS")
saveRDS(kernel_matrix4, file = "R-results/kernelmatrix-global_linear4.RDS")
saveRDS(kernel_widths, file = "R-results/kw_global_linear.RDS")
