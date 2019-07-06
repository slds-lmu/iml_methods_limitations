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

mean((task_pred$data$truth - task_pred$data$response)^2)

ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)



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
                                          n_permutations = 10000, 
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
                                          n_permutations = 10000, 
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
                                          n_permutations = 10000, 
                                          dist_fun = "euclidean",
                                          ci = TRUE,
                                          seed = 3)

panel3 <- plot_kernels(km_3[[1]], 
                       kernel_widths, 
                       true_coefficients = c(5, -3, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x2 is -3.")

png("04-09-12.png", width = 2800, height = 1000)
grid.arrange(panel1, panel2, panel3, nrow = 1)
dev.off()

frame1 <- as.data.frame(cbind(kernel_widths,
                              km_1[[1]][[3]], 
                              km_1[[2]][[3]], 
                              km_1[[3]][[3]]))

frame2 <- as.data.frame(cbind(kernel_widths,
                              km_2[[1]][[3]], 
                              km_2[[2]][[3]], 
                              km_2[[3]][[3]]))

frame3 <- as.data.frame(cbind(kernel_widths,
                              km_3[[1]][[3]], 
                              km_3[[2]][[3]], 
                              km_3[[3]][[3]]))

colnames(frame1) <- c("Kernel", "Mean", "lower", "upper")
colnames(frame2) <- c("Kernel", "Mean", "lower", "upper")
colnames(frame3) <- c("Kernel", "Mean", "lower", "upper")


plotframe1 <- frame1
plotframe1[plotframe1 > 7] <- 7
plotframe1[plotframe1 < -6] <- -6
panel2_1 <- ggplot(plotframe1, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = plotframe1, size = 3) +
  geom_ribbon(data = plotframe1, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) -4) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") +
  labs(title = "True local coefficient for x2 is -4.")

plotframe2 <- frame2
plotframe2[plotframe2 > 9] <- 9
plotframe2[plotframe2 < -1] <- -1 #
panel2_2 <- ggplot(plotframe2, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = plotframe2, size = 3) +
  geom_ribbon(data = plotframe2, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) 6) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") +
  labs(title = "True local coefficient for x2 is 6.")

plotframe3 <- frame3
plotframe3[plotframe3 > 7] <- 7
plotframe3[plotframe3 < -5] <- -5
panel2_3 <- ggplot(plotframe3, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = plotframe3, size = 3) +
  geom_ribbon(data = plotframe3, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) -3) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") +
  labs(title = "True local coefficient for x2 is -3.")

png("04-09-13.png", width = 2800, height = 1000)
grid.arrange(panel2_1, panel2_2, panel2_3, nrow = 1)
dev.off()

saveRDS(km_1, file = "R-results/kernelmatrix-global_linear1")
saveRDS(km_2, file = "R-results/kernelmatrix-global_linear2")
saveRDS(km_3, file = "R-results/kernelmatrix-global_linear3")