library(dplyr)
library(mlr)
library(lime)
library(gridExtra)
library(ggplot2)
library(reshape2)
source("utils.R")
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
learner <- makeLearner("regr.randomForest", ntree = 100)
learner <- makeLearner("regr.earth")
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)

png("04-09-09.png", width = 1000, height = 848)
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)
dev.off()

explainer <- lime(data_set$train[ , 2:4], black_box,
                  bin_continuous = FALSE, use_density = FALSE)

## representative
epsilon <- 0.75
set.seed(1)
# x_1 < 6
candidates <- data_set$test[data_set$test$x1 < 6 - epsilon, ]
obs_1 <- candidates[sample(1:nrow(candidates), 2), ]
# x_1 > 6
candidates <- data_set$test[data_set$test$x1 > 6 + 0.5 * epsilon, ]
obs_2 <- candidates[sample(1:nrow(candidates), 2), ]


kernel_widths <- c(seq(0.02, 0.1, 0.0075), seq(0.1, 0.75, 0.05), 
                   seq(0.8, 2, 0.2))

km_1_1 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_1[1, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE,
                                            seed = 1)

km_1_2 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_1[2, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE, 
                                            seed = 2)

km_2_1 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_2[1, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE,
                                            seed = 3)

km_2_2 <- analyse_multivariate_kernel_width(kernel_widths,
                                            obs_2[2, 2:4], 
                                            explainer,
                                            n_features = 3, 
                                            n_permutations = 10000, 
                                            dist_fun = "euclidean",
                                            ci = TRUE,
                                            seed = 4)

km_nc_1_1 <- km_1_1[[1]]
km_nc_1_2 <- km_1_2[[1]]
km_nc_2_1 <- km_2_1[[1]]
km_nc_2_2 <- km_2_2[[1]]


panel1 <- plot_kernels(km_nc_1_1, 
             kernel_widths, 
             true_coefficients = c(5, -4, 3), 
             ymin = -10, ymax = 10,
             title = "True local coefficient for x1 is 5.")

panel2 <- plot_kernels(km_nc_1_2, 
             kernel_widths, 
             true_coefficients = c(5, -4, 3), 
             ymin = -10, ymax = 10,
             title = "True local coefficient for x1 is 5.")

panel3 <- plot_kernels(km_nc_2_1, 
             kernel_widths, 
             true_coefficients = c(0, -4, 3),
             ymin = -10, ymax = 10,
             title = "True local coefficient for x1 is 0.")

panel4 <- plot_kernels(km_nc_2_2, 
             kernel_widths, 
             true_coefficients = c(0, -4, 3), 
             ymin = -10, ymax = 10,
             title = "True local coefficient for x1 is 0.")

png("04-09-10.png", width = 2000, height = 1700)
grid.arrange(panel1, panel2, panel3, panel4, nrow = 2)
dev.off()

x1_frame <- as.data.frame(cbind(kernel_widths,
                                km_2_1[[1]][[2]], 
                                km_2_1[[2]][[2]], 
                                km_2_1[[3]][[2]]))

x2_frame <- as.data.frame(cbind(kernel_widths,
                                km_2_1[[1]][[3]], 
                                km_2_1[[2]][[3]], 
                                km_2_1[[3]][[3]]))

x3_frame <- as.data.frame(cbind(kernel_widths,
                                km_2_1[[1]][[4]], 
                                km_2_1[[2]][[4]], 
                                km_2_1[[3]][[4]]))

colnames(x1_frame) <- c("Kernel", "Mean", "lower", "upper")
colnames(x2_frame) <- c("Kernel", "Mean", "lower", "upper")
colnames(x3_frame) <- c("Kernel", "Mean", "lower", "upper")

x1_plot <- x1_frame
x1_plot[x1_plot > 7] <- 7
x1_plot[x1_plot < -7] <- -7
png("04-09-11-1.png", width = 1000, height = 848)
ggplot(x1_plot, aes(y = Mean, x = Kernel)) +
    geom_point(size = 3) +
    geom_line(data = x1_plot, size = 3) +
    geom_ribbon(data = x1_plot, aes(ymin = lower, ymax = upper), 
                alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                         fun = function(x) 0) +
  theme(text = element_text(size = 35)) + ylab("Coefficient")
  
dev.off()
  
x2_plot <- x2_frame
x2_plot[x2_plot > 3] <- 3
x2_plot[x2_plot < -11] <- -11
png("04-09-11-2.png", width = 1000, height = 848)
ggplot(x2_plot, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = x2_plot, size = 3) +
  geom_ribbon(data = x2_plot, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) -4) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") 
dev.off()

x3_plot <- x3_frame
x3_plot[x3_plot > 7] <- 10
x3_plot[x3_plot < -4] <- -4
png("04-09-11-3.png", width = 1000, height = 848)
ggplot(x3_plot, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = x3_plot, size = 3) +
  geom_ribbon(data = x3_plot, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) 3) +
  theme(text = element_text(size = 35)) + ylab("Coefficient")
dev.off()

saveRDS(km_1_1, file = "R-results/kernelmatrix-local_linear1_1")
saveRDS(km_1_2, file = "R-results/kernelmatrix-local_linear1_2")
saveRDS(km_2_1, file = "R-results/kernelmatrix-local_linear2_1")
saveRDS(km_2_2, file = "R-results/kernelmatrix-local_linear2_2")
