library(lime)
library(ggplot2)
library(mlr)
library(reshape2)
source("utils.R")

data_set <- make_split(
  simulate_data(2500, 3, seed = 1, mu = c(5, 5, 5), 
              Sigma = matrix(
                c(1, 0.3, 0.4, 0.3, 0.9, 0.2, 0.4, 0.2, 0.8), 
                ncol = 3, nrow = 3, byrow = TRUE),
              true_coefficients = c(4, -3, 5), intercept = 5), 0.9
)

### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 1000)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)

png("04-09-07.png", width = 1000, height = 848)
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35))
dev.off()

# Set up LIME explanations
explainer <- lime(data_set$train[ , 2:4], black_box,
                  bin_continuous = FALSE)
# Choose observation
set.seed(1)
explanation <- explain(data_set$test[1:4, 2:4], explainer,
                       n_features = 3, 
                       n_permutations = 10000, kernel_width = 1, 
                       dist_fun = "euclidean")
as.data.frame(explanation)

kernel_widths <- seq(0.1, 5, 0.1)
kernel_widths <- c(0.01, 0.05, kernel_widths)

kernel_matrix <- analyse_multivariate_kernel_width(kernel_widths,
                                                   data_set$test[1, 2:4], 
                                                   explainer,
                                                   n_features = 3, 
                                                   n_permutations = 1000, 
                                                   dist_fun = "euclidean")

# obs 1 .- plot 8a
kernel_widths <- seq(0.1, 5, 0.1)
kernel_widths <- c(0.01, 0.05, kernel_widths)
kernel_matrix <- analyse_multivariate_kernel_width(kernel_widths,
                                                   data_set$test[1, 2:4], 
                                                   explainer,
                                                   n_features = 3, 
                                                   n_permutations = 1000, 
                                                   dist_fun = "euclidean")

png("04-09-08-1.png", width = 1000, height = 848)
plot_kernels(kernel_matrix, 
             kernel_widths, 
             true_coefficients)
dev.off()

# obs 21 .- plot 8b
kernel_widths <- seq(0.1, 5, 0.1)
kernel_widths <- c(0.01, 0.05, kernel_widths)
kernel_matrix <- analyse_multivariate_kernel_width(kernel_widths,
                                                   data_set$test[21, 2:4], 
                                                   explainer,
                                                   n_features = 3, 
                                                   n_permutations = 1000, 
                                                   dist_fun = "euclidean")

png("04-09-08-2.png", width = 1000, height = 848)
plot_kernels(kernel_matrix, 
             kernel_widths, 
             true_coefficients)
dev.off()

# obs 33 - plot 8c
kernel_widths <- seq(0.1, 5, 0.1)
kernel_widths <- c(0.01, 0.05, kernel_widths)
kernel_matrix <- analyse_multivariate_kernel_width(kernel_widths,
                                                   data_set$test[33, 2:4], 
                                                   explainer,
                                                   n_features = 3, 
                                                   n_permutations = 1000, 
                                                   dist_fun = "euclidean")

png("04-09-08-3.png", width = 1000, height = 848)
plot_kernels(kernel_matrix, 
             kernel_widths, 
             true_coefficients)
dev.off()

# obs 103 - plot 8d
kernel_widths <- seq(0.1, 5, 0.1)
kernel_widths <- c(0.01, 0.05, kernel_widths)
kernel_matrix <- analyse_multivariate_kernel_width(kernel_widths,
                                                   data_set$test[103, 2:4], 
                                                   explainer,
                                                   n_features = 3, 
                                                   n_permutations = 1000, 
                                                   dist_fun = "euclidean")

png("04-09-08-4.png", width = 1000, height = 848)
plot_kernels(kernel_matrix[-15, ], 
             kernel_widths[-15], 
             true_coefficients)
dev.off()

