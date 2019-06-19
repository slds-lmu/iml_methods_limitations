library(devtools)
#install_github("https://github.com/pkopper/lime")
library(lime)
library(mlr)
library(gridExtra)
library(ggplot2)
source("~/utils.R")

### First simulation
### We simulate normally distributed data.
### Only one feature deterministically affects the target.
### The association is highly non-linear.
set.seed(1)
x1 <- rnorm(500, 10, 2)
x2 <- rnorm(500, 10, 10)
y <- 4 * (sin(0.06 * x1 ^ 2) + 0.1 * x1) + rnorm(500, 0, 1)
data_set <- make_split(data.frame(y, x1, x2), 0.7)

### We create a black box model using mlr
# Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
# Define the learner (mlr): random forest
learner <- makeLearner("regr.randomForest", ntree = 1000)
# Train the model (mlr)
black_box <- train(learner, task)

### We make oos predictions.
task_pred <- predict(black_box, newdata = data_set$test)
pred_frame <- data.frame(y_hat = task_pred$data$response, 
                         x1 = data_set$test$x1)

### This creates plot #1. Optical goodness of fit evaluation for random forest
png("04-09-02.png", width = 1000, height = 848)
ggplot(data_set$train, aes(x = x1, y = y)) +
  geom_point(size = 5) +
  theme(text = element_text(size = 35)) + ylab("Predicted Value")
dev.off()

### This creates plot # 2: The predictions are displayed together with the 
### true marginal predictive surface.
Fun <- function(x) 4 * (sin(0.06 * x^2) + 0.1 * x)
png("04-09-03.png", width = 1000, height = 848)
ggplot(pred_frame, aes(x = x1, y = y_hat)) +
  geom_point(size = 5) + stat_function(fun = Fun, size = 2) +
  theme(text = element_text(size = 35)) + ylab("Predicted Value")
dev.off()

# This creates plot # 3: A linear approximation of plot #2.
f <- function(x) {
  y <- rep(0, length(x))
  y <- ifelse(x >= min(x) & x < 5.8, 0.225 * x + 4.5, y)
  y <- ifelse(x >= 5.8 & x < 8.496, - 2.3 * x + 19.14, y)
  y <- ifelse(x >= 8.496 & x < 9.1, - 0.4, y)
  y <- ifelse(x >= 9.1 & x < 11.1, 4.25 * x - 39.13, y)
  y <- ifelse(x >= 11.1 & x < 11.8, 8.045, y)
  y <- ifelse(x >= 11.8 & x < 13.1, - 4.5 * x + 61.15, y)
  y <- ifelse(x >= 13.1 & x <= max(x), 2.25 * x - 27.27, y)
  y
}
png("04-09-04.png", width = 1000, height = 848)
ggplot(pred_frame, aes(x = x1, y = y_hat)) +
  geom_point(size = 5) + 
  stat_function(fun = f, size = 2, col = 2) +
  theme(text = element_text(size = 35)) + ylab("Predicted Value")
dev.off()

### We create an observation which we later want an explanation for.
### test_obs is the true observation, test is only necessary for plotting
test_obs <- data.frame(y = 4, x1 = 12.75, x2 = 4)
test_pred <- predict(black_box, newdata = test_obs)
test <- data.frame(y_hat = test_pred$data$response, x1 = test_obs$x1)

### We now Set up LIME a explainer for the black box.
### We use do not bin cont. features.
explainer <- lime(data_set$train[ , 2:3], black_box,
                  bin_continuous = FALSE)

### We generate an explanation (sparse) for test_obs.
### extract_average_local_model uses the explain function and averages
### the resulting models.
### We found the kernel size of 0.08 to be reasonable by grid search.
set.seed(1)
local_model1 <- extract_average_local_model(test_obs[ , 2:3], explainer,
                                           n_features = 1, 
                                           n_permutations = 5000, 
                                           kernel_width = 0.08, 
                                           dist_fun = "euclidean")

### This creates plot #5: The local model found by LIME for the kernel size
### 0.08 and (randomly chosen) 2 in order to contrast the results.
Fun <- function(x, local_model) {
  local_model[1] + local_model[2] * x
}

local_model2 <- extract_average_local_model(test_obs[ , 2:3], explainer,
                                            n_features = 1, 
                                            n_permutations = 5000, 
                                            kernel_width = 2, 
                                            dist_fun = "euclidean")

kernel_widths <- c(0.08, 2)
png("04-09-05.png", width = 1000, height = 848)
p4 + stat_function(fun = Fun, size = 2, 
                   args = list(local_model = local_model1),
                   aes(colour = as.character(kernel_widths[1])))  +
  stat_function(fun = Fun, size = 2, 
                args = list(local_model = local_model2),
                aes(colour = as.character(kernel_widths[2]))) +
  coord_cartesian(ylim = c(-1, 9))  +
  scale_colour_manual("Kernel Width", 
                      values = c("red", "yellow")) + ylab("Predicted Value")
dev.off()

### In order to study the effect of different kernel widths better for
### different observations we compare the effect of a variety of kernel
### width on two observations at very different points in space.
kernel_widths <- seq(0, 2, 0.05)
kernel_widths[1] <- 0.01
kernel_widths <- c(0.005, kernel_widths)
set.seed(2)
result_1 <- analyse_univariate_kernel_width(kernel_widths,
                                            test_obs[, 2:3],
                                            explainer,
                                            n_features = 1, 
                                            n_permutations = 3000,
                                            dist_fun = "euclidean", 
                                            iterations = 100)

### This creates the first panel of plot 6: Different kernel widths for an
### observation where negative slope is appropriate.
p1 <- ggplot(data = pred_frame, aes(x1, y_hat)) +
  geom_point(size = 5) +
  stat_function(fun = function(x) result_1[1, 1] + result_1[1, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[1]))) +
  stat_function(fun = function(x) result_1[3, 1] + result_1[3, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[3]))) + 
  stat_function(fun = function(x) result_1[4, 1] + result_1[4, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[4]))) + 
  stat_function(fun = function(x) result_1[7, 1] + result_1[7, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[7]))) + 
  stat_function(fun = function(x) result_1[8, 1] + result_1[8, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[8]))) + 
  stat_function(fun = function(x) result_1[12, 1] + result_1[12, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[12]))) +
  stat_function(fun = function(x) result_1[22, 1] + result_1[22, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[22]))) +
  stat_function(fun = function(x) result_1[42, 1] + result_1[42, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[42])))
p1 <- p1 + scale_colour_manual("Kernel Width", 
                               values = c("red", "orange", "yellow", 
                                          "darkgreen", "lightblue", "blue", 
                                          "purple", "grey")) +
  geom_point(data = test_1, colour = "green", size = 8) +
  theme(text = element_text(size = 35)) +
  ylim(-1, 9) + labs(title = "True slope is negative.") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Predicted Value")

### This creates the second panel of plot 6: Different kernel widths for an
### observation where positive slope is appropriate.
test_obs_2 <- data.frame(y = 5, x1 = 10.2, x2 = 4)
test_pred_2 <- predict(black_box, newdata = test_obs_2)
test_2 <- data.frame(y_hat = test_pred_2$data$response, x1 = test_obs_2$x1)
set.seed(2)
result_2 <- analyse_univariate_kernel_width(kernel_widths,
                                            test_obs_2[, 2:3],
                                            explainer,
                                            n_features = 1, 
                                            n_permutations = 3000,
                                            dist_fun = "euclidean", 
                                            iterations = 100)

p2 <- ggplot(data = pred_frame, aes(x1, y_hat)) +
  geom_point(size = 5) +
  stat_function(fun = function(x) result_2[1, 1] + result_2[1, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[1]))) +
  stat_function(fun = function(x) result_2[3, 1] + result_2[3, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[3]))) + 
  stat_function(fun = function(x) result_2[4, 1] + result_2[4, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[4]))) + 
  stat_function(fun = function(x) result_2[7, 1] + result_2[7, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[7]))) + 
  stat_function(fun = function(x) result_2[8, 1] + result_2[8, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[8]))) + 
  stat_function(fun = function(x) result_2[12, 1] + result_2[12, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[12]))) +
  stat_function(fun = function(x) result_2[22, 1] + result_2[22, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[22]))) +
  stat_function(fun = function(x) result_2[42, 1] + result_2[42, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[42])))
p2 <- p2 + scale_colour_manual("Kernel Width", 
                               values = c("red", "orange", "yellow",
                                          "darkgreen", "lightblue", 
                                          "blue", "purple", "grey")) +
  geom_point(data = test_2, colour = "green", size = 8) +
  theme(text = element_text(size = 35)) +
  ylim(-1, 9) + labs(title = "True slope is positive.") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Predicted Value")

png("04-09-06.png", width = 1000, height = 1760)
grid.arrange(p1, p2, nrow = 2)
dev.off()
