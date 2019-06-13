library(devtools)
install_github("https://github.com/pkopper/lime")
library(lime)
library(mlr)
install.packages("SiZer")
library(SiZer)

source("~/utils.R")

### First simulation
set.seed(1)
x1 <- rnorm(500, 10, 2)
x2 <- rnorm(500, 10, 10)
y <- 4 * (sin(0.06 * x1^2) + 0.1 * x1) + rnorm(500, 0, 1)
data_set <- make_split(data.frame(y, x1, x2), 0.7)

### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "y")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 1000)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)
pred_frame <- data.frame(y_hat = task_pred$data$response, 
                         x1 = data_set$test$x1)

# plot number 1
ggplot(data_set$train, aes(x = x1, y = y)) +
  geom_point(size = 3)

# plot number 2
Fun <- function(x) 4 * (sin(0.06 * x^2) + 0.1 * x)
ggplot(pred_frame, aes(x = x1, y = y_hat)) +
  geom_point(size = 3) + stat_function(fun = Fun, size = 1.25)

# plot number 3
f <- function(x) {
  y <- rep(0, length(x))
  y <- ifelse(x >= min(x) & x < 5.6, 0.225 * x + 4.5, y)
  y <- ifelse(x >= 5.6 & x < 6.75, - 0.9 * x + 10.9, y)
  y <- ifelse(x >= 6.75 & x < 8.25, - 3 * x + 25, y)
  y <- ifelse(x >= 9 & x < 9.5, 2 * x - 18, y)
  y <- ifelse(x >= 9.5 & x < 10, 2.51 * x - 22.85, y)
  y <- ifelse(x >= 10 & x < 11.1, 5.6 * x - 53.5, y)
  y <- ifelse(x >= 11.1 & x < 12, 8.1, y)
  y <- ifelse(x >= 12 & x < 13.4, - 4.52 * x + 62.41, y)
  y <- ifelse(x >= 13.4 & x <= max(x), 2.75 * x - 34.9, y)
  y
}
ggplot(pred_frame, aes(x = x1, y = y_hat)) +
  geom_point(size = 3) + 
  stat_function(fun = f, size = 1.25, col = 2)

# Set up LIME explanations
explainer <- lime(df[, 2:3], black_box, bin_continuous = FALSE)
# Choose observation
test_obs <- data.frame(x_1 = 13, x_2 = 4, y = 4)
set.seed(1)
explanation <- explain(new_df[, 2:3, drop = FALSE], explainer,
                       n_features = 1, 
                       n_permutations = 10000, kernel_width = 2, 
                       dist_fun = "euclidean")
as.data.frame(explanation)

# Compute the explanations for different kernel widths
# do not run always: seems not too variable here, takes some time

kernel_widths <- seq(0.1, 5, 0.1)
res <- rep(0, length(kernel_widths))
res <- cbind(res, res)
i = 0
for (k in kernel_widths) {
  print(k)
  i = i + 1
  vec <- rep(0, 50)
  for (j in 1:50) {
    explanation <- explain(new_df[ ,1:2, drop = FALSE], explainer, 
                           n_features = 1, dist_fun = "euclidean", 
                           kernel_width = k)
    vec[j] <- explanation$feature_weight
  }
  res[i, ] <- c(mean(vec), sd(vec))
}

res <- as.data.frame(res)
colnames(res) <- c("mean", "sd")

frame <- data.frame(mean = res[, 1],
                    lwr = res[, 1] - 1.96 * res[, 2],
                    upr = res[, 1] + 1.96 * res[, 2])


(p1 <- ggplot(frame, aes(kernel_widths, mean))+
    geom_point()+
    geom_line(data = frame) +
    geom_ribbon(data = frame, aes(ymin = lwr,ymax = upr), alpha = 0.3))
p1

prediction <- predict(black_box, task = task)
kernel_widths <- seq(0, 2, 0.05)
kernel_widths[1] <- 0.01
kernel_widths <- c(0.005, kernel_widths)
result <- rep(0, length(kernel_widths))
result <- cbind(result, result)
colnames(result) <- c("b0", "b1")
result <- as.data.frame(result)
i = 0
for (k in kernel_widths) {
  i <- i + 1
  explanation <- explain(new_df[ ,1:2, drop = FALSE], explainer, 
                         n_features = 1, dist_fun = "euclidean", 
                         kernel_width = k)
  result[i, ] = c(explanation$model_intercept, explanation$feature_weight)
}
plot(x_1, prediction$data$response)
for (i in 1:nrow(result)) {
  abline(a = result$b0[i], b = result$b1[i])
}

(p1 <- ggplot(frame, aes(kernel_widths, mean))+
    geom_point()+
    geom_line(data = frame) +
    geom_ribbon(data = frame, aes(ymin = lwr, ymax = upr), alpha = 0.3))


f0 <- function(a, b, x) a + b * x

p <- ggplot(data = df, aes(x_1, y)) +
  geom_point() +
  stat_function(fun = function(x) result[1, 1] + result[1, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[1]))) +
  stat_function(fun = function(x) result[3, 1] + result[3, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[3]))) + 
  stat_function(fun = function(x) result[4, 1] + result[4, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[4]))) + 
  stat_function(fun = function(x) result[7, 1] + result[7, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[7]))) + 
  stat_function(fun = function(x) result[13, 1] + result[13, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[13]))) + 
  stat_function(fun = function(x) result[23, 1] + result[23, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[23]))) +
  stat_function(fun = function(x) result[43, 1] + result[43, 2] * x, 
                size = 1, aes(colour = as.character(kernel_widths[43])))
p + scale_colour_manual("Kernel Width", 
                        values = c("red", "orange", "yellow", "green", 
                                   "blue", "purple", "white")) +
  ylim(min(y), max(y))

data_points <- data.frame(x_1 = 1:200, x_2 = 0)
kernel_widths <- seq(0.1, 4, 0.1)
kernel_widths <- c(0.001, 0.005, 0.01, 0.05, kernel_widths)
solutions <- matrix(0, ncol = length(kernel_widths), nrow = nrow(data_points))
colnames(solutions) <- kernel_widths
seed = set.seed(1)
for (i in 1:nrow(data_points)) {
  current_point <- data_points[i, , drop = FALSE]
  for (j in 5:length(kernel_widths)) {
    current_kernel <- kernel_widths[j]
    r2 <- rep(0, 5)
    for (k in 1:5) {
      explanation <- explain(current_point, explainer, 
                             n_features = 1, dist_fun = "euclidean", 
                             bin_continous = TRUE,
                             kernel_width = current_kernel, n_permutations = 300)
      r2[k] <- explanation$model_r2
    }
    solutions[i, j] <- mean(r2)
  }
  print(i)
}
apply(solutions, 1, which.max)
pmax(solutions)
