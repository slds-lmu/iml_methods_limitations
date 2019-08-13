### Simulation refering to the section 
### "The problem in a one-dimensional setting"
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

### TOptical goodness of fit evaluation for random forest
fig2 <- ggplot(data_set$train, aes(x = x1, y = y)) +
  geom_point(size = 5) +
  theme(text = element_text(size = 35)) + ylab("Predicted Value")
saveRDS(fig2, file = "R-results/LIME/Neighbourhood/fig2.RDS")

### The predictions are displayed together with the true marginal predictive 
### surface.
Fun <- function(x) 4 * (sin(0.06 * x^2) + 0.1 * x)
p <- ggplot(pred_frame, aes(x = x1, y = y_hat)) +
  geom_point(size = 5) + stat_function(fun = Fun, size = 2) +
  theme(text = element_text(size = 35)) + ylab("Predicted Value")
saveRDS(p, file = "R-results/LIME/Neighbourhood/fig3.RDS")

### We aim to express the non-linear association by piece-wise linear models.
### The numeric values have been computed in order to grant continuity of the piece-wise
### piece-wise linear model.
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

fig4 <- ggplot(pred_frame, aes(x = x1, y = y_hat)) +
  geom_point(size = 5) + 
  stat_function(fun = f, size = 2, col = 2) +
  theme(text = element_text(size = 35)) + ylab("Predicted Value")
saveRDS(fig4, file = "R-results/LIME/Neighbourhood/fig4.RDS")

### We create an observation which we later want an explanation for.
### test_obs is the true observation, test is only necessary for plotting
test_obs <- data.frame(y = 4, x1 = 12.75, x2 = 4)
test_pred <- predict(black_box, newdata = test_obs)
test_1 <- data.frame(y_hat = test_pred$data$response, x1 = test_obs$x1)

### We now Set up LIME a explainer for the black box.
### We use do not bin cont. features.
explainer <- lime(data_set$train[ , 2:3], black_box,
                  bin_continuous = FALSE, use_density = FALSE)

### We generate an explanation (sparse) for test_obs.
### extract_average_local_model uses the explain function and averages
### the resulting models.
### We found the kernel size of 0.08 to be reasonable by grid search.
set.seed(1)
local_model1 <- extract_average_local_model(test_obs[ , 2:3], explainer,
                                            n_features = 1, 
                                            n_permutations = 5000, 
                                            kernel_width = 0.08, 
                                            dist_fun = "euclidean",
                                            seed = 1,
                                            feature_select = "auto",
                                            iterations = 5000,
                                            se = FALSE)
local_model1 <- local_model1[[1]][1:2]
### This creates plot #5: The local model found by LIME for the kernel size
### 0.08 and (randomly chosen) 2 in order to contrast the results.
local_model2 <- extract_average_local_model(test_obs[ , 2:3], explainer,
                                            n_features = 1, 
                                            n_permutations = 5000, 
                                            kernel_width = 2, 
                                            dist_fun = "euclidean", 
                                            seed = 1, 
                                            feature_select = "auto",
                                            iterations = 5000,
                                            se = FALSE)
local_model2 <- local_model2[[1]][1:2]

### In order to study the effect of different kernel widths better for
### different observations we compare the effect of a variety of kernel
### width on two observations at very different points in space.
kernel_widths <- seq(0, 2, 0.05)
kernel_widths[1] <- 0.01
kernel_widths <- c(0.01, kernel_widths)

### The true coefficient is negative.
result_1 <- analyse_univariate_kernel_width(kernel_widths,
                                            test_obs[, 2:3],
                                            explainer,
                                            n_features = 1, 
                                            n_permutations = 3000,
                                            dist_fun = "euclidean", 
                                            iterations = 100,
                                            seed = 2)


test_obs_2 <- data.frame(y = 5, x1 = 10.2, x2 = 4)
test_pred_2 <- predict(black_box, newdata = test_obs_2)
test_2 <- data.frame(y_hat = test_pred_2$data$response, x1 = test_obs_2$x1)

### The true coefficient is positive.
result_2 <- analyse_univariate_kernel_width(kernel_widths,
                                            test_obs_2[, 2:3],
                                            explainer,
                                            n_features = 1, 
                                            n_permutations = 3000,
                                            dist_fun = "euclidean", 
                                            iterations = 100,
                                            seed = 2)

### Save for reproducibility and plots.
saveRDS(pred_frame, 
        file = "R-results/LIME/Neighbourhood/univariate_predframe.RDS")
saveRDS(test_1, file = "R-results/LIME/Neighbourhood/univariate_test_1.RDS")
saveRDS(test_2, file = "R-results/LIME/Neighbourhood/univariate_test_2.RDS")
saveRDS(result_1, file = "R-results/LIME/Neighbourhood/univariate_1.RDS")
saveRDS(result_2, file = "R-results/LIME/Neighbourhood/univariate_2.RDS")
saveRDS(kernel_widths, file = "R-results/LIME/Neighbourhood/kw_univariate.RDS")
saveRDS(local_model1, 
        file = "R-results/LIME/Neighbourhood/univariate_local_1.RDS")
saveRDS(local_model2, 
        file = "R-results/LIME/Neighbourhood/univariate_local_2.RDS")
