remove.packages("lime")
library(mlr)
library(devtools)
install_github("https://github.com/pkopper/lime/tree/gower")
library(dplyr)
library(lime)
explain <- lime::explain
library(MASS)
library(ggplot2)
library(reshape2)
df <- Boston
df$chas <- as.factor(df$chas)
data_set <- make_split(df, 0.85)

#data_set <- data_set[[1]]
#scales <- data_set[[2]]
### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "crim")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 90)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)
ranking <- rank_predictions(task_pred)
sum(ranking)

# fit not as good as before but not that important because we don't know truth.
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)

#explainer <- lime(data_set$train[, 2:14], black_box,
 #                 bin_continuous = FALSE, use_density = FALSE) ## BUG!!!!!!
explainer <- lime(data_set$train[, 2:14], black_box, bin_continuous = FALSE,
                  use_density =  TRUE)

a <- as.data.frame(explain(data_set$train[1, 2:14], explainer, n_features = 3, 
        n_permutations = 5000, kernel_width = 0.01, 
        feature_select = "lasso_path"))

model <- regsubsets(crim ~.,
                    data = data_set$train, nbest = 1, nvmax = 20,
                    really.big = TRUE
)
set.seed(110101)
stability_paths <- get_stability_paths(model, data_set$train, reps = 100)
stability_paths <- stability_paths[, order(colnames(stability_paths))]
plot_stability_paths(stability_paths)

kernel_widths <- c(seq(0.07, 0.15, 0.02), seq(0.275, 1, 0.075),
                   seq(1.25, 2.5, 0.25), seq(3.5, 5.5, 1))

km_real <- vector(mode = "list", length = 2)

for (i in 1:length(km_real)) {
  km_real[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[i, 2:14], 
                                                        explainer,
                                                        n_features = 5, 
                                                        n_permutations = 15000, 
                                                        dist_fun = "gower",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "auto",
                                                        iterations = 25))
}

plot_pseudo_stability_paths(kernel_widths, 
                     stability_paths = km_real[[1]][[2]][, 2:14],
                     5)


frame_zn <- data.frame(kernel_widths, 
                       means = km_real[[1]][[1]][[1]]$zn, 
                       lwr = km_real[[1]][[1]][[2]]$zn, 
                       upr = km_real[[1]][[1]][[3]]$zn)

ggplot(plotframe, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = plotframe, size = 3) +
  geom_ribbon(data = plotframe, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + 
  theme(text = element_text(size = 35)) + ylab("Coefficient") +
  labs(title = "True local coefficient for x2 is -3.")
