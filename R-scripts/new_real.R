library(readr)
day <- read_csv("datasets/day.csv")
as.data.frame(day)
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day <- day[, c(3, 6, 8:13, 16)]
#lut <- c("1" = "Winter",
 #        "2" = "Spring",
  #       "3" = "Summer",
   #      "4" = "Autumn")
#day$season <- lut[day$season]
day$season <- as.factor(day$season)
#lut <- c("1" = "sunny",
 #        "2" = "overcast",
  #       "3" = "rain",
   #      "4" = "storm")
#day$weathersit <- lut[day$weathersit]
day$weathersit <- as.factor(day$weathersit)
library(data.table)
library(mltools)
#day <- one_hot(as.data.table(day))
day <- as.data.frame(day)
data_set <- make_split(day, 0.85)

#data_set <- data_set[[1]]
#scales <- data_set[[2]]
### Define the task (mlr)
task <- makeRegrTask(data = data_set$train, target = "cnt")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 200)
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
explainer <- lime(data_set$train[, 1:8], black_box, bin_continuous = FALSE,
                  use_density = FALSE)
a <- as.data.frame(explain(data_set$test[3, 1:8], explainer, n_features = 3, 
                           n_permutations = 25000, kernel_width = 100, 
                           dist_fun = "euclidean",
                           feature_select = "auto"))

)
set.seed(110101)
stability_paths <- get_stability_paths(model, data_set$train, reps = 100)
stability_paths <- stability_paths[, order(colnames(stability_paths))]
plot_stability_paths(stability_paths)

kernel_widths <- c(seq(0.07, 0.15, 0.02), seq(0.275, 1, 0.075),
                   seq(1.15, 1.75, 0.15), seq(1.75, 5.75, 1), 20)

km_real3 <- vector(mode = "list", length = 5)
a <- Sys.time()
for (i in 1:length(km_real3)) {
  km_real3[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[i, 1:8], 
                                                        explainer,
                                                        n_features = 4, 
                                                        n_permutations = 9000, 
                                                        dist_fun = "euclidean",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "auto",
                                                        iterations = 48))
}
b <- Sys.time()
for (i in 1:length(km_real3)) {
  print(plot_pseudo_stability_paths(kernel_widths, 
                            stability_paths = km_real3[[i]][[2]][, 2:8],
                            2, title = as.character(i)))
}
2 ist mit bins 3 ohne
