set.seed(1)
day <- read_csv("datasets/day.csv")
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day <- day[, c(3, 6, 8:10, 12:13, 16)]
day$season <- as.factor(day$season)
day$weathersit <- as.factor(day$weathersit)
day <- as.data.frame(day)
data_set <- make_split(day, 0.85)

##################### rf ############################

task <- makeRegrTask(data = data_set$train, target = "cnt")
### Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 100)
### Train the model (mlr)
black_box <- train(learner, task)
### predict
task_pred <- predict(black_box, newdata = data_set$test)

### resampling
crossval(learner, task, iters = 10, measures = list(mae, mse))

# fit not as good as before but not that important because we don't know truth.
ggplot(data = task_pred$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)

explainer <- lime(data_set$train[, 1:7], black_box, bin_continuous = FALSE,
                  use_density = FALSE)

kernel_widths <- c(seq(0.07, 0.15, 0.02), seq(0.275, 1, 0.075),
                   seq(1.15, 1.75, 0.15), seq(1.75, 5.75, 1), 20)

km_real <- vector(mode = "list", length = 10)
set.seed(5)
examples <- sample(1:nrow(data_set$test), 10)
for (i in 1:length(km_real)) {
  cur <- examples[i]
  km_real[[i]] <- try(analyse_multivariate_kernel_width(kernel_widths,
                                                        data_set$test[cur, 1:7], 
                                                        explainer,
                                                        n_features = 3, 
                                                        n_permutations = 6500, 
                                                        dist_fun = "euclidean",
                                                        seed = 1,
                                                        ci = TRUE,
                                                        feature_select = 
                                                          "auto",
                                                        iterations = 48))
}

saveRDS(km_real, file = paste("R-results/LIME/Neighbourhood/kernelmatrix",
                              "-bike-randomforest.RDS", sep = ""))
saveRDS(kernel_widths, file = "R-results/LIME/Neighbourhood/kw_real.RDS")
