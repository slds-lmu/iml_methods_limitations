source("R-scripts/packages.R")
source("R-scripts/utils.R")
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

panels <- vector(mode = "list", length = 10)
j <- 0
for (i in c(1, 3, 4, 5)) {
  j <- j + 1
  panels[[j]] <- plot_pseudo_stability_paths(kernel_widths, 
                                    stability_paths = km_real[[i]][[2]][, 2:8],
                                    4, title = paste("Observation", 
                                                     as.character(j)))
}

j <- 0
for (i in 1:length(km_real)) {
  j <- j + 1
  panels[[j]] <- plot_pseudo_stability_paths(kernel_widths, 
                                             stability_paths = 
                                      km_real[[i]][[2]][, 2:8],
                                             4, title = paste("Observation", 
                                                              as.character(j)))
}

for (i in 2:(length(km_real2) - 1)) {
  j <- j + 1
  panels[[j]] <- plot_pseudo_stability_paths(kernel_widths, 
                                    stability_paths = 
                                      km_real2[[i]][[2]][, 2:8],
                                    4, title = paste("Observation", 
                                                     as.character(j)))
}


png("04-09-14.png", width = 2000, height = 2000)
grid.arrange(panels[[1]], panels[[2]], panels[[3]], panels[[4]], panels[[6]],
             panels[[8]], nrow = 3)
dev.off()

png("04-09-14a.png", width = 1000, height = 848)
panels[[1]]
dev.off()

png("04-09-14b.png", width = 1000, height = 848)
panels[[2]]
dev.off()

png("04-09-14c.png", width = 1000, height = 848)
panels[[3]]
dev.off()

png("04-09-14d.png", width = 1000, height = 848)
panels[[4]]
dev.off()

png("04-09-14e.png", width = 1000, height = 848)
panels[[5]]
dev.off()

png("04-09-14f.png", width = 1000, height = 848)
panels[[6]]
dev.off()

png("04-09-14g.png", width = 1000, height = 848)
panels[[7]]
dev.off()

png("04-09-14h.png", width = 1000, height = 848)
panels[[8]]
dev.off()

png("04-09-14i.png", width = 1000, height = 848)
panels[[9]]
dev.off()

png("04-09-14j.png", width = 1000, height = 848)
panels[[10]]
dev.off()

saveRDS(km_real, file = "R-results/kernelmatrix-bike-randomforest.RDS")
saveRDS(kernel_widths, file = "R-results/kw_real.RDS")

data_set$test[examples, ]
