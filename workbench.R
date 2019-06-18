library(mlr)
# devtools::install_github('pkopper/lime')
# devtools::install_github('thomasp85/lime')
library(lime)
library(BBmisc)
library(mlbench)
library(ggplot2)
source("helpers.R", local = TRUE)

# create "mnist" object in
load_mnist()

set.seed(5)
mnist_2 = mnist[sample(10000), ]
#mnist_2 = mnist[mnist$y %in% c(0,8), ]
mnist_2$y[mnist_2$y %in% 0:4] = 0L
mnist_2$y[mnist_2$y %in% 5:9] = 1L
# remove variables with no variance
mnist_2 = mnist_2[, sapply(mnist_2, var) != 0]
# remove variables where >= 99% entries are equal
mnist_2 = mnist_2[, sapply(mnist_2, function(x) max(table(x))/length(x)) < 0.99]
pts_to_predict = mnist_2[sample(nrow(mnist_2), 10), ]

result = permutation_growth(
  mnist_2, "y", pts_to_predict, type = "classif",
  dim_increment = 300,
  permutation_seq = c(100, 1000)
)
saveRDS(result, "perm_300_100_1000")

result = feature_growth(
  mnist_2, "y", pts_to_predict, type = "classif",
  dim_increment = 300
)
saveRDS(result, "feat_300")

data(BostonHousing)

set.seed(5)
boston = BostonHousing[sample(nrow(BostonHousing), 20), ]
boston = boston[!duplicated(boston$lstat), ]#c("lstat", "medv")]

P = 10
bostp = as.data.frame(sapply(1:P, function(i) (boston$lstat^i)))
names(bostp) = paste0("lstat_", 1:P)
bostp$medv = boston$medv
btask <- makeRegrTask(data = bostp, target = "medv")
regr_model <- makeLearner("regr.lm")
black_box <- mlr::train(regr_model, btask)
x_grid = 1:4000 / 100
x_feat = as.data.frame(sapply(1:P, function(i) (x_grid^i)))
names(x_feat) = paste0("lstat_", 1:P)
y_pred = predict(black_box, newdata = x_feat)




#model = lm(data = boston, medv ~ poly(lstat, 15) + crim)
#class(model) = c(class(model), "lime_regressor")
#model = caret::train(x = boston[c("lstat")], y = boston[["medv"]], method = "lm")
explainer = lime(boston[c("lstat")], black_box, bin_continuous = FALSE, use_density = FALSE)

explanation = explain(
  data.frame(lstat=boston[c("lstat")][1,]),
  explainer,
  n_labels = 1L,
  dist_fun = "euclidean",
  n_features = 1L#,
#  n_permutations = 10L
)


data(BostonHousing)

set.seed(5)
boston = BostonHousing[sample(nrow(BostonHousing), 20), ]
boston = boston[!duplicated(boston$lstat), ]#c("lstat", "medv")]

pts_to_predict = BostonHousing[sample(nrow(BostonHousing), 10), c("lstat", "medv")]
result = permutation_growth(
  boston, "medv", pts_to_predict,
  "lstat", type = "regr",
  permutation_seq = c(100,1000)
)

### First simulation
set.seed(1)
x_1 <- rnorm(300, 100, 42)
x_2 <- rnorm(300, 10, 10)
y <- (300 * sin(0.0002 * x_1^2) + 0.5 * x_1 + 250 + rnorm(300, 0, 70))
plot(x_1, y)



to_explain  = iris[ 1, 1:4]
train_set   = iris[-1, ]
# create task and calculate black box model
task_iris   = makeClassifTask(data = train_set, target = "Species")
learner     = makeLearner("classif.randomForest", ntree = 200, predict.type = "prob")
black_box   = train(learner, task_iris)
# use lime to explain new data point
explainer   = lime(train_set[, 1:4], black_box)
explanation = explain(to_explain,
                      explainer,
                      n_labels = 1,
                      n_features = 2)

# index of data point we want to explain
to_explain  = 1
mnist_2 = mnist[sample(10000), ]
#mnist_2 = mnist[mnist$y %in% c(0,8), ]
mnist_2$y[mnist_2$y %in% 0:4] = 0L
mnist_2$y[mnist_2$y %in% 5:9] = 1L
mnist_2 = mnist_2[, sapply(mnist_2, var) != 0]

M = cor(mnist_2[538:619])
corrplot::corrplot(M)
# create task and calculate black box model
task_mnist    = makeClassifTask(data = mnist_2[-to_explain, ], target = "y")
learner     = makeLearner("classif.randomForest", ntree = 20, predict.type = "prob")
#rdesc = makeResampleDesc("CV", iters = 3)
#r = resample(learner, task_mnist, rdesc)
# 1v2 - 0.008; 4v9 - 0.018; 1v8 - 0.008; 0:4v5:9 - 0.034 (60000);
# 0:4v5:9 - 0.04 (30000); 0:4v5:9 - 0.06 (10000);
black_box   = train(learner, task_mnist)
sum(black_box$learner.model$importance != 0)
# 1v2 - 440; 4v9 - 481; 1v8 - 406; 0:4v5:9 - 577
plot(
  black_box$learner.model$importance[order(black_box$learner.model$importance, decreasing=T)][1:50]
) # 0:4v5:9 > 1v2 > 1v8 > 4v9
# use lime to explain new data point
explainer   = lime(mnist_2[-to_explain, 1:(ncol(mnist_2)-1)], black_box)
explanation1 = explain(mnist_2[to_explain, 1:(ncol(mnist_2)-1)],
                       explainer,
                       n_labels = 1,
                       n_features = ncol(mnist_2)-1)

explanation2 = explain(mnist_2[to_explain, 1:(ncol(mnist_2)-1)],
                      explainer,
                      n_labels = 1,
                      n_features = ncol(mnist_2)-1)

explanation3 = explain(mnist_2[to_explain, 1:(ncol(mnist_2)-1)],
                       explainer,
                       n_labels = 1,
                       n_features = ncol(mnist_2)-1)

plot_features(explanation3)

mean(abs(
  c(explanation1$feature_weight - explanation2$feature_weight,
    explanation1$feature_weight - explanation3$feature_weight,
    explanation2$feature_weight - explanation3$feature_weight)
)) 
# 1v2 - 0.0021; 04v59 - 0.0034


result1 = feature_growth(iris, "Species", dim_increment = 1)
result2 = permutation_growth(iris, "Species", dim_increment = 1)


data(BostonHousing)

set.seed(5)
boston = BostonHousing[sample(nrow(BostonHousing), 20), ]
boston = boston[!duplicated(boston$lstat), ]#c("lstat", "medv")]

btask = makeRegrTask(data = boston[c("lstat", "medv")], target = "medv")
regr_model = makeLearner("regr.ranger", min.node.size = 1, num.trees = 1)
#regr_model = makeLearner("regr.svm", degree = 200, cost = 500)
black_box <- mlr::train(regr_model, btask)
x_grid = 1:4000 / 100
y_pred = predict(black_box, newdata = data.frame(lstat=x_grid))

#model <- lm(data = boston, medv ~ poly(lstat, P))
#x_grid <- 1:4000 / 100
#y_pred <- predict.lm(model, newdata = data.frame(lstat = x_grid))

ggplot(data = boston, aes(y = medv, x = lstat)) +
  geom_point() +
  geom_line(data = data.frame(x_grid, y_pred=y_pred$data$response), aes(x = x_grid, y = y_pred)) +
  ylim(c(0,50))

#
plot_lime = function(model_stability = 50, sample_seed, kernel_width = 900, sample_size = 10) {
  
  # create ground truth
  black_box = function(x) sin(x / model_stability)
  x = 1:1000
  y = black_box(x)

  set.seed(1)
  # randomly pick data point to explain
  x_ex = runif(1, 1, 1000)
  y_ex = black_box(x_ex)
  
  set.seed(sample_seed)
  # sample new data points
  x_samp = runif(sample_size, 1, 1000)
  y_samp = black_box(x_samp)
  data   = data.frame(x = x_samp, y = y_samp)
  
  # apply gaussian kernel to receive weights
  weights = exp( - (x_samp - x_ex)^2 / kernel_width )
  
  # fit surrogate model and get predictions
  model  = lm(y ~ x, data = data, weights = weights)
  y_pred = predict(model, newdata = data.frame(x = x))
  
  # visualize everything
  ggplot(data = NULL, aes(y = y, x = x)) +
    geom_line(color = "#00C5CD", size = 1.5) +
    geom_point(data = NULL, aes(x = x_samp, y = y_samp)) +
    geom_line(data = NULL, aes(x = x, y = y_pred), color = "#e04d2e", size = 1) +
    geom_point(data = NULL, aes(x = x_ex, y = y_ex), color = "#c1c10d", size = 3) +
    geom_vline(aes(xintercept = x_ex - sqrt(kernel_width))) +
    geom_vline(aes(xintercept = x_ex + sqrt(kernel_width))) +
    theme_minimal() +
    ylim(c(-1.5, 1.5))

}

plot_lime(sample_seed = 2, model_stability = 200, kernel_width = 900)
plot_lime(sample_seed = 1, model_stability = 200, kernel_width = 900)
plot_lime(sample_seed = 2, model_stability = 50, kernel_width = 900)
plot_lime(sample_seed = 1, model_stability = 50, kernel_width = 900)
plot_lime(sample_seed = 2, model_stability = 50, kernel_width = 900, sample_size = 100)
plot_lime(sample_seed = 1, model_stability = 50, kernel_width = 900, sample_size = 100)
plot_lime(sample_seed = 2, model_stability = 50, kernel_width = 9000)
plot_lime(sample_seed = 1, model_stability = 50, kernel_width = 9000)


plot_better_lime = function(model_stability = 50, sample_seed, kernel_width = 900, sample_size = 10) {
  
  # create ground truth
  black_box = function(x) sin(x / model_stability)
  x = 1:1000
  y = black_box(x)
  
  set.seed(1)
  # randomly pick data point to explain
  x_ex = runif(1, 1, 1000)
  y_ex = black_box(x_ex)
  
  
  set.seed(sample_seed)
  # sample new data points
  x_samp = rnorm(size, x_ex, sqrt(kernel_width))
  y_samp = black_box(x_samp)
  data   = data.frame(x = x_samp, y = y_samp)
  
  # fit surrogate model and get predictions
  model  = lm(y ~ x, data = data)
  y_pred = predict(model, newdata = data.frame(x = x))
  
  # visualize everything
  ggplot(data = NULL, aes(y = y, x = x)) +
    geom_line(color = "#00C5CD", size = 1.5) +
    geom_point(data = NULL, aes(x = x_samp, y = y_samp)) +
    geom_line(data = NULL, aes(x = x, y = y_pred), color = "#e04d2e", size = 1) +
    geom_point(data = NULL, aes(x = x_ex, y = y_ex), color = "#c1c10d", size = 3) +
    geom_vline(aes(xintercept = x_ex - sqrt(kernel_width))) +
    geom_vline(aes(xintercept = x_ex + sqrt(kernel_width))) +
    theme_minimal() +
    ylim(c(-1.5, 1.5))
  
}

plot_better_lime(sample_seed = 2, model_stability = 50, kernel_width = 900)
plot_better_lime(sample_seed = 1, model_stability = 50, kernel_width = 900)
plot_better_lime(sample_seed = 2, model_stability = 50, kernel_width = 9000)
plot_better_lime(sample_seed = 1, model_stability = 50, kernel_width = 9000)
