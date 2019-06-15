library(mlr)
# devtools::install_github('pkopper/lime')
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
mnist_2 = mnist_2[, sapply(mnist_2, var) != 0]
pts_to_predict = mnist_2[sample(nrow(mnist_2), 10), ]

result = permutation_growth(
  mnist_2, "y", pts_to_predict, type = "classif",
  dim_increment = 300,
  permutation_seq = c(100, 1000)
)

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


btask = makeRegrTask(data = boston[c("lstat", "medv")], target = "medv")
regr_model = makeLearner("regr.extraTrees", nodesize = 1, ntree = 1)
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


df <- data.frame(x_1, x_2, y)

# Define the task (mlr)
task <- makeRegrTask(data = df, target = "y")
# Define the learner (mlr)
learner <- makeLearner("regr.randomForest", ntree = 200)
# Train the model (mlr)
black_box <- train(learner, task)
# predict
task_pred = predict(black_box, task = task)
# Set up LIME explanations
explainer <- lime(df[, 1:2], black_box, bin_continuous = FALSE)
# New df, 130 is important
distance <- function(x) x[1, , drop = FALSE]
new_df <- data.frame(x_1 = 130, x_2 = 0, y = 400)
explanation <- explain(new_df[, 1:2, drop = FALSE], explainer, n_features = 1,
                       n_permutations = 10000, kernel_width = 2,
                       dist_fun = "euclidean")
as.data.frame(explanation)


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
