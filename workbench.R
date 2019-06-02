library(mlr)
library(lime)
library(BBmisc)

# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('mnist/train-images-idx3-ubyte')
  test <<- load_image_file('mnist/t10k-images-idx3-ubyte')
  
  train$y <<- load_label_file('mnist/train-labels-idx1-ubyte')
  test$y <<- load_label_file('mnist/t10k-labels-idx1-ubyte')  
}

load_mnist()



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

library(lime)
library(mlr)

# index of data point we want to explain
to_explain  = 1

# create task and calculate black box model
tsk_iris    = makeClassifTask(data = iris[-to_explain, ], target = "Species")
learner     = makeLearner("classif.randomForest", ntree = 200, predict.type = "prob")
black_box   = train(learner, tsk_iris)

# use lime to explain new data point
explainer   = lime(iris[-to_explain, 1:4], black_box)
explanation = explain(iris[to_explain, 1:4],
                      explainer,
                      n_labels = 1,
                      n_features = 4)

plot_features(explanation)
