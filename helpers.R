# Load the MNIST digit recognition dataset into R
# http://yann.lecun.com/exdb/mnist/
# assume you have all 4 files and gunzip'd them
# creates train$n, train$x, train$y  and test$n, test$x, test$y
# e.g. train$x is a 60000 x 784 matrix, each row is one digit (28x28)
# call:  show_digit(train$x[5,])   to see a digit.
# brendan o'connor - gist.github.com/39760 - anyall.org

load_mnist = function() {
  load_image_file = function(filename) {
    ret = list()
    f   = file(filename, 'rb')
    readBin(f, 'integer', n=1, size=4, endian='big')
    ret$n = readBin(f, 'integer', n=1, size=4, endian='big')
    nrow  = readBin(f, 'integer', n=1, size=4, endian='big')
    ncol  = readBin(f, 'integer', n=1, size=4, endian='big')
    x     = readBin(f, 'integer', n=ret$n*nrow*ncol, size=1, signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file = function(filename) {
    f = file(filename, 'rb')
    readBin(f, 'integer', n=1, size=4, endian='big')
    n = readBin(f, 'integer', n=1, size=4, endian='big')
    y = readBin(f, 'integer', n=n, size=1, signed=F)
    close(f)
    y
  }
  mnist <<- as.data.frame(load_image_file('mnist/train-images-idx3-ubyte')$x)
  mtest <<- as.data.frame(load_image_file('mnist/t10k-images-idx3-ubyte')$x)
  
  mnist$y <<- load_label_file('mnist/train-labels-idx1-ubyte')
  mtest$y <<- load_label_file('mnist/t10k-labels-idx1-ubyte')  
}


#' @return dataframe of feature weights
#' @example result = feature_growth(iris, "Species", dim_increment = 1)
feature_growth <- function(data, target, repeats = 10L, dim_increment = 10L, seed = 123) {
  
  set.seed(seed)
  
  # dimension of feature space
  p_max = ncol(data) - 1L
  feature_names = names(data[names(data) != target])

  # move target variable to the end
  data_sort           = data[names(data) != target]
  data_sort[[target]] = data[[target]]

  # iterate over amount of feature dimensions
  outer_return = lapply(
    seq(2L, p_max, by = dim_increment),
    function(p) {
      # sample point to remove selection-bias
      to_explain = sample(nrow(data), 1L)
      train_data = data_sort[-to_explain, 1L:p]
      target_pt  = data_sort[ to_explain, 1L:p]
      
      train_data[[target]] = data_sort[[target]][-to_explain]
      
      task      = makeClassifTask(data = train_data, target = target)
      learner   = makeLearner("classif.randomForest", ntree = 20L, predict.type = "prob")
      black_box = train(learner, task)
      explainer = lime(train_data[1L:p], black_box)
      
      # create sequence of "n_feature" arguments
      n_feat_seq = seq(1L, p, by = dim_increment)
      n_feat_seq = rep(n_feat_seq, each = repeats)
      
      # iterate over sequence of "n_feature" arguments
      inner_return = sapply(
        n_feat_seq,
        function(n_features) {
          
          feat_return        = rep(NA, p_max)
          names(feat_return) = feature_names
          
          explanation = explain(
            target_pt,
            explainer,
            n_labels = 1L,
            n_features = n_features
          )
          
          to_update = names(feat_return) %in% explanation$feature
          feat_return[to_update] = explanation$feature_weight
          
          c(
            p = p,
            n_features = n_features,
            feat_return
          )
          
        }
      )
      # output progress
      log = sprintf("%2.2f/1.00 done", (p-1)/(p_max-1))
      print(log)
      # transpose matrix and transform to dataframe
      as.data.frame(t(inner_return))
    }
  )
  # concatenate dataframes
  data.table::rbindlist(outer_return)
}


permutation_growth = function(
  data,
  target,
  pts_to_predict,
  type,
  repeats = 10L,
  permutation_seq = c(2500L, 5000L, 10000L),
  dim_increment = 10L
  ) {
  
  # dimension of feature space
  p_max = ncol(data) - 1L
  feature_names = names(data[names(data) != target])
  
  # create sequence of "n_feature" arguments
  n_feat_seq = seq(1L, p_max, by = dim_increment)
  n_feat_seq = rep(n_feat_seq, each = repeats)
  
  # move target variable to the end
  train_data           = data[names(data) != target]
  train_data[[target]] = data[[target]]
  
  
  if (type == "classif") {
    task = makeClassifTask(data = train_data, target = target)
    learner = makeLearner("classif.randomForest", ntree = 20L, predict.type = "prob")
    
  } else {
    task = makeRegrTask(data = train_data, target = target)
    learner = makeLearner("regr.randomForest", ntree = 20L)
  }
  
  black_box = train(learner, task)
  explainer = lime(train_data[1L:p_max], black_box)
  
  
  # iterate over sequence of permutation amount
  outer_return = lapply(
    permutation_seq,
    function(n_permutations) {
      
      # iterate over sequence of "n_feature" arguments
      inner_return = sapply(
        n_feat_seq,
        function(n_features) {
          
          feat_return        = rep(NA, p_max)
          names(feat_return) = feature_names
          
          apply(
            pts_to_predict,
            MARGIN = 1,
            function(target_pt) {
              
              explanation = explain(
                as.data.frame(t(target_pt[1:p_max])),
                explainer,
                n_labels = 1L,
                n_features = p_max,
                n_permutations = n_permutations
              )
              
              to_update = names(feat_return) %in% explanation$feature
              feat_return[to_update] = explanation$feature_weight
              names(target_pt) = paste0("data_", feature_names)
              
              c(
                n_features = n_features,
                n_permutations = n_permutations,
                target_pt,
                feat_return
              )
            }
          )
        }
      )
      # output progress
      frac = which(n_permutations == permutation_seq) / length(permutation_seq)
      log = sprintf("%2.2f/1.00 done", frac)
      print(log)
      # transpose matrix and transform to dataframe
      as.data.frame(t(inner_return))
    }
  )
  # concatenate dataframes
  data.table::rbindlist(outer_return)
}


complexity_growth = function(
  data,
  target,
  pts_to_predict,
  covariable,
  repeats = 10L,
  seed = 123L,
  n_permutations = 10,
  max_degree = 15,
  repetitions = 10
) {
  
  set.seed(seed)
  
  pts_to_predict = apply(
    pts_to_predict,
    MARGIN = 2,
    function(col) rep(col, repetitions)
  )
  
  # dimension of feature space
  p_max = ncol(data) - 1L
  feature_names = names(data[names(data) != target])
  
  # move target variable to the end
  train_data           = data[names(data) != target]
  train_data[[target]] = data[[target]]
  
  # iterate over sequence of polynomial degrees
  outer_return = lapply(
    1:max_degree,
    function(degree) {

      model = lm(data = train_data, medv ~ poly(get(covariable), degree))
      class(model) = c(class(model), "lime_regressor")
      explainer = lime(train_data[1L:p_max], model)
      
      inner_return = apply(
        pts_to_predict,
        MARGIN = 1,
        function(target_pt) {
          
          feat_return        = rep(NA, p_max)
          names(feat_return) = feature_names

          explanation = explain(
            as.data.frame(t(target_pt[1:p_max])),
            explainer,
            n_labels = 1L,
            n_features = p_max,
            n_permutations = n_permutations
          )
          
          to_update = names(feat_return) %in% explanation$feature
          feat_return[to_update] = explanation$feature_weight
          names(target_pt) = paste0("data_", feature_names)
          
          c(
            degree = degree,
            n_permutations = n_permutations,
            target_pt,
            feat_return
          )
          
        }
      )
      # output progress
      frac = degree / max_degree
      log = sprintf("%2.2f/1.00 done", frac)
      print(log)
      # transpose matrix and transform to dataframe
      as.data.frame(t(inner_return))
    }
  )
  # concatenate dataframes
  data.table::rbindlist(outer_return)
}
