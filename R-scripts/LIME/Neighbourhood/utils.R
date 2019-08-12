#' Train-test splitting
#' 
#' This function performs a usual hold-out split.
#' @param data a data.frame (or data table or tibble) 
#' @param share a numeric value between 0 and 1. 
#' Indicates the share of the data used for training.
#' @param seed a numeric value. The random seed for splitting.
#' @return A list of two data frames where the first entry is the training data
#' and the second one the test data.
make_split <- function(data, share, seed = 100) {
  set.seed(seed)
  split <- sample(1:nrow(data), floor(share * nrow(data)))
  return(list(train = data[split, ], test = data[-split, ]))
}

#' Extraction of one local LIME models
#' 
#' This function extracts the coefficients of a LIME summary.
#' @param observation a data.frame with only one row.
#' @param explainer an explainer object from the lime package.
#' @param n_features a pos. integer value; the number of features for the LIME 
#' explainer.
#' @param n_permutations a pos. integer value; the number of permuations of the
#' LIME algorithm.
#' @param kernel_width a positive numeric value; ther kernel width of the LIME
#' algorithm
#' @param dist_fun a string indicating a valid distance function for the explain
#' function in the lime package.
#' @param feature_select a string indicating a valid feature selection strategy
#' as in the explain fucntion of the lime package.
#' @param ... additional arguments passed to the subfunctions 
#' (such as parallelisation)
#' @return a named vector with the local LIME coefficients.
extract_local_model <- function(observation, 
                                explainer, 
                                n_features, 
                                n_permutations, 
                                kernel_width,
                                dist_fun,
                                feature_select,
                                ...) {
  explanation <- explain(observation, explainer,
                         n_features = n_features, 
                         n_permutations = n_permutations, 
                         kernel_width = kernel_width, 
                         dist_fun = dist_fun,
                         feature_select = feature_select,
                         ...)
  n_col <- ncol(observation)
  sorted_col_names <- colnames(observation)[order(colnames(observation))]
  if (n_features < n_col) {
    coefs <- rep(0, n_col + 1)
    coefs[1] <- explanation$model_intercept[1]
    names(coefs)[1] <- "Intercept"
    names(coefs)[2:(n_col + 1)] <- sorted_col_names
    for (i in 2:(n_col + 1)) {
      if (sorted_col_names[i - 1] %in% explanation$feature) {
        coefs[i] <- explanation$feature_weight[explanation$feature == 
                                                 sorted_col_names[i - 1]]
      } else {
        coefs[i] <- NA
      }
    }
  } else {
    coefs <- rep(0, n_features + 1)
    coefs[1] <- explanation$model_intercept[1]
    names(coefs)[1] <- "Intercept"
    for (n in 1:n_features) {
      coefs[n + 1] <- explanation$feature_weight[n]
      names(coefs)[n + 1] <- explanation$feature[n]
    }
  }
  return(coefs)
}

#' Averaging the extractions of local LIME models
#' 
#' This function averages the extractions the coefficients of a LIME summary.
#' @param observation a data.frame with only one row.
#' @param explainer an explainer object from the lime package.
#' @param n_features a pos. integer value; the number of features for the LIME 
#' explainer.
#' @param n_permutations a pos. integer value; the number of permuations of the
#' LIME algorithm.
#' @param kernel_width a positive numeric value; ther kernel width of the LIME
#' algorithm
#' @param dist_fun a string indicating a valid distance function for the explain
#' function in the lime package.
#' @param iterations a pos. integer indicating the number of iterations over
#' which we want to average.
#' @param se boolean; should standard errors be computed?
#' @param seed numeric; the RNG seed.
#' @param parallel boolean; should the models be computed parrally? 
#' (Windows users should choose FALSE)
#' @param feature_select a string indicating a valid feature selection strategy
#' as in the explain fucntion of the lime package.
#' @param ... additional arguments passed to the subfunctions 
#' @return a named vector with the averaged local LIME coefficients.
extract_average_local_model <- function(observation, 
                                        explainer, 
                                        n_features, 
                                        n_permutations, 
                                        kernel_width,
                                        dist_fun,
                                        iterations,
                                        se,
                                        seed,
                                        feature_select = feature_select,
                                        parallel = TRUE,
                                        ...) {
  res <- vector(mode = "list", length = iterations)
  if (parallel) {
    res <- foreach(m = 1:iterations, .options.RNG = seed, 
                   .combine = bind_rows) %dorng% {
                     extract_local_model(observation, 
                                         explainer, 
                                         n_features, 
                                         n_permutations, 
                                         kernel_width,
                                         dist_fun = dist_fun,
                                         feature_select = feature_select, 
                                         ...)
                   }
  } else {
    set.seed(seed)
    for (i in 1:iterations) {
      res[[i]] <- extract_local_model(observation, 
                                      explainer, 
                                      n_features, 
                                      n_permutations, 
                                      kernel_width,
                                      dist_fun = dist_fun,
                                      feature_select = feature_select, 
                                      ...)
    }
  }
  means <- colMeans(res, na.rm = TRUE)
  NA_count <- 1 - (apply(is.na(res), 2, sum) / iterations)
  if (!se) {
    out <- means
  } else {
    sds <- sapply(res, sd, na.rm = TRUE)
    out <- list(means, sds)
  }
  if (n_features < ncol(observation)) {
    out <- list(out, NA_count)
  }
  out
}
#' Univariate analysis of the kernel width
#' 
#' This function averages the extractions the coefficients of LIME summaries
#' for a set od different kernel widths.
#' This function only works if there is one covariate.
#' @param kernel_widths a vector with positive numeric values; 
#' the set of kernel width which we want LIME explanations for.
#' @param observation a data.frame with only one row.
#' @param explainer an explainer object from the lime package.
#' @param n_features a pos. integer value; the number of features for the LIME 
#' explainer.
#' @param n_permutations a pos. integer value; the number of permuations of the
#' LIME algorithm.
#' @param dist_fun a string indicating a valid distance function for the explain
#' function in the lime package.
#' @param iterations a pos. integer indicating the number of iterations over
#' which we want to average.
#' @param se boolean; should standard errors be computed?
#' @param seed numeric; the RNG seed.
#' @return a data.frame: The columns represent the kernel widths and the rows
#' the averaged local LIME coefficients.
analyse_univariate_kernel_width <- function(kernel_widths, 
                                            observation, 
                                            explainer, 
                                            n_features, 
                                            n_permutations = 2500,
                                            dist_fun = "euclidean",
                                            iterations = 25,
                                            seed) {
  result <- rep(0, length(kernel_widths))
  result <- cbind(result, result)
  result <- as.data.frame(result)
  i = 0
  for (k in kernel_widths) {
    i <- i + 1
    local_model <- extract_average_local_model(observation,
                                               explainer,
                                               n_features = 1, 
                                               n_permutations,
                                               dist_fun, 
                                               kernel_width = k,
                                               seed = seed,
                                               feature_select = "auto")
    result[i, ] <- local_model[[1]][1:2]
  }
  result
}

#' Data simulation
#' 
#' This function simulates multivariate gaussian distributed features and a
#' target that is (non-)linearly affected by these.
#' @param n_obs a pos. integer; the number of observations to be sampled.
#' @param n_vars a pos. integer; the number of covariates to be sampled.
#' @param nonlinear_intervals a list that indicates the way non-linearity 
#' should be incorparted.
#' @param piece_wise_intervals a list that indicates the way non-linearity 
#' should be incorparted (by local coefficients).
#' @seed a pos. numeric value; the seed for RNG.
#' @mu a numeric vector; the expected value of the MVG distribution of all 
#' features.
#' @Sigma a pd. matrix that represents the covariance matrix corresponding to
#' mu.
#' @param true_coefficients a vector of length n_vars indicating the true 
#' (linear) of the coefficents.
#' @param inctercept a numeric value representing the intercept of the true 
#' model.
#' @param shock either a string or a numeric value; This is to create noise on
#' the target.
#' If "internal" the shock (variance) is internally determined; if a number than
#' this number serves as sd of a gaussian error
#' @return a data.frame with n_obs rows and n_vars + 1 columns.
simulate_data <- function(n_obs, n_vars, nonlinear_intervals = NULL, 
                          piece_wise_intervals = NULL,
                          seed, mu, Sigma, true_coefficients, intercept,
                          shock = "internal") {
  set.seed(seed)
  df <- mvrnorm(n = n_obs, mu, Sigma, tol = 1e-6, empirical = FALSE, 
                EISPACK = FALSE)
  if (is.null(nonlinear_intervals) & is.null(piece_wise_intervals)) {
    y_det <- cbind(1, df) %*% c(intercept, true_coefficients)
  } else if (!is.null(piece_wise_intervals)) {
    df_sim <- df
    for (i in 1:length(piece_wise_intervals)) {
      df_sim[df[, i] < piece_wise_intervals[[i]]$lower , i] <- 
        piece_wise_intervals[[i]]$lower
      df_sim[df[, i] > piece_wise_intervals[[i]]$upper , i] <- 
        piece_wise_intervals[[i]]$upper
    }
    y_det <- cbind(1, df_sim) %*% c(intercept, true_coefficients) 
  } else {
    y_partial <- vector(mode = "list", length = n_vars)
    for (j in 1:length(nonlinear_intervals)) {
      y_partial[[j]] <- rep(0, n_obs)
      if (!is.null(nonlinear_intervals[[j]])) {
        support_intercepts <- make_support_intercepts(nonlinear_intervals[[j]])
        for (i in 1:length(nonlinear_intervals[[j]]$coefs)) {
          int_min <- ifelse(i == 1, -Inf, nonlinear_intervals[[j]]$knots[i - 1])
          int_max <- ifelse(i == length(nonlinear_intervals[[j]]$coefs), Inf, 
                            nonlinear_intervals[[j]]$knots[i])
          y_partial[[j]] <- y_partial[[j]] + 
            ifelse(df[, j] > int_min & df[, j] < int_max, df[, j] * 
            nonlinear_intervals[[j]]$coefs[i] +
            support_intercepts[i], 0)
        }
      } else {
        y_partial[[j]] <- rep(0, n_obs)
      }
    }
    y_partial <- Reduce("+", y_partial)
    y_det <- y_partial + 
      cbind(1, df[, unlist(lapply(nonlinear_intervals, is.null))]) %*%
      c(intercept, true_coefficients[unlist(lapply(nonlinear_intervals, 
                                                   is.null))])
  }
  colnames(df) <- paste("x", as.character(1:n_vars), sep = "")
  if (shock == "internal") {
    y <- rnorm(n_obs, y_det, abs(y_det) / runif(n_obs, 1, 100))
  } else {
    y <- rnorm(n_obs, y_det, shock)
  }
  data.frame(y, df)
}

#' Support fucntion to guarantee continuity of non-linear assocations.
make_support_intercepts <- function(nonlinear_interval) {
  res <- rep(0, length(nonlinear_interval$coefs))
  for (i in 2:length(nonlinear_interval$coefs)) {
    knot_value <- nonlinear_interval$coefs[i - 1] * 
      nonlinear_interval$knots[i - 1]
    res[i] <- knot_value - nonlinear_interval$coefs[i] * 
      nonlinear_interval$knots[i - 1] + res[i - 1]
  }
  res
}

#' Multivariate analysis of the kernel width
#' 
#' This function averages the extractions the coefficients of LIME summaries
#' for a set od different kernel widths.
#' This function is for multivatriate problems.
#' @param kernel_widths a vector with positive numeric values; 
#' the set of kernel width which we want LIME explanations for.
#' @param observation a data.frame with only one row.
#' @param explainer an explainer object from the lime package.
#' @param n_features a pos. integer value; the number of features for the LIME 
#' explainer.
#' @param n_permutations a pos. integer value; the number of permuations of the
#' LIME algorithm.
#' @param dist_fun a string indicating a valid distance function for the explain
#' function in the lime package.
#' @param iterations a pos. integer indicating the number of iterations over
#' which we want to average.
#' @param ci boolean; should CIs be computed?
#' @param seed numeric; the RNG seed.
#' @param feature_select a string indicating the valid feature selection 
#' strategy of the explain function in the LIME package.
#' @param ... further arguments passed to the sub functions.
#' @return a list of data.frames: The columns represent the kernel widths and 
#' the rows the averaged local LIME coefficients or the lower and upper 
#' confidence intervals of the inclusion probabilites.
analyse_multivariate_kernel_width <- function(kernel_widths, 
                                              observation, 
                                              explainer, 
                                              n_features, 
                                              n_permutations,
                                              dist_fun = "euclidean",
                                              iterations = 50,
                                              ci = FALSE,
                                              seed, 
                                              feature_select = "auto",
                                              ...) {
  n_cores <- detectCores()
  registerDoParallel(n_cores - 1)
  if (!ci) {
    result <- matrix(0, ncol = ncol(observation) + 1, 
                     nrow = length(kernel_widths))
    result <- as.data.frame(result)
    NA_count <- result
    i <- 0
    for (k in kernel_widths){
      i <- i + 1
      local <- extract_average_local_model(observation,
                                           explainer,
                                           n_features, 
                                           n_permutations,
                                           dist_fun = dist_fun, 
                                           kernel_width = k,
                                           iterations = iterations,
                                           se = FALSE,
                                           seed = seed,
                                           feature_select = feature_select,
                                           ...)
      if (n_features < ncol(observation)) {
        local_model <- local[[1]]
        NA_count[i, ] <- local[[2]]
      } else {
        local_model <- local
      }
      result[i, ] <- local_model
    }
    col_names <- names(local_model)
    colnames(result) <- col_names
    result[is.na(result)] <- 0
  } else {
    result <- vector(mode = "list", length = 3)
    result[[1]] <- as.data.frame(matrix(0, ncol = ncol(observation) + 1, 
                                        nrow = length(kernel_widths)))
    result[[2]] <- as.data.frame(matrix(0, ncol = ncol(observation) + 1, 
                                        nrow = length(kernel_widths)))
    result[[3]] <- as.data.frame(matrix(0, ncol = ncol(observation) + 1, 
                                        nrow = length(kernel_widths)))
    NA_count <- result[[1]]
    i = 0
    for (k in kernel_widths) {
      i <- i + 1
      local <- extract_average_local_model(observation,
                                           explainer = explainer,
                                           n_features = n_features, 
                                           n_permutations = n_permutations,
                                           dist_fun = dist_fun, 
                                           kernel_width = k,
                                           iterations = iterations,
                                           se = TRUE,
                                           seed = seed,
                                           feature_select = feature_select,
                                           ...)
      if (n_features < ncol(observation)) {
        local_model <- local[[1]]
        NA_count[i, ] <- local[[2]]
      } else {
        local_model <- local
      }
      result[[1]][i, ] <- local_model[[1]]
      result[[2]][i, ] <- local_model[[1]] - 1.96 * local_model[[2]]
      result[[3]][i, ] <- local_model[[1]] + 1.96 * local_model[[2]]
    }
    result[[1]][is.na(result[[1]])] <- 0
    result[[2]][is.na(result[[2]])] <- 0
    result[[3]][is.na(result[[3]])] <- 0
    col_names <- names(local_model[[1]])
    colnames(result[[1]]) <- col_names
    colnames(result[[2]]) <- col_names
    colnames(result[[3]]) <- col_names
  } 
  if (n_features < ncol(observation)) {
    colnames(NA_count) <- col_names
    list(result, NA_count)
  } else {
    result
  }
}

#' Plotting the estimates of LIME explanations for different kernel sizes.
#' 
#' This function takes the estimated grid from 
#' analyse_multivariate_kernel_width and visualises the results.
#' This function only serves for simulated data.
#' @param kernel_matrix a matrix resulting from 
#' analyse_multivariate_kernel_width (without the SE) or 
#' analyse_univariate_kernel_width
#' @param kernel_width a vector with the same kernel width used to compute 
#' kernel_matrix
#' @param true_coefficients a vector indicating the true (local!!) coefficients 
#' (which we know).
#' @param title a string with an optional title.
#' @param ymin a numeric value, a lower bound for the plot. (y-axis)
#' @param ymax a numeric value, an upper bound for the plot. (y-axis)
#' @return a ggplot object (hence a plotted object) where the estimated 
#' coefficiencts of an explainer with a specific kernel width is plotted 
#' along a grid of kernel widths as specified in kernel_widths.
plot_kernels <- function(kernel_matrix, 
                         kernel_widths,
                         true_coefficients, 
                         title,
                         ymin,
                         ymax) {
  model_per_kernel <- cbind(kernel_widths, kernel_matrix)
  colnames(model_per_kernel) <- c("kernel", "intercept", 
                                  paste("x", 1:length(true_coefficients), 
                                        sep = ""))
  plot_frame <- cbind(rep(kernel_widths, length(true_coefficients)), 
                      melt(model_per_kernel[, 3:(length(
                        true_coefficients) + 2)]))
  plot_frame$value <- ifelse(plot_frame$value < ymin, ymin, plot_frame$value)
  plot_frame$value <- ifelse(plot_frame$value > ymax, ymax, plot_frame$value)
  colnames(plot_frame) <- c("kernel", "Feature", "coefficient")
  
  p <- ggplot(data = plot_frame, aes(y = coefficient, x = kernel, 
                                     group = Feature)) + 
    geom_line(aes(color = Feature), size = 2) + 
    geom_point(aes(color = Feature), size = 3) + 
    labs(x = "Kernel width", y = "Coefficient") +
    theme(text = element_text(size = 35)) +
    labs(title = title)
  add_this <- ""
  for (i in 1:length(true_coefficients)) {
    new <- paste(" + geom_path(colour = ", i, 
                 " + 1, stat = 'function', size = 1.5, ", 
                 "fun = function(x) true_coefficients[", i, "])", sep = "")
    add_this <- paste(add_this, new, sep = "") 
  }
  call_text <- paste("p", add_this)
  eval(parse(text = call_text))
}

plot_kernels_real <- function(kernel_matrix, 
                              kernel_widths,
                              global_coefficients, 
                              title,
                              ymin,
                              ymax) {
  model_per_kernel <- cbind(kernel_widths, kernel_matrix)
  colnames(model_per_kernel)[1] <- "kernel"
  plot_frame <- cbind(rep(kernel_widths, length(true_coefficients)), 
                      melt(model_per_kernel[, 3:(length(
                        true_coefficients) + 2)]))
  plot_frame$value <- ifelse(plot_frame$value < ymin, ymin, plot_frame$value)
  plot_frame$value <- ifelse(plot_frame$value > ymax, ymax, plot_frame$value)
  colnames(plot_frame) <- c("kernel", "Feature", "coefficient")
  
  p <- ggplot(data = plot_frame, aes(y = coefficient, x = kernel, 
                                     group = Feature)) + 
    geom_line(aes(color = Feature), size = 3) + 
    geom_point(aes(color = Feature), size = 3) + 
    labs(x = "Kernel width", y = "Coefficient") +
    theme(text = element_text(size = 35)) +
    labs(title = title)
  add_this <- ""
  for (i in 1:length(true_coefficients)) {
    new <- paste(" + geom_path(colour = ", i, 
                 " + 1, stat = 'function', size = 1.5, ", 
                 "fun = function(x) global_coefficients[", i, "])", sep = "")
    add_this <- paste(add_this, new, sep = "") 
  }
  call_text <- paste("p", add_this)
  eval(parse(text = call_text))
}

#' Plotting the the inclusion probabilites for different features of the LIME 
#' explanations for different kernel sizes.
#' 
#' This function takes the estimated grid from 
#' analyse_multivariate_kernel_width and visualises the results.
#' @kernel_width a vector with the same kernel width used to compute 
#' kernel_matrix
#' @param stability_paths a matrix featuring inclusion probabilites  resulting 
#' from analyse_multivariate_kernel_width
#' @param max_kernel numeric value; what is the maximum kernel width to be 
#' plotted.
#' @param title a string with an optional title. (can be empty)
#' @return a ggplot object (hence a plotted object) where the estimated 
#' coefficiencts of an explainer with a specific kernel width is plotted 
#' along a grid of kernel widths as specified in kernel_widths.
plot_pseudo_stability_paths <- function(kernel_widths, stability_paths, 
                                        max_kernel = NULL, title = ""){
  stability_paths <- cbind(melt(stability_paths), kernel_widths)
  names(stability_paths) <- c("variable", "probality", "kernel")
  if (!(is.null(max_kernel))) {
    stability_paths <- stability_paths[stability_paths$kernel < max_kernel, ]
  }
  x <- stability_paths$kernel
  y <- stability_paths$probality
  variable <- stability_paths$variable
  p <- ggplot(data = stability_paths, aes(x = x, y = y, group = variable))
  p <- p + geom_line(aes(color = variable), size = 1.75) + 
    geom_point(aes(color = variable), size = 3) + 
    labs(x = "Kernel Width", y = expression(pi)) + labs(title = title) + 
    theme(text = element_text(size = 35))
  return(p)
}
