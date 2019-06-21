library(MASS)
make_split <- function(data, share) {
  split <- sample(1:nrow(data), floor(share * nrow(data)))
  return(list(train = data[split, ], test = data[-split, ]))
}

extract_local_model <- function(observation, 
                                explainer, 
                                n_features, 
                                n_permutations, 
                                kernel_width,
                                dist_fun = "euclidean") {
  observation <- as.data.frame(observation)
  explanation <- explain(observation, explainer,
                         n_features = n_features, 
                         n_permutations = n_permutations, 
                         kernel_width = kernel_width, 
                         dist_fun = dist_fun)
  coefs <- rep(0, n_features + 1)
  coefs[1] <- explanation$model_intercept[1]
  names(coefs)[1] <- "Intercept"
  for (i in 1:n_features) {
    coefs[i + 1] <- explanation$feature_weight[i]
    names(coefs)[i + 1] <- explanation$feature[i]
  }
  coefs
}

extract_average_local_model <- function(observation, 
                                        explainer, 
                                        n_features, 
                                        n_permutations, 
                                        kernel_width,
                                        dist_fun = "euclidean",
                                        iterations = 25,
                                        se = FALSE) {
  res <- vector(mode = "list", length = iterations)
  for (i in 1:iterations) {
    res[[i]] <- extract_local_model(observation, 
                                    explainer, 
                                    n_features, 
                                    n_permutations, 
                                    kernel_width,
                                    dist_fun = "euclidean")
  }
  means <- Reduce("+", res) / iterations
  if (!se) {
    means
  } else {
    sq_error <- res
    for (i in 1:length(res)) {
      sq_error[[i]] <- (res[[i]] - means) ^ 2
    }
    se <- sqrt(Reduce("+", sq_error) / iterations)
    list(means, se)
  }
}

analyse_univariate_kernel_width <- function(kernel_widths, 
                                            observation, 
                                            explainer, 
                                            n_features, 
                                            n_permutations,
                                            dist_fun = "euclidean",
                                            iterations = 25) {
  result <- rep(0, length(kernel_widths))
  result <- cbind(result, result)
  result <- as.data.frame(result)
  i = 0
  for (k in kernel_widths) {
    i <- i + 1
    local_model <- extract_average_local_model(observation,
                                               explainer,
                                               n_features = 1, 
                                               n_permutations = 2500,
                                               dist_fun = "euclidean", 
                                               kernel_width = k)
    result[i, ] <- local_model
  }
  result
}


simulate_data <- function(n_obs, n_vars, nonlinear = NULL, 
                          piece_wise_intervals = NULL,
                          seed, mu, Sigma, true_coefficients, intercept) {
  set.seed(seed)
  df <- mvrnorm(n = n_obs, mu, Sigma, tol = 1e-6, empirical = FALSE, 
                EISPACK = FALSE)
  if (is.null(nonlinear) & is.null(piece_wise_intervals)) {
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
    df_nl <- df
    df_nl[, nonlinear] <- cos(df[, nonlinear])
    true_coefficients[nonlinear] <- true_coefficients[nonlinear] * 5
    y_det <- df_nl %*% true_coefficients
  }
  colnames(df) <- paste("x", as.character(1:n_vars), sep = "")
  y <- rnorm(n_obs, y_det, abs(y_det) / runif(n_obs, 1, 100))
  data.frame(y, df)
}


analyse_multivariate_kernel_width <- function(kernel_widths, 
                                              observation, 
                                              explainer, 
                                              n_features, 
                                              n_permutations,
                                              dist_fun = "euclidean",
                                              iterations = 50,
                                              ci = FALSE) {
  if (!ci) {
    result <- matrix(0, ncol = n_features + 1, nrow = length(kernel_widths))
    result <- as.data.frame(result)
    i = 0
    for (k in kernel_widths) {
      i <- i + 1
      local_model <- extract_average_local_model(observation,
                                                 explainer,
                                                 n_features, 
                                                 n_permutations,
                                                 dist_fun = "euclidean", 
                                                 kernel_width = k,
                                                 iterations = iterations,
                                                 se = FALSE)
      result[i, ] <- local_model
    }
    result
  } else {
    result <- vector(mode = "list", length = 3)
    result[[1]] <- as.data.frame(matrix(0, ncol = n_features + 1, 
                                        nrow = length(kernel_widths)))
    result[[2]] <- as.data.frame(matrix(0, ncol = n_features + 1, 
                                        nrow = length(kernel_widths)))
    result[[3]] <- as.data.frame(matrix(0, ncol = n_features + 1, 
                                        nrow = length(kernel_widths)))
    
    i = 0
    for (k in kernel_widths) {
      i <- i + 1
      local_model <- extract_average_local_model(observation,
                                                 explainer,
                                                 n_features, 
                                                 n_permutations,
                                                 dist_fun = "euclidean", 
                                                 kernel_width = k,
                                                 iterations = iterations,
                                                 se = TRUE)
      result[[1]][i, ] <- local_model[[1]]
      result[[2]][i, ] <- local_model[[1]] - 1.96 * local_model[[2]]
      result[[3]][i, ] <- local_model[[1]] + 1.96 * local_model[[2]]
    }
    result
  }
}

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
    geom_line(aes(color = Feature), size = 3) + 
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
