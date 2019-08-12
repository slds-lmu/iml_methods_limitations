
make_split <- function(data, share, seed = 100) {
  set.seed(seed)
  split <- sample(1:nrow(data), floor(share * nrow(data)))
  return(list(train = data[split, ], test = data[-split, ]))
}

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


extract_local_models <- function(observations, 
                                 explainer, 
                                 n_features, 
                                 n_permutations, 
                                 kernel_width,
                                 dist_fun,
                                 feature_select,
                                 ...) {
  explanations <- explain(observations, explainer,
                          n_features = n_features, 
                          n_permutations = n_permutations, 
                          kernel_width = kernel_width, 
                          dist_fun = dist_fun,
                          feature_select = feature_select,
                          ...)
  n_col <- ncol(observations)
  n_row <- nrow(observations)
  sorted_col_names <- colnames(observations)[order(colnames(observations))]
  coefs <- as.data.frame(matrix(NA, ncol = n_col + 1, nrow = n_row))
  coefs[, 1] <- explanations$model_intercept[seq(1, n_row * n_features, 
                                                 by = n_features)]
  colnames(coefs)[1] <- "Intercept"
  colnames(coefs)[2:(n_col + 1)] <- sorted_col_names
  for (i in 2:(n_col + 1)) {
    coefs[rep(1:n_row, each = n_features)[explanations$feature == 
                                            sorted_col_names[i - 1]], i] <- 
      explanations$feature_weight[explanations$feature == 
                                    sorted_col_names[i - 1]]
  }
return(coefs)
}

extract_average_local_model <- function(observation, 
                                        explainer, 
                                        n_features, 
                                        n_permutations, 
                                        kernel_width,
                                        dist_fun,
                                        iterations = 25,
                                        se = FALSE,
                                        seed,
                                        feature_select = feature_select,
                                        ...) {
  res <- vector(mode = "list", length = iterations)
  if (parallel = TRUE) {
    res <- foreach(m = 1:iterations, .options.RNG = seed, 
                   .combine = bind_rows) %dorng% {
                     extract_local_model(observations, 
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
      res[[i]] <- extract_local_model(observations, 
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

extract_average_local_models <- function(observations, 
                                        explainer, 
                                        n_features, 
                                        n_permutations, 
                                        kernel_width,
                                        dist_fun,
                                        iterations,
                                        se = FALSE,
                                        seed,
                                        feature_select = feature_select,
                                        ...) {
  res <- vector(mode = "list", length = iterations)
  res <- foreach(m = 1:iterations, .options.RNG = seed, 
                 .combine = bind_rows) %dorng% {
                   extract_local_models(observations, 
                                       explainer, 
                                       n_features, 
                                       n_permutations, 
                                       kernel_width,
                                       dist_fun = dist_fun,
                                       feature_select = feature_select, 
                                       ...)
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

analyse_univariate_kernel_width <- function(kernel_widths, 
                                            observation, 
                                            explainer, 
                                            n_features, 
                                            n_permutations = 2500,
                                            dist_fun = "euclidean",
                                            iterations,
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

scale_data <- function(data_set) {
  means <- colMeans(data_set$train)
  sds <- apply(data_set$train, 2, sd)
  for (i in 1:ncol(data_set$train)) {
    data_set$train[ , i] <- (data_set$train[ , i] - means[i]) / sds[i]
    data_set$test[ , i] <- (data_set$test[ , i] - means[i]) / sds[i]
  }
  return(list(data_set, c(means = means, sds = sds)))
}

rank_predictions <- function(task_pred) {
  mse_cont <- (task_pred$data$response - task_pred$data$truth) ^ 2
  names(mse_cont) <- rownames(task_pred$data)
  mse_cont[order(mse_cont)]
}
