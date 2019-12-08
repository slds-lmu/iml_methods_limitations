#' @description Generating artificial LIME examples of a sinus shaped prediction surface
plot_lime = function(model_smoothness = 270, sample_seed, kernel_width = 900, sample_size = 10, ylab = "target") {
  
  # create ground truth
  black_box = function(x) sin(x / model_smoothness)
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
    geom_line(color = "#00C5CD", size = 2.5) +
    geom_line(data = NULL, aes(x = x, y = y_pred), color = "#e04d2e", size = 2) +
    geom_point(data = NULL, aes(x = x_samp, y = y_samp), size = 4) +
    geom_point(data = NULL, aes(x = x_ex, y = y_ex), color = "#c1c10d", size = 10) +
    geom_vline(aes(xintercept = x_ex - sqrt(kernel_width))) +
    geom_vline(aes(xintercept = x_ex + sqrt(kernel_width))) +
    theme_minimal() +
    theme(
      text = element_text(size = 30)
    ) +
    ylim(c(-1.5, 1.5)) +
    ylab(ylab) +
    xlab("feature")
  
}

#' @description Function creating a plot of means and standard deviation for each feature weight.
#' Probably outdated.
book_plot = function(means, sds, color1 = rgb(135/255, 150/255, 40/255), color2 = rgb(70/255, 95/255, 25/255), xlab = "Feature", ylab = "Weight", angle = 0, hjust = 0) {
  
  ggplot(data = NULL, aes(x = names(means), y = means)) +
    geom_bar(stat = "identity", fill = color1) +
    geom_errorbar(aes(ymin = means - sds, ymax = means + sds), color = color2, width = 0.4, size = 1.2, alpha = 0.7) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.title.x = element_text(vjust = -4),
      axis.text.x = element_text(angle = angle, hjust = hjust),
      plot.margin = ggplot2::margin(20,20,30,20)
    ) +
    xlab(xlab) +
    ylab(ylab)
}

#' @description Function creating a plot of means and upper / lower ci bounds for each feature weight.
presi_plot = function(means, lower, upper, color1 = rgb(135/255, 150/255, 40/255), color2 = rgb(70/255, 95/255, 25/255), xlab = "Feature", ylab = "Weight", angle = 0, hjust = 0, ylim = NULL, size = 25) {
  
  ggplot(data = NULL, aes(x = names(means), y = means, ymin = lower, ymax = upper)) +
    geom_errorbar(color = color1, width = 1L, size = 1.6) +
    geom_point(color = color2, size = 2L) +
    geom_hline(yintercept = 0L) +
    theme_minimal() +
    theme(
      text = element_text(size = size),
      axis.title.x = element_text(vjust = -4),
      axis.text.x = element_text(angle = angle, hjust = hjust),
      plot.margin = ggplot2::margin(20,20,30,20)
    ) +
    xlab(xlab) +
    ylab(ylab) +
    ylim(ylim)
}
