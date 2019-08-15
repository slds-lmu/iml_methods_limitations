### OUTLOOK EXPLORATION


plot_better_lime = function(model_smoothness = 50, sample_seed, kernel_width = 900, sample_size = 10) {
  
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
  x_samp = rnorm(sample_size, x_ex, sqrt(kernel_width))
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
    ylim(c(-1.5, 1.5)) +
    ylab("target") +
    xlab("feature")
  
}


# EVALUATE OTHER SCRIPTS FIRST
# this function is corrupted and only works when executed in the global script order
plot_better_lime_boston = function(model_smoothness = 50, sample_seed, kernel_width = 900, sample_size = 10) {
  
  # create grid
  x_grid     = 1:4000 / 100
  y_grid     = predict(black_box, newdata = data.frame(lstat=x_grid))
  
  set.seed(1)
  # randomly pick data point to explain
  x_ex = runif(1, 1, 40)
  yret = predict(black_box, newdata = data.frame(lstat = x_ex))
  y_ex = yret$data$response
  
  set.seed(sample_seed)
  # sample new data points
  x_samp = rnorm(sample_size, x_ex, sqrt(kernel_width))
  y_samp = predict(black_box, newdata = data.frame(lstat = x_samp))
  data   = data.frame(x = x_samp, y = y_samp$data$response)
  
  # fit surrogate model and get predictions
  model  = lm(y ~ x, data = data)
  y_pred = predict(model, newdata = data.frame(x = x_grid))
  
  # visualize everything
  ggplot(data = NULL, aes(x = x_grid, y = y_grid$data$response)) +
    geom_line(color = "#00C5CD", size = 1.5) +
    geom_point(data = data, aes(x = x, y = y)) +
    geom_line( data = NULL, aes(x = x_grid, y = y_pred), color = "#e04d2e", size = 1) +
    geom_point(data = NULL, aes(x = x_ex,   y = y_ex  ), color = "#c1c10d", size = 3) +
    geom_vline(aes(xintercept = x_ex - sqrt(kernel_width))) +
    geom_vline(aes(xintercept = x_ex + sqrt(kernel_width))) +
    theme_minimal() +
    theme(
      text = element_text(size = 15),
      axis.title.x = element_text(vjust = -4),
      plot.margin = ggplot2::margin(20,20,30,20)
    ) +
    ylim(c(0, 50)) +
    ylab("target") +
    xlab("feature")
  
}


filename = paste0("images/boston_sampled_tree_fst_better.png")
png(filename, width = 700L, height = 500L)
plot_better_lime_boston(sample_seed = 1, kernel_width = 1)
dev.off()

filename = paste0("images/boston_sampled_tree_snd_better.png")
png(filename, width = 700L, height = 500L)
plot_better_lime_boston(sample_seed = 2, kernel_width = 1)
dev.off()

filename = paste0("images/boston_sampled_tree_snd_better_slim.png")
png(filename, width = 700L, height = 500L)
plot_better_lime_boston(sample_seed = 2, kernel_width = 0.01)
dev.off()

filename = paste0("images/boston_sampled_tree_snd_slim.png")
png(filename, width = 700L, height = 500L)
plot_lime_boston(sample_seed = 2, kernel_width = 0.01)
dev.off()
