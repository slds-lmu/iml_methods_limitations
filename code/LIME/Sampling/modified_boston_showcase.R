# CREATE MODIFIED BOSTON EXAMPLE

data(BostonHousing)
set.seed(5)
boston = BostonHousing[sample(nrow(BostonHousing), 20), ]

# create task and leaner
btask      = makeRegrTask(data = boston[c("lstat", "medv")], target = "medv")
regr_model = makeLearner("regr.xgboost", nrounds = 50)#, min.node.size = 1, num.trees = 1)#makeLearner("regr.ranger", min.node.size = 1, num.trees = 1)
# run model and get prediction surface
black_box  = train(regr_model, btask)
x_grid     = 1:4000 / 100
y_pred     = predict(black_box, newdata = data.frame(lstat=x_grid))

# visualize results
plot = ggplot() +
  geom_line(
    data = data.frame(x_grid, y_pred=y_pred$data$response),
    aes(x = x_grid, y = y_pred),
    color = "#00C5CD",
    size  = 2.5
  ) +
  geom_point(data = boston, aes(y = medv, x = lstat), size = 4L) +
  ylim(c(0, 50)) +
  theme_minimal() +
  theme(
    text = element_text(size = 30L),
    axis.title.x = element_text(vjust = -4),
    plot.margin = ggplot2::margin(20,20,30,20)
  ) +
  ylab("target (medv)") +
  xlab("feature (lstat)")

filename = paste0("images/boston_sampled_tree_presi.png")
png(filename, width = 700L, height = 500L)
plot
dev.off()


# EVALUATE ABOVE FIRST
# this function is badly written, but we don't care because we only need it right here right now
plot_lime_boston = function(model_smoothness = 50, sample_seed, kernel_width = 25, sample_size = 10) {
  
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
  x_samp = runif(sample_size, 1, 40)
  y_samp = predict(black_box, newdata = data.frame(lstat = x_samp))
  data   = data.frame(x = x_samp, y = y_samp$data$response)
  
  # apply gaussian kernel to receive weights
  weights = exp( - (x_samp - x_ex)^2 / kernel_width )
  
  # fit surrogate model and get predictions
  model  = lm(y ~ x, data = data, weights = weights)
  y_pred = predict(model, newdata = data.frame(x = x_grid))
  
  # visualize everything
  ggplot(data = NULL, aes(x = x_grid, y = y_grid$data$response)) +
    geom_line(color = "#00C5CD", size = 2.5) +
    geom_point(data = data, aes(x = x, y = y), size = 4) +
    geom_line( data = NULL, aes(x = x_grid, y = y_pred), color = "#e04d2e", size = 2) +
    geom_point(data = NULL, aes(x = x_ex,   y = y_ex  ), color = "#c1c10d", size = 10) +
    geom_vline(aes(xintercept = x_ex - sqrt(kernel_width))) +
    geom_vline(aes(xintercept = x_ex + sqrt(kernel_width))) +
    theme_minimal() +
    theme(
      text = element_text(size = 30L),
      axis.title.x = element_text(vjust = -4),
      plot.margin = ggplot2::margin(20,20,30,20)
    ) +
    ylim(c(0, 50)) +
    ylab("") +
    xlab("feature")
  
}

filename = paste0("images/boston_sampled_tree_1_presi.png")
png(filename, width = 700L, height = 500L)
plot2 = plot_lime_boston(sample_seed = 1, kernel_width = 1)
dev.off()


filename = paste0("images/boston_sampled_tree_presi3.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot, plot2, nrow = 1)
dev.off()
