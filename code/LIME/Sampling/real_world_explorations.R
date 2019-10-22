data("BostonHousing", package = "mlbench")
# removing categorical feature
boston     = BostonHousing[, -4]
# normalizing standard deviation to make coefficients comparable
boston     = as.data.frame(lapply(boston, function(x) x/sd(x)))

plots = lapply(
  names(boston[-ncol(boston)]), function(feat) qplot(get(feat), medv, data = boston, ylab = "", xlab = feat) + theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    text = element_text(size = 20))
)
args = paste0("plots[[", 1:12, "]]", collapse = ", ")
grid_plot_func = paste0("gridExtra::grid.arrange(", args, ", nrow = 3)")

filename = paste0("images/boston_prezi.png")
png(filename, width = 700L, height = 500L)
eval(parse(text = grid_plot_func))
dev.off()

btask      = makeRegrTask(data = boston, target = "medv")

# normal non-continuous model
regr_model = makeLearner("regr.ranger")
black_box  = train(regr_model, btask)
explainer  = lime(boston[, -ncol(boston)], black_box, bin_continuous = FALSE, use_density = FALSE)


# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
set.seed(123)
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})

means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/boston_standard_presi.png")
png(filename, width = 700L, height = 500L)
plot1 = presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.4, 0.35), size = 30)
plot1
dev.off()



# pick max of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], max))
set.seed(123)
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})

means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/boston_outlier_presi.png")
png(filename, width = 700L, height = 500L)
plot2 = presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.4, 0.35), size = 30, ylab = "")
plot2
dev.off()


filename = paste0("images/boston_meanVSmax.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()


# switch to bins
explainer  = lime(boston[, -ncol(boston)], black_box)


# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
set.seed(123)
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/boston_bins_presi.png")
png(filename, width = 700L, height = 500L)
plot2 = presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.4, 0.35), size = 30, ylab = "")
plot2
dev.off()


filename = paste0("images/boston_kdeVSbins.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()


### LINEAR MODEL
regr_model = makeLearner("regr.lm")
black_box  = train(regr_model, btask)
explainer  = lime(boston[, -ncol(boston)], black_box, bin_continuous = FALSE, use_density = FALSE)


# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
set.seed(123)
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/boston_lm_presi.png")
png(filename, width = 700L, height = 500L)
plot2 = presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.65, 0.45), size = 30, ylab = "")
plot2
dev.off()



### OVERFITTING MODEL
regr_model = makeLearner("regr.ranger", num.trees = 1, min.node.size = 1)
black_box  = train(regr_model, btask)
explainer  = lime(boston[, -ncol(boston)], black_box, bin_continuous = FALSE, use_density = FALSE)


# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
set.seed(123)
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})

means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/boston_tree_presi.png")
png(filename, width = 700L, height = 500L)
plot1 = presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.65, 0.45), size = 30)
plot1
dev.off()


filename = paste0("images/boston_treeVSlm.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()



################
# BIKES DATASET
################


bikes = read.csv("datasets/day.csv")
# remove undesired variables
bikes = bikes[-which(names(bikes) %in% c("casual", "registered", "instant", "dteday"))]

# quantile binning with 4 bins
bikes[c("temp", "atemp", "hum", "windspeed")] = lapply(
  bikes[c("temp", "atemp", "hum", "windspeed")],
  function(vec) {
    quantiles = quantile(vec)
    quantiles[5] = Inf
    sapply(vec, function(x) sum(x >= quantiles))
  }
)
# normalize
bikes$cnt = bikes$cnt / sd(bikes$cnt)
bikes[-ncol(bikes)] = lapply(bikes[-ncol(bikes)], as.factor)

plots = lapply(names(bikes[-ncol(bikes)]), function(feat) qplot(get(feat), cnt, data = bikes, ylab = "", xlab = feat, geom = "boxplot") + theme(
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  text = element_text(size = 20))
)

args = paste0("plots[[", 1:11, "]]", collapse = ", ")
grid_plot_func = paste0("gridExtra::grid.arrange(", args, ", nrow = 3)")

filename = paste0("images/bikes_prezi.png")
png(filename, width = 700L, height = 500L)
eval(parse(text = grid_plot_func))
dev.off()

btask      = makeRegrTask(data = bikes, target = "cnt")

# normal non-continuous model
regr_model = makeLearner("regr.ranger")
black_box  = train(regr_model, btask)
explainer  = lime(bikes[, -ncol(bikes)], black_box)


# pick majority of each feature as data point
data_point = as.data.frame(lapply(
  bikes[, -ncol(bikes)],
  function(vec) names(table(vec))[which.max(table(vec))]
))
set.seed(123)
# repeat explanation 100 times
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/bikes_standard_presi.png")
png(filename, width = 700L, height = 500L)
plot1 = presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-1.0, 1.0), size = 30)
plot1
dev.off()



# pick minority of each feature as data point
data_point = as.data.frame(lapply(
  bikes[, -ncol(bikes)],
  function(vec) names(table(vec))[which.min(table(vec))]
))

set.seed(123)
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/bikes_outlier_presi.png")
png(filename, width = 700L, height = 500L)
plot2 = presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-1.0, 1.0), size = 30, ylab = "")
plot2
dev.off()


filename = paste0("images/bikes_majVSmin.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()


# EASY MODEL
regr_model = makeLearner("regr.lm")
black_box  = train(regr_model, btask)
explainer  = lime(bikes[, -ncol(bikes)], black_box)

# pick majority of each feature as data point
data_point = as.data.frame(lapply(
  bikes[, -ncol(bikes)],
  function(vec) names(table(vec))[which.max(table(vec))]
))

set.seed(123)
# repeat explanation 100 times
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/bikes_lm_presi.png")
png(filename, width = 700L, height = 500L)
plot2 = presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-1.1, 1.1), size = 30, ylab = "")
plot2
dev.off()



####

# OVERFITTING MODEL
regr_model = makeLearner("regr.ranger", num.trees = 1, min.node.size = 1)
black_box  = train(regr_model, btask)
explainer  = lime(bikes[, -ncol(bikes)], black_box)

# pick majority of each feature as data point
data_point = as.data.frame(lapply(
  bikes[, -ncol(bikes)],
  function(vec) names(table(vec))[which.max(table(vec))]
))

set.seed(123)
# repeat explanation 100 times
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)
ci_upper = apply(limes, MARGIN = 1, quantile, 0.975)
ci_lower = apply(limes, MARGIN = 1, quantile, 0.025)

filename = paste0("images/bikes_tree_presi.png")
png(filename, width = 700L, height = 500L)
plot1 = presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-1.1, 1.1), size = 30)
plot1
dev.off()

filename = paste0("images/bikes_treeVSlm.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()


