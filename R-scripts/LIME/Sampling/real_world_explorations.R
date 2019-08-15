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
presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.4, 0.3))
dev.off()

filename = paste0("images/boston_100iter_standard.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


# pick max of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], max))
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
presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.4, 0.3))
dev.off()


filename = paste0("images/boston_100iter_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()

# switch to bins
explainer  = lime(boston[, -ncol(boston)], black_box)

# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
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
presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.4, 0.3))
dev.off()


filename = paste0("images/boston_100iter_bins.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()

# pick max of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], max))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)

filename = paste0("images/boston_100iter_bins_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


### LINEAR MODEL
regr_model = makeLearner("regr.lm")
black_box  = train(regr_model, btask)
explainer  = lime(boston[, -ncol(boston)], black_box, bin_continuous = FALSE, use_density = FALSE)

# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
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
presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.45, 0.35))
dev.off()

filename = paste0("images/boston_lm.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


# pick max of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], max))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)

filename = paste0("images/boston_lm_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


# switch to bins
explainer  = lime(boston[, -ncol(boston)], black_box)

# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})

means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)

filename = paste0("images/boston_lm_bins.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


# pick max of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], max))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})

means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)

filename = paste0("images/boston_lm_bins_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


### OVERFITTING MODEL
regr_model = makeLearner("regr.ranger", num.trees = 1, min.node.size = 1)
black_box  = train(regr_model, btask)
explainer  = lime(boston[, -ncol(boston)], black_box, bin_continuous = FALSE, use_density = FALSE)

# pick mean of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], mean))
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
presi_plot(means, lower = ci_lower, ci_upper, angle = 45L, hjust = 1L, ylim = c(-0.45, 0.35))
dev.off()


filename = paste0("images/boston_tree.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
dev.off()


# pick max of each feature as data point
data_point = as.data.frame(lapply(boston[, -ncol(boston)], max))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(boston)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})

means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)

filename = paste0("images/boston_tree_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds)
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
presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-1.0, 1.0))
dev.off()


filename = paste0("images/bikes_standard.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#62b1e7", color2 = "#6274e7", angle = 45, hjust = 1)
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
presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-1.0, 1.0))
dev.off()


filename = paste0("images/bikes_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#62b1e7", color2 = "#6274e7", angle = 45, hjust = 1)
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
presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-0.6, 1.1))
dev.off()


filename = paste0("images/bikes_lm.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#62b1e7", color2 = "#6274e7", angle = 45, hjust = 1)
dev.off()


# pick minority of each feature as data point
data_point = as.data.frame(lapply(
  bikes[, -ncol(bikes)],
  function(vec) names(table(vec))[which.min(table(vec))]
))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)


filename = paste0("images/bikes_lm_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#62b1e7", color2 = "#6274e7", angle = 45, hjust = 1)
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
presi_plot(means, lower = ci_lower, ci_upper, color1 = "#62b1e7", color2 = "#6274e7", angle = 45L, hjust = 1L, ylim = c(-0.6, 1.1))
dev.off()


filename = paste0("images/bikes_tree.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#62b1e7", color2 = "#6274e7", angle = 45, hjust = 1)
dev.off()


# pick minority of each feature as data point
data_point = as.data.frame(lapply(
  bikes[, -ncol(bikes)],
  function(vec) names(table(vec))[which.min(table(vec))]
))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)


filename = paste0("images/bikes_tree_outlier.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#62b1e7", color2 = "#6274e7", angle = 45, hjust = 1)
dev.off()


# MORE BIKES

bikes = read.csv("datasets/day.csv")
# remove undesired variables
bikes = bikes[-which(names(bikes) %in% c("casual", "registered", "instant", "dteday"))]
bikes[c("temp", "atemp", "hum", "windspeed")] = as.data.frame(lapply(
  bikes[c("temp", "atemp", "hum", "windspeed")],
  function(x) x/sd(x)
))

# normalize
bikes$cnt = bikes$cnt / sd(bikes$cnt)

bikes_num = as.data.frame(bikes[c("temp", "atemp", "hum", "windspeed")])
bikes_num$cnt = bikes$cnt

b_numtask = makeRegrTask(data = bikes_num, target = "cnt")

# normal non-continuous model
regr_model = makeLearner("regr.ranger")
black_box  = train(regr_model, b_numtask)
explainer  = lime(bikes_num[, -ncol(bikes_num)], black_box)

# pick majority of each feature as data point
data_point = as.data.frame(lapply(
  bikes_num[, -ncol(bikes_num)],
  mean
))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes_num)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)


filename = paste0("images/bikes_bins_lime.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#df8a4f", color2 = "#c64b00", angle = 45, hjust = 1)
dev.off()

# quantile binning with 4 bins
bikes_bins = as.data.frame(lapply(
  bikes[c("temp", "atemp", "hum", "windspeed")],
  function(vec) {
    quantiles = quantile(vec)
    quantiles[5] = Inf
    sapply(vec, function(x) sum(x >= quantiles))
  }
))

bikes_bins$cnt = bikes$cnt
bikes_bins[-ncol(bikes_bins)] = lapply(bikes_bins[-ncol(bikes_bins)], as.factor)


b_binstask = makeRegrTask(data = bikes_bins, target = "cnt")

# normal non-continuous model
regr_model = makeLearner("regr.ranger")
black_box  = train(regr_model, b_binstask)
explainer  = lime(bikes_bins[, -ncol(bikes_bins)], black_box)

# put mean point into bins
data_point = as.data.frame(lapply(
  names(data_point),
  function(name) {
    quantiles = quantile(bikes_num[[name]])
    quantiles[5] = Inf
    as.factor(sum(data_point[[name]] >= quantiles))
  }
))
names(data_point) = names(bikes_bins[-ncol(bikes_bins)])

# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes_bins)-1)
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)

filename = paste0("images/bikes_manual_bins.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#df8a4f", color2 = "#c64b00", angle = 45, hjust = 1)
dev.off()

##########
### non bins, but kernel density estimation

# normal non-continuous model
regr_model = makeLearner("regr.ranger")
black_box  = train(regr_model, b_numtask)
explainer  = lime(bikes_num[, -ncol(bikes_num)], black_box, bin_continuous = FALSE, use_density = FALSE)

# pick majority of each feature as data point
data_point = as.data.frame(lapply(
  bikes_num[, -ncol(bikes_num)],
  mean
))
# set index of datapoint to use
limes = sapply(1:100, function(k) {
  interim = explain(data_point, explainer, n_features = ncol(bikes_num)-1, dist_fun = "euclidian")
  weights_l = interim$feature_weight
  names(weights_l) = interim$feature
  weights_l
})


means = apply(limes, MARGIN = 1, mean)
sds   = apply(limes, MARGIN = 1, sd)


filename = paste0("images/bikes_no_bins.png")
png(filename, width = 700L, height = 500L)
book_plot(means, sds, color1 = "#df8a4f", color2 = "#c64b00", angle = 45, hjust = 1)
dev.off()
