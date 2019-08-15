#################################
# AMOUNT OF FEATURES VS STABILITY
#################################

#' @description function used in this chunk for general stability measurement in dependence
#' of feature dimensions and amount of selected features (n_features parameter)
#' @return dataframe of feature weights
feature_growth = function(
  data,
  target,
  pts_to_predict,
  type,
  repeats = 10L,
  dim_increment = 10L,
  bin_continuous = FALSE,
  use_density = FALSE
) {
  
  # dimension of feature space
  p_max = ncol(data) - 1L
  feature_names = names(data[names(data) != target])
  
  # move target variable to the end
  data_sort           = data[names(data) != target]
  data_sort[[target]] = data[[target]]
  
  # remove the target variable from points for interpretation
  pts_to_predict = pts_to_predict[names(pts_to_predict) != target]
  
  # iterate over amount of feature dimensions
  outer_return = lapply(#parallel::mclapply(
    #mc.cores = 4,
    seq(2L, p_max, by = dim_increment),
    function(p) {
      
      # define train data based on iterated dimension
      train_data           = data_sort[, 1L:p]
      train_data[[target]] = data_sort[[target]]
      
      # define task and learner based on data type
      if (type == "classif") {
        task = makeClassifTask(data = train_data, target = target)
        learner = makeLearner("classif.randomForest", ntree = 20L, predict.type = "prob")
        
      } else if (type == "regr") {
        task = makeRegrTask(data = train_data, target = target)
        learner = makeLearner("regr.randomForest", ntree = 20L)
        
      } else {
        stop("Wrong type, buddy")
      }
      
      black_box = train(learner, task)
      explainer = lime(train_data[1L:p], black_box, bin_continuous = bin_continuous, use_density = use_density)
      
      if (!bin_continuous && use_density) {
        lapply(1:length(explainer$feature_distribution), function(i) {
          explainer$feature_distribution[[i]]["mean"] <<- mean(explainer$feature_distribution[[i]]$x, na.rm = TRUE)
          explainer$feature_distribution[[i]]["sd"]   <<- sd(  explainer$feature_distribution[[i]]$x, na.rm = TRUE)
        })
      }
      
      # create sequence of "n_feature" arguments
      n_feat_seq = seq(1L, p, by = dim_increment)
      n_feat_seq = rep(n_feat_seq, each = repeats)
      
      # iterate over sequence of "n_feature" arguments
      inner_return = lapply(
        n_feat_seq,
        function(n_features) {
          
          feat_return        = rep(NA, p_max)
          names(feat_return) = feature_names
          
          # iterate over all points for interpretation
          inner_inner = apply(
            pts_to_predict,
            MARGIN = 1,
            function(target_pt) {
              
              explanation = explain(
                as.data.frame(t(target_pt[1:p])),
                explainer,
                n_labels = 1L,
                n_features = n_features,
                dist_fun = "euclidian",
                kernel_width = 100
              )
              
              to_update = names(feat_return) %in% explanation$feature
              feat_return[to_update] = explanation$feature_weight
              names(target_pt) = paste0("data_", feature_names)
              
              c(
                p = p,
                n_features = n_features,
                target_pt,
                feat_return
              )
            }
          )
          # transform from matrix to dataframe
          as.data.frame(t(inner_inner))
        }
      )
      
      # output progress
      log = sprintf("%2.2f/1.00 done", (p-1)/(p_max-1))
      print(log)
      # concatenate dataframes
      data.table::rbindlist(inner_return)
    }
  )
  # concatenate dataframes
  data.table::rbindlist(outer_return)
}


data("BostonHousing", package = "mlbench")
# removing categorical feature
boston     = BostonHousing[, -4]
# normalizing standard deviation to make coefficients comparable
boston     = as.data.frame(lapply(boston, function(x) x/sd(x)))
# forgot seed, pls dont kill me
set.seed(123)
pts_to_predict = boston[sample(1:nrow(boston), 10), -ncol(boston)]

# this may take a while
#results = feature_growth(boston, "medv", pts_to_predict, type = "regr", dim_increment = 1L, repeats = 10)
#saveRDS(results, "LIME_experiment_results/feature_growth_boston_repeats10")
results  = readRDS("LIME_experiment_results/feature_growth_boston_repeats10")
results$type = "kernel"
#resultsb = feature_growth(
#  boston, "medv", pts_to_predict,
#  type = "regr",
#  dim_increment = 1L,
#  repeats = 10,
#  bin_continuous = TRUE,
#  use_density = TRUE
#)
#saveRDS(resultsb, "LIME_experiment_results/feature_growth_boston_repeats10_bins")
resultsb = readRDS("LIME_experiment_results/feature_growth_boston_repeats10_bins")
resultsb$type = "bins"
#resultsn = feature_growth(
#  boston, "medv", pts_to_predict,
#  type = "regr",
#  dim_increment = 1L,
#  repeats = 10,
#  use_density = TRUE
#)
#saveRDS(resultsn, "LIME_experiment_results/feature_growth_boston_repeats10_nd")
resultsn = readRDS("LIME_experiment_results/feature_growth_boston_repeats10_nd")
resultsn$type = "normal"

### categorical data
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
set.seed(123)
pts_to_predict = bikes[sample(10, 1:nrow(bikes)), -ncol(bikes)]

#resultsc = feature_growth(
#  bikes, "cnt", pts_to_predict,
#  type = "regr", dim_increment = 1L, repeats = 10,
#  bin_continuous = TRUE,
#  use_density = TRUE
#)
#saveRDS(resultsc, "LIME_experiment_results/feature_growth_bikes_repeats10")
resultsc = readRDS("LIME_experiment_results/feature_growth_bikes_repeats10")
resultsc$type = "categorical"


results = rbind(results, resultsb, resultsn)
# boston
results_gr = dplyr::group_by(
  results,
  data_crim,
  data_zn,
  data_indus,
  data_nox,
  data_rm,
  data_age,
  data_dis,
  data_rad,
  data_tax,
  data_ptratio,
  data_b,
  data_lstat,
  p,
  type
)

# bikes
resultsc_gr = dplyr::group_by(
  resultsc,
  data_season,
  data_yr,
  data_mnth,
  data_holiday,
  data_weekday,
  data_workingday,
  data_weathersit,
  data_temp,
  data_atemp,
  data_hum,
  data_windspeed,
  p,
  type
)

# boston
results_sd = as.data.frame(dplyr::summarize(
  results_gr,
  sd_crim  = sd(crim, na.rm = TRUE),
  sd_zn    = sd(zn, na.rm = TRUE),
  sd_indus = sd(indus, na.rm = TRUE),
  sd_nox   = sd(nox, na.rm = TRUE),
  sd_rm    = sd(rm, na.rm = TRUE),
  sd_age   = sd(age, na.rm = TRUE),
  sd_dis   = sd(dis, na.rm = TRUE),
  sd_rad   = sd(rad, na.rm = TRUE),
  sd_tax   = sd(tax, na.rm = TRUE),
  sd_ptratio = sd(ptratio, na.rm = TRUE),
  sd_b     = sd(b, na.rm = TRUE),
  sd_lstat = sd(lstat, na.rm = TRUE)#,
))

# bikes
resultsc_sd = as.data.frame(dplyr::summarize(
  resultsc_gr,
  sd_season     = sd(as.numeric(as.character(season)), na.rm = TRUE),
  sd_yr         = sd(as.numeric(as.character(yr)), na.rm = TRUE),
  sd_mnth       = sd(as.numeric(as.character(mnth)), na.rm = TRUE),
  sd_holiday    = sd(as.numeric(as.character(holiday)), na.rm = TRUE),
  sd_weekday    = sd(as.numeric(as.character(weekday)), na.rm = TRUE),
  sd_workingday = sd(as.numeric(as.character(workingday)), na.rm = TRUE),
  sd_weathersit = sd(as.numeric(as.character(weathersit)), na.rm = TRUE),
  sd_temp       = sd(as.numeric(as.character(temp)), na.rm = TRUE),
  sd_atemp      = sd(as.numeric(as.character(atemp)), na.rm = TRUE),
  sd_hum        = sd(as.numeric(as.character(hum)), na.rm = TRUE),
  sd_windspeed  = sd(as.numeric(as.character(windspeed)), na.rm = TRUE)
))
# the following shows that the standard deviation is the same across (almost) all features
# thus standardizing would do more harm than good
#summary(resultsc_sd)

results_gr = dplyr::group_by(
  results_sd,
  p,
  type
)

# bikes
resultsc_gr = dplyr::group_by(
  resultsc_sd,
  p,
  type
)

# boston
results_sd = as.data.frame(dplyr::summarize(
  results_gr,
  sd_crim  = mean(sd_crim, na.rm = TRUE),
  sd_zn    = mean(sd_zn, na.rm = TRUE),
  sd_indus = mean(sd_indus, na.rm = TRUE),
  sd_nox   = mean(sd_nox, na.rm = TRUE),
  sd_rm    = mean(sd_rm, na.rm = TRUE),
  sd_age   = mean(sd_age, na.rm = TRUE),
  sd_dis   = mean(sd_dis, na.rm = TRUE),
  sd_rad   = mean(sd_rad, na.rm = TRUE),
  sd_tax   = mean(sd_tax, na.rm = TRUE),
  sd_ptratio = mean(sd_ptratio, na.rm = TRUE),
  sd_b     = mean(sd_b, na.rm = TRUE),
  sd_lstat = mean(sd_lstat, na.rm = TRUE)#,
))

# bikes
resultsc_sd = as.data.frame(dplyr::summarize(
  resultsc_gr,
  sd_season     = mean(as.numeric(as.character(sd_season)), na.rm = TRUE),
  sd_yr         = mean(as.numeric(as.character(sd_yr)), na.rm = TRUE),
  sd_mnth       = mean(as.numeric(as.character(sd_mnth)), na.rm = TRUE),
  sd_holiday    = mean(as.numeric(as.character(sd_holiday)), na.rm = TRUE),
  sd_weekday    = mean(as.numeric(as.character(sd_weekday)), na.rm = TRUE),
  sd_workingday = mean(as.numeric(as.character(sd_workingday)), na.rm = TRUE),
  sd_weathersit = mean(as.numeric(as.character(sd_weathersit)), na.rm = TRUE),
  sd_temp       = mean(as.numeric(as.character(sd_temp)), na.rm = TRUE),
  sd_atemp      = mean(as.numeric(as.character(sd_atemp)), na.rm = TRUE),
  sd_hum        = mean(as.numeric(as.character(sd_hum)), na.rm = TRUE),
  sd_windspeed  = mean(as.numeric(as.character(sd_windspeed)), na.rm = TRUE)
))

plot_data = data.frame(sd = c(
  apply(results_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)]))#,
  #  apply(resultsc_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)]))
))
plot_data$p = c(results_sd$p)#, as.numeric(as.character(resultsc_sd$p)))
plot_data$type = c(results_sd$type)#, resultsc_sd$type)


plot = ggplot(plot_data, aes(y = sd, x = p, color = type)) +
  geom_line(size = 2) +
  theme_minimal() +
  theme(
    text = element_text(size = 25L),
    axis.title.x = element_text(vjust = -4L),
    axis.text.x = element_text(angle = 45L, hjust = 1L),
    plot.margin = ggplot2::margin(20L, 20L, 30L, 20L),
    axis.ticks.x = element_blank()
  ) +
  ylab("Average Standard Deviation") +
  xlab("Added feature") +
  scale_x_continuous(
    breaks = 1L:12L,
    labels = c("", "crim & zn", names(boston[c(-1, -2, -13)]))
  ) +
  ylim(c(0, 0.2))

filename = paste0("images/sd_p_presi2.png")
png(filename, width = 700L, height = 500L)
plot
dev.off()

plot_datac = data.frame(
  sd = apply(resultsc_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)], na.rm = TRUE)),
  p = as.numeric(as.character(resultsc_sd$p)),
  type = resultsc_sd$type
)

plotc = ggplot(plot_datac, aes(y = sd, x = p, color = type)) +
  geom_line(size = 2L) +
  theme_minimal() +
  theme(
    text = element_text(size = 25L),
    axis.title.x = element_text(vjust = -4L),
    axis.text.x = element_text(angle = 45L, hjust = 1L),
    plot.margin = ggplot2::margin(20L, 20L, 30L, 20L),
    axis.ticks.x = element_blank()
  ) +
  ylab("Average Standard Deviation") +
  xlab("Added feature") +
  scale_x_continuous(
    breaks = 1L:11L,
    labels = c("", "season & yr", names(bikes[c(-1, -2, -12)]))
  ) +
  ylim(c(0, 0.2))


filename = paste0("images/sd_pc_presi2.png")
png(filename, width = 700L, height = 500L)
plotc
dev.off()


#######################
### plotting n_features
#######################
# boston
results_12 = results[results$p == 12, ]
# bikes
resultsc_11 = resultsc[resultsc$p == 11, ]

results_gr = dplyr::group_by(
  results_12,
  data_crim,
  data_zn,
  data_indus,
  data_nox,
  data_rm,
  data_age,
  data_dis,
  data_rad,
  data_tax,
  data_ptratio,
  data_b,
  data_lstat,
  n_features,
  type
)

# bikes
resultsc_gr = dplyr::group_by(
  resultsc_11,
  data_season,
  data_yr,
  data_mnth,
  data_holiday,
  data_weekday,
  data_workingday,
  data_weathersit,
  data_temp,
  data_atemp,
  data_hum,
  data_windspeed,
  n_features,
  type
)

# boston
results_sd = as.data.frame(dplyr::summarize(
  results_gr,
  sd_crim  = sd(crim, na.rm = TRUE),
  sd_zn    = sd(zn, na.rm = TRUE),
  sd_indus = sd(indus, na.rm = TRUE),
  sd_nox   = sd(nox, na.rm = TRUE),
  sd_rm    = sd(rm, na.rm = TRUE),
  sd_age   = sd(age, na.rm = TRUE),
  sd_dis   = sd(dis, na.rm = TRUE),
  sd_rad   = sd(rad, na.rm = TRUE),
  sd_tax   = sd(tax, na.rm = TRUE),
  sd_ptratio = sd(ptratio, na.rm = TRUE),
  sd_b     = sd(b, na.rm = TRUE),
  sd_lstat = sd(lstat, na.rm = TRUE)#,
))

# bikes
resultsc_sd = as.data.frame(dplyr::summarize(
  resultsc_gr,
  sd_season     = sd(as.numeric(as.character(season)), na.rm = TRUE),
  sd_yr         = sd(as.numeric(as.character(yr)), na.rm = TRUE),
  sd_mnth       = sd(as.numeric(as.character(mnth)), na.rm = TRUE),
  sd_holiday    = sd(as.numeric(as.character(holiday)), na.rm = TRUE),
  sd_weekday    = sd(as.numeric(as.character(weekday)), na.rm = TRUE),
  sd_workingday = sd(as.numeric(as.character(workingday)), na.rm = TRUE),
  sd_weathersit = sd(as.numeric(as.character(weathersit)), na.rm = TRUE),
  sd_temp       = sd(as.numeric(as.character(temp)), na.rm = TRUE),
  sd_atemp      = sd(as.numeric(as.character(atemp)), na.rm = TRUE),
  sd_hum        = sd(as.numeric(as.character(hum)), na.rm = TRUE),
  sd_windspeed  = sd(as.numeric(as.character(windspeed)), na.rm = TRUE)
))

# boston
results_gr = dplyr::group_by(
  results_sd,
  n_features,
  type
)

# bikes
resultsc_gr = dplyr::group_by(
  resultsc_sd,
  n_features,
  type
)

# boston
results_sd = as.data.frame(dplyr::summarize(
  results_gr,
  sd_crim  = mean(sd_crim, na.rm = TRUE),
  sd_zn    = mean(sd_zn, na.rm = TRUE),
  sd_indus = mean(sd_indus, na.rm = TRUE),
  sd_nox   = mean(sd_nox, na.rm = TRUE),
  sd_rm    = mean(sd_rm, na.rm = TRUE),
  sd_age   = mean(sd_age, na.rm = TRUE),
  sd_dis   = mean(sd_dis, na.rm = TRUE),
  sd_rad   = mean(sd_rad, na.rm = TRUE),
  sd_tax   = mean(sd_tax, na.rm = TRUE),
  sd_ptratio = mean(sd_ptratio, na.rm = TRUE),
  sd_b     = mean(sd_b, na.rm = TRUE),
  sd_lstat = mean(sd_lstat, na.rm = TRUE)#,
))

# bikes
resultsc_sd = as.data.frame(dplyr::summarize(
  resultsc_gr,
  sd_season     = mean(as.numeric(as.character(sd_season)), na.rm = TRUE),
  sd_yr         = mean(as.numeric(as.character(sd_yr)), na.rm = TRUE),
  sd_mnth       = mean(as.numeric(as.character(sd_mnth)), na.rm = TRUE),
  sd_holiday    = mean(as.numeric(as.character(sd_holiday)), na.rm = TRUE),
  sd_weekday    = mean(as.numeric(as.character(sd_weekday)), na.rm = TRUE),
  sd_workingday = mean(as.numeric(as.character(sd_workingday)), na.rm = TRUE),
  sd_weathersit = mean(as.numeric(as.character(sd_weathersit)), na.rm = TRUE),
  sd_temp       = mean(as.numeric(as.character(sd_temp)), na.rm = TRUE),
  sd_atemp      = mean(as.numeric(as.character(sd_atemp)), na.rm = TRUE),
  sd_hum        = mean(as.numeric(as.character(sd_hum)), na.rm = TRUE),
  sd_windspeed  = mean(as.numeric(as.character(sd_windspeed)), na.rm = TRUE)
))

plot_data = data.frame(
  sd = apply(results_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row) & !is.na(row)])),
  n_features = results_sd$n_features,
  type = results_sd$type
)

plot = ggplot(plot_data, aes(y = sd, x = n_features, color = type)) +
  geom_line(size = 2L) +
  theme_minimal() +
  theme(
    text = element_text(size = 25L),
    axis.title.x = element_text(vjust = -4L),
    axis.text.x = element_text(angle = 45L, hjust = 1L),
    plot.margin = ggplot2::margin(20L, 20L, 30L, 20L),
    axis.ticks.x = element_blank()
  ) +
  ylab("Average Standard Deviation") +
  xlab("Amount selected features") +
  scale_x_continuous(
    breaks = 1:12L,
    labels = 1:12L
  ) +
  ylim(c(0, 0.3))

filename = paste0("images/sd_nfeat_presi2.png")
png(filename, width = 700L, height = 500L)
plot
dev.off()

# bikes
plot_datac = data.frame(
  sd = apply(resultsc_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)], na.rm = TRUE)),
  n_features = as.numeric(as.character(resultsc_sd$n_features)),
  type = resultsc_sd$type
)

plotc = ggplot(plot_datac, aes(y = sd, x = n_features, color = type)) +
  geom_line(size = 2L) +
  theme_minimal() +
  theme(
    text = element_text(size = 25L),
    axis.title.x = element_text(vjust = -4L),
    axis.text.x = element_text(angle = 45L, hjust = 1L),
    plot.margin = ggplot2::margin(20L, 20L, 30L, 20L),
    axis.ticks.x = element_blank()
  ) +
  ylab("Average Standard Deviation") +
  xlab("Amount selected features") +
  scale_x_continuous(
    breaks = 1L:11L,
    labels = 1L:11L
  ) +
  ylim(c(0, 0.3))

filename = paste0("images/sd_nfeatc_presi2.png")
png(filename, width = 700L, height = 500L)
plotc
dev.off()
