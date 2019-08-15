#################################
# BLACK BOX COMPLEXITY/SMOOTHNESS VS STABILITY
#################################

#' @description function used in this chunk for general stability measurement in dependence
#' of black box model complexity
#' @return dataframe of feature weights
complexity_growth = function(
  data,
  target,
  pts_to_predict,
  type,
  repeats = 10L,
  max_degree = 10L,
  bin_continuous = FALSE,
  use_density = FALSE
) {
  
  
  # dimension of feature space
  p_max = ncol(data) - 1L
  feature_names = names(data[names(data) != target])
  
  # move target variable to the end
  train_data           = data[names(data) != target]
  train_data[[target]] = data[[target]]
  
  # set exponential size for nrounds hyperparameter
  degree_seq = rep(1:max_degree, each = repeats)
  # iterate over sequence of exponential degrees
  outer_return = parallel::mclapply(
    mc.cores = 4,
    degree_seq,
    function(degree) {
      
      # make model smoother with each iteration
      num.trees = 1L + (degree - 1L) * 10L
      # less overfitting
      min.node.size = degree
      
      # more overfitting
      nrounds = 2^degree
      
      # define task and learner based on data type
      if (type == "classif") {
        task = makeClassifTask(data = train_data, target = target)
        learner = makeLearner("classif.ranger", num.trees = num.trees, min.node.size = min.node.size, predict.type = "prob")
        #learner = makeLearner("classif.xgboost", nrounds = nrounds, predict.type = "prob")
        
      } else if (type == "regr") {
        task = makeRegrTask(data = train_data, target = target)
        #learner = makeLearner("regr.xgboost", nrounds = nrounds)
        learner = makeLearner("regr.ranger", num.trees = num.trees, min.node.size = min.node.size)
        
      } else {
        stop("Wrong type, buddy")
      }
      
      black_box = train(learner, task)
      explainer = lime(train_data[1L:p_max], black_box, bin_continuous = bin_continuous, use_density = use_density)
      
      if (!bin_continuous && use_density) {
        lapply(1:length(explainer$feature_distribution), function(i) {
          explainer$feature_distribution[[i]]["mean"] <<- mean(explainer$feature_distribution[[i]]$x, na.rm = TRUE)
          explainer$feature_distribution[[i]]["sd"]   <<- sd(  explainer$feature_distribution[[i]]$x, na.rm = TRUE)
        })
      }
      
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
            dist_fun = "euclidian"
          )
          
          to_update = names(feat_return) %in% explanation$feature
          feat_return[to_update] = explanation$feature_weight
          names(target_pt) = paste0("data_", feature_names)
          
          c(
            smoothness = degree,
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

data("BostonHousing", package = "mlbench")
# removing categorical feature
boston     = BostonHousing[, -4]
# normalizing standard deviation to make coefficients comparable
boston     = as.data.frame(lapply(boston, function(x) x/sd(x)))
set.seed(123)
pts_to_predict = boston[sample(1:nrow(boston), 10), -ncol(boston)]

# this may take a while
#results = complexity_growth(boston, "medv", pts_to_predict, type = "regr", repeats = 10L)
#saveRDS(results, "LIME_experiment_results/complexity_growth_boston_repeats10_xgboost10")
results = readRDS("LIME_experiment_results/complexity_growth_boston_repeats10_xgboost10")
results$type = "kernel"

# this may take a while
#resultsb = complexity_growth(boston, "medv", pts_to_predict, type = "regr", repeats = 10L, bin_continuous = TRUE, use_density = TRUE)
#saveRDS(resultsb, "LIME_experiment_results/complexity_growth_boston_repeats10_xgboost10_bins")
resultsb = readRDS("LIME_experiment_results/complexity_growth_boston_repeats10_xgboost10_bins")
resultsb$type = "bins"

# this may take a while
#resultsn = complexity_growth(boston, "medv", pts_to_predict, type = "regr", repeats = 10L, use_density = TRUE)
#saveRDS(resultsn, "LIME_experiment_results/complexity_growth_boston_repeats10_xgboost10_nd")
resultsn = readRDS("LIME_experiment_results/complexity_growth_boston_repeats10_xgboost10_nd")
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
# dummy encoding
bikes = createDummyFeatures(
  bikes, target = "cnt",
  cols = names(bikes[-ncol(bikes)])
)
set.seed(123)
pts_to_predict = bikes[sample(10, 1:nrow(bikes)), -1] # "cnt" switched position

#resultsc = complexity_growth(bikes, "cnt", pts_to_predict, type = "regr", repeats = 10L, bin_continuous = TRUE, use_density = TRUE)
#saveRDS(resultsc, "LIME_experiment_results/complexity_growth_bikes_xgboost10")
resultsc = readRDS("LIME_experiment_results/complexity_growth_bikes_xgboost10")
resultsc$type = "categorical"

# merge results
results = rbind(results, resultsb, resultsn)

# bikes...
resultsc$season = resultsc$season.1 + resultsc$season.2 + resultsc$season.3 + resultsc$season.4
resultsc$yr = resultsc$yr.0 + resultsc$yr.1
resultsc$mnth = eval(parse(text = paste0("resultsc$mnth.", 1:12, collapse = " + ")))
resultsc$holiday = eval(parse(text = paste0("resultsc$holiday.", 0:1, collapse = " + ")))
resultsc$weekday = eval(parse(text = paste0("resultsc$weekday.", 0:6, collapse = " + ")))
resultsc$workingday = eval(parse(text = paste0("resultsc$workingday.", 0:1, collapse = " + ")))
resultsc$weathersit = eval(parse(text = paste0("resultsc$weathersit.", 1:3, collapse = " + ")))
resultsc$temp = eval(parse(text = paste0("resultsc$temp.", 1:4, collapse = " + ")))
resultsc$atemp = eval(parse(text = paste0("resultsc$atemp.", 1:4, collapse = " + ")))
resultsc$hum = eval(parse(text = paste0("resultsc$hum.", 1:4, collapse = " + ")))
resultsc$windspeed = eval(parse(text = paste0("resultsc$windspeed.", 1:4, collapse = " + ")))

resultsc$data_season = resultsc$season.1 + resultsc$season.2*2 + resultsc$season.3*3 + resultsc$season.4*4
resultsc$data_yr = resultsc$yr.0 + resultsc$yr.1 * 2
resultsc$data_mnth = eval(parse(text = paste0("resultsc$data_mnth.", 1:12, "*", 1:12, collapse = " + ")))
resultsc$data_holiday = eval(parse(text = paste0("resultsc$data_holiday.", 0:1, "*", 1:2, collapse = " + ")))
resultsc$data_weekday = eval(parse(text = paste0("resultsc$data_weekday.", 0:6, "*", 1:7, collapse = " + ")))
resultsc$data_workingday = eval(parse(text = paste0("resultsc$data_workingday.", 0:1, "*", 1:2, collapse = " + ")))
resultsc$data_weathersit = eval(parse(text = paste0("resultsc$data_weathersit.", 1:3, "*", 1:3, collapse = " + ")))
resultsc$data_temp = eval(parse(text = paste0("resultsc$data_temp.", 1:4, "*", 1:4, collapse = " + ")))
resultsc$data_atemp = eval(parse(text = paste0("resultsc$data_atemp.", 1:4, "*", 1:4, collapse = " + ")))
resultsc$data_hum = eval(parse(text = paste0("resultsc$data_hum.", 1:4, "*", 1:4, collapse = " + ")))
resultsc$data_windspeed = eval(parse(text = paste0("resultsc$data_windspeed.", 1:4, "*", 1:4, collapse = " + ")))


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
  smoothness,
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
  smoothness,
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
  smoothness,
  type
)

# bikes
resultsc_gr = dplyr::group_by(
  resultsc_sd,
  smoothness,
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
  sd = c(
    apply(results_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)]))#,
    #apply(resultsc_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)]))
  ),
  smoothness = c(
    results_sd$smoothness#,
    #as.numeric(as.character(resultsc_sd$smoothness))
  ),
  type = c(results_sd$type)#, resultsc_sd$type)
)

############################
### calculate train error
############################

# set exponential size for nrounds hyperparameter
degree_seq = rep(1:10)
# iterate over sequence of exponential degrees
errors_data = lapply(#parallel::mclapply(
  #mc.cores = 4,
  degree_seq,
  function(degree) {
    
    # more overfitting
    nrounds = 2^degree
    
    task = makeRegrTask(data = boston, target = "medv")
    learner = makeLearner("regr.xgboost", nrounds = nrounds)
    
    # let's assume 10 times 99,9% of the training data have almost equal train error as 100%
    black_box = train(learner, task)
    
    preds = predict(black_box, task)
    
    data.frame(
      smoothness = degree,
      mse = mean((preds$data$response - preds$data$truth)^2, na.rm = TRUE)
    )
  }
)
# concatenate dataframes
errors_data = data.table::rbindlist(errors_data)


plot = ggplot(plot_data, aes(y = sd, x = smoothness, color = type)) +
  geom_line(size = 2L) +
  geom_line(data = errors_data, aes(y = mse/max(mse) * 0.02, x = smoothness, color = NULL), size = 2L) +
  theme_minimal() +
  theme(
    text = element_text(size = 25L),
    axis.title.x = element_text(vjust = -4),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = ggplot2::margin(20,20,30,20)
  ) +
  ylab("Average Standard Deviation") +
  xlab("Amount of trees") +
  scale_x_continuous(
    breaks = 1:10,
    labels = 2^(1:10)
  )

filename = paste0("images/sd_overfitting_presi2.png")
png(filename, width = 1000L, height = 500L)
plot
dev.off()



data("BostonHousing", package = "mlbench")
# removing categorical feature
boston     = BostonHousing[, -4]
# normalizing standard deviation to make coefficients comparable
boston     = as.data.frame(lapply(boston, function(x) x/sd(x)))
set.seed(123)
pts_to_predict = boston[sample(1:nrow(boston), 10), -ncol(boston)]

# this may take a while
results = readRDS("LIME_experiment_results/complexity_growth_boston_repeats10")
#complexity_growth(boston, "medv", pts_to_predict, type = "regr", repeats = 10L)
#saveRDS(results, "LIME_experiment_results/complexity_growth_boston_repeats10")
results$type = "kernel"

# this may take a while
resultsb = readRDS("LIME_experiment_results/complexity_growth_boston_repeats10_bins")
#complexity_growth(boston, "medv", pts_to_predict, type = "regr", repeats = 10L, bin_continuous = TRUE, use_density = TRUE)
#saveRDS(resultsb, "LIME_experiment_results/complexity_growth_boston_repeats10_bins")
resultsb$type = "bins"

# this may take a while
resultsn = readRDS("LIME_experiment_results/complexity_growth_boston_repeats10_nd")
#complexity_growth(boston, "medv", pts_to_predict, type = "regr", repeats = 10L, use_density = TRUE)
#saveRDS(resultsn, "LIME_experiment_results/complexity_growth_boston_repeats10_nd")
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
pts_to_predict = bikes[sample(1:nrow(bikes), 10), -ncol(bikes)]

#resultsc = complexity_growth(bikes, "cnt", pts_to_predict, type = "regr", repeats = 10L, bin_continuous = TRUE, use_density = TRUE)
#saveRDS(resultsc, "LIME_experiment_results/complexity_growth_bikes_repeats10")
resultsc = readRDS("LIME_experiment_results/complexity_growth_bikes_repeats10")
resultsc$type = "categorical"


# merge results
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
  smoothness,
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
  smoothness,
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
  smoothness,
  type
)

# bikes
resultsc_gr = dplyr::group_by(
  resultsc_sd,
  smoothness,
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
  sd = c(
    apply(results_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)])),
    apply(resultsc_sd[-1:-2], MARGIN = 1, function(row) mean(row[!is.nan(row)]))
  ),
  smoothness = c(
    results_sd$smoothness,
    as.numeric(as.character(resultsc_sd$smoothness))
  ),
  type = c(results_sd$type, resultsc_sd$type)
)

plot = ggplot(plot_data, aes(y = sd, x = smoothness, color = type)) +
  geom_line(size = 2L) +
  theme_minimal() +
  theme(
    text = element_text(size = 25),
    axis.title.x = element_text(vjust = -4),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = ggplot2::margin(20,20,30,20)#,
    #axis.ticks.x = element_blank()
  ) +
  ylab("Average Standard Deviation") +
  xlab("Min. node size and tree amount [x10]") +
  scale_x_continuous(
    breaks = 1:10,
    labels = 1:10
  )

filename = paste0("images/sd_smoothness_presi2.png")
png(filename, width = 1000L, height = 500L)
plot
dev.off()


