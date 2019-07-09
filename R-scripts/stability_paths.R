devtools::install_github("cran/ElemStatLearn")
library(ElemStatLearn)
library(MASS)
get_stability_paths <- function(model, data, reps = 100, method = "subsample",
                                strata = NULL, fraction = 0.5) {
  stopifnot(inherits(model, "regsubsets"))
  selected <- list()
  for (i in seq_len(reps)) {
    new_data <- resample(data, method = method, strata = strata, fraction = fraction)
    new_model <- refit(model, new_data)
    selected[[i]] <- get_selected(new_model)
  }
  stability_paths <- make_paths(selected)
  stability_paths
}

############## resample ########################################################

resample <- function(data, method = c("subsample", "bootstrap"),
                     strata = NULL, fraction = 0.5) {
  method <- match.arg(method)
  nrows <- nrow(data)
  rows <- resample_rows(nrows, method, strata, fraction)
  data[rows, ]
}

resample_rows <- function(nrows, method, strata = NULL, fraction = 0.5) {
  if (method == "bootstrap") {
    resampled_rows <- sample_with_replacement(nrows, strata)
  } else {
    resampled_rows <- sample_without_replacement(nrows, strata, fraction = fraction)
  }
  return(resampled_rows)
}

sample_with_replacement <- function(nrows, strata = NULL) {
  if (is.null(strata)) {
    sample(nrows, replace = TRUE)
  } else {
    stopifnot(length(strata) == nrows)
    rows <- tapply(
      X = seq_len(nrows), INDEX = strata, FUN = sample,
      replace = TRUE
    )
    return(as.vector(unlist(rows)))
  }
}

############## refit ###########################################################

refit <- function(model, new_data) {
  modelcall <- model$call
  modelcall$data <- new_data
  # use regsubsets-generic instead of regsubsets.formula or other method as
  # methods are not exported by leaps:
  modelcall[[1]] <- `regsubsets`
  eval(modelcall)
}


### This function samples rows without replacement. This function is very general;
### no (immiediate) data is needed for it to run. Two branches exist: one performs
### stratified (strata != NULL) sampling, one unstratified sampling. Fraction
### determines which fraction of the data is sampled.
### Arguments:
### nrows:    number of rows of the data set we want to sample from. 
###           Needs to be an integer.
### strata:   indicates to which class/stratum each observation belongs;
###           Needs to be a vector of the same length as the data set for which
###           we intend to sample. Must be convertible to reasonable factors.
###           Alternatively: If the data is not stratified, NULL (default) may
###           be entered.
### fraction: indicates the fraction of how often (relative to the absolute #
###           of rows) we want to sample. Must be a float number between [0,1]
### Returns: A vector with the indices of the rows to be sampled.

#install.packages("checkmate")
library(checkmate)

sample_without_replacement <- function(nrows, strata = NULL, fraction = 0.5) {
  # Input checking
  assert_number(fraction, lower = 0, upper = 1)
  if (is.null(strata)) {
    rows <- sample(nrow(data), ceiling(0.5 * nrow(data)), replace = FALSE)
  } else {
    stopifnot(length(strata) == nrows)
    index <- 1:nrows
    rows <- c()
    # Sample for each stratum the fraction of data provided
    for (s in unique(strata)){
      nrows_strata <- sum(ifelse(strata == s, 1, 0))
      strata_index <- index[strata == s]
      strata_sample <- sample(nrow(data), ceiling(fraction * nrow(data)), replace = FALSE)
      strata_rows <- strata_index[strata_sample]
      rows <- as.vector(rbind(rows, strata_rows))
    }
  }
  return(rows)
}

### This function determines the partial stability path for all variables for a
### given susample.
### Arguments:
### new_model:  The model on which the data shall be refit. In this case this must
###             a model of the class "regsubset".
### Returns:    One realisation of the stability paths;
###             A dataframe / matrix with arbitrary number of different 
###             regularisation parameters (rows) and arbitrary number of
###             variables (columns). Each entry (boolean) indicates whether
###             the variable is selected into the model or not. 

get_selected <- function(new_model) {
  selection <- summary(new_model)$which
  selection <- as.data.frame(rbind(rep(FALSE, ncol(selection)), selection))
  selection$`(Intercept)` <- NULL
  row.names(selection)[1] <- 0
  selection <- as.data.frame(as.matrix(selection) + 0)
  return(selection)
}

### This function combines all partial stability paths and determines the relative 
### frequency of each combination for a given variable-regularisation combination.
### Arguments:
### selected:   A list of all realisations of stability paths (see get_selected)
### Returns:    A dataframe / matrix with arbitrary number of different 
###             regularisation parameters (rows) and arbitrary number of
###             variables (columns). Each entry (float [0,1]) represents the 
###             relative selection probability for each regularisation-variable
###             combination.

make_paths <- function(selected) {
  selected <- Reduce("+", selected) / length(selected)
  return(selected)
}

#install.packages("ggplot2")
#install.packages("reshape2")
library(ggplot2)
library(reshape2)

### This function plots a line plot of the stability paths gropued by each 
### variable-regularisation combination.
### Arguments:
### stability_paths:  A dataframe / matrix with arbitrary number of different 
###                   regularisation parameters (rows) and arbitrary number of
###                   variables (columns). Each entry (float [0,1]) represents
###                   the respective relative frquency for each variable to be
###                   in the model for a given level of regularisation.
### Returns: A ggplot object which produces a line plot.

plot_stability_paths <- function(stability_paths){
  stability_paths <- cbind(melt(stability_paths), 
                           rep(0:(nrow(stability_paths) - 1), ncol(stability_paths)))
  names(stability_paths) <- c("variable", "probality", "regularisation")
  x <- stability_paths$regularisation
  y <- stability_paths$probality
  variable <- stability_paths$variable
  p <- ggplot(data = stability_paths, aes(x = x, y = y, group = variable))
  p <- p + geom_line(aes(color = variable), size = 2) + 
    geom_point(aes(color = variable), size = 3) + 
    labs(x = "# covariates", y = expression(pi)) + 
    theme(text = element_text(size = 35))
  return(p)
}
data(prostate)
data <- prostate
max_formula <- lpsa ~ (. - train)
model <-  regsubsets(max_formula,
                     data = data, nbest = 1, nvmax = 8,
                     really.big = TRUE
)

set.seed(20141020)
stability_paths <- get_stability_paths(model, data, reps = 10)

png("04-09-stabpath.png", width = 1000, height = 848)
plot_stability_paths(stability_paths)
dev.off()
