### Real Data Application 
### Boston Housing Data

library(MASS)
library(tidyverse)
library(mlr)
library(featureImportance)
library(gridExtra)

## packages for titanic dataset

install.packages("titanic")

## load package

library(titanic)

## Load Boston Housing Data
require(MASS)
data(BostonHousing, package = "mlbench")
str(BostonHousing)

## Specify and train ML algorithm 
#### Random Forest model






boston.task = makeRegrTask(data = BostonHousing, target = "medv")

# Specify the machine learning algorithm with the mlr package
lrn = makeLearner("regr.randomForest", ntree = 100)

# Create indices for train and test data
n = getTaskSize(boston.task)
train.ind = sample(n, size = 0.6*n)
test.ind = setdiff(1:n, train.ind)

## Create test data using test indices
test = getTaskData(boston.task, subset = test.ind)

## Fit model on train data using train indices
mod = train(lrn, boston.task, subset = train.ind)


## Use feature values of 20 randomly chosen observations from test data to plot the importance curves
obs.id = sample(1:nrow(test), 100)

BH_features <- as.vector(names(BostonHousing))
BH_predictors <- BH_features[1: (length(BH_features)-1)]

pfi_BH <- calculate_PFI(data = test, features = BH_predictors, target = "medv", mod = mod, mid ="mse", local = TRUE, replace.ids = obs.id)


## plot Partial Importance and Individual Conditional Importance


pi.curve_BH_lstat <- plotImportance(pfi_BH, feat ="lstat", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_lstat <- plotImportance(pfi_BH, feat ="lstat", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_lstat, ici.curves_BH_lstat, nrow = 1)

pi.curve_BH_rm <- plotImportance(pfi_BH, feat ="rm", mid = "mse", individual = FALSE, hline = TRUE) +
  expand_limits(c(5,8))
ici.curves_BH_rm <- plotImportance(pfi_BH, feat ="rm", mid = "mse", individual = TRUE, hline = FALSE) +
  expand_limits(c(5,8))
grid.arrange(pi.curve_BH_rm, ici.curves_BH_rm, nrow = 1)

pi.curve_BH_age <- plotImportance(pfi_BH, feat ="age", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_age <- plotImportance(pfi_BH, feat ="age", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_age, ici.curves_BH_age, nrow = 1)


pi.curve_BH_crim <- plotImportance(pfi_BH, feat ="crim", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_crim <- plotImportance(pfi_BH, feat ="crim", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_crim, ici.curves_BH_crim, nrow = 1)


## calculate derivative
d_ICI_BH_lstat <- dICI_2(data = pfi_BH, feature = "lstat", measure = "mse")
d_ICI_BH_rm <- dICI_2(data = pfi_BH, feature = "rm", measure = "mse")
d_ICI_BH_age <- dICI_2(data = pfi_BH, feature = "age", measure = "mse")
d_ICI_BH_crim <- dICI_2(data = pfi_BH, feature = "crim", measure = "mse")

## plot dICI

d_ICI_BH_lstat_plot <- dICI_plot(data = d_ICI_BH_lstat, feature = "lstat")
d_ICI_BH_rm_plot <- dICI_plot(data = d_ICI_BH_rm, feature = "rm")
d_ICI_BH_age_plot <- dICI_plot(data = d_ICI_BH_age, feature = "age")
d_ICI_BH_crim_plot <- dICI_plot(data = d_ICI_BH_crim, feature = "crim")

### test for Interaction Detection

ici_BH_test <- subset(pfi_BH, features == "lstat")
ici_BH_test.integral <- ici_BH_test[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]

## Interaction Detection 

interaction_BH <- PFI_interaction_identifier(pfi = pfi_BH, data = test, mid = "mse", features = c("lstat", "rm", "age"), model = "Decision Tree", tree_depth = 1)
interaction_BH_rf <- PFI_interaction_identifier(pfi = pfi_BH, data = test, mid = "mse", features = "lstat", model = "RandomForest", n_tree = 50)



## plot 

inter_BH_lstat_model <- interaction_BH$model_results.lstat
plot(as.party(inter_BH_lstat_model))
plot(as.party(interaction_BH$model_results["age"]))


lstat_imp <- varImpPlot(interaction_BH_rf$mod.learner.model)

# this part just creates the data.frame for the plot part

lstat_imp <- as.data.frame(lstat_imp)
lstat_imp$varnames <- rownames(lstat_imp) # row names to column
rownames(lstat_imp) <- NULL  
lstat_imp$var_categ <- rep(1, 12) # random var category

# this is the plot part, be sure to use reorder with the correct measure name

ggplot(lstat_imp, aes(x=reorder(varnames, IncNodePurity), weight=IncNodePurity, fill=as.factor(var_categ))) + 
  geom_bar() +
  scale_fill_discrete(name="Variable Group") +
  ylab("IncNodePurity") +
  xlab("Variable Name")


importance_lstat    <- importance(interaction_BH_rf$mod.learner.model)
varImportance_lstat <- data.frame(Variables = row.names(importance_lstat), 
                            Importance = round(importance_lstat[ ,'IncNodePurity'],2))

#Create a rank variable based on importance
rankImportance_lstat <- varImportance_lstat %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

#Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance_lstat, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() #+ 
  #theme_few()







### Conditionl Feature Importance for "lstat" and "dis"

ici_BH_lstat <- subset(pfi_BH, features == "lstat")
use0_BH_lstat <- pfi_BH[features == "dis" & feature.value >= 2.06, unique(replace.id)]
use1_BH_lstat <- pfi_BH[features == "dis" & feature.value < 2.06, unique(replace.id)]


ici_BH_lstat[["dis"]] <- as.factor(as.numeric(ici_BH_lstat$row.id %in% use1_BH_lstat))


pi_BH_lstat_0 <- subset(ici_BH_lstat, row.id %in% use0_BH_lstat & replace.id %in% use0_BH_lstat)
pi_BH_lstat_1 <- subset(ici_BH_lstat, row.id %in% use1_BH_lstat & replace.id %in% use1_BH_lstat)

by <- c("replace.id", "features", "feature.value", "dis")
pi_BH_lstat_0 <- pi_BH_lstat_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_BH_lstat_1 <- pi_BH_lstat_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]



cond_plot_aggr_BH_lstat <- plotImportance(pfi_BH, feat = "lstat", mid = "mse", hline = FALSE) +
  geom_line(data = pi_BH_lstat_0, aes(color = dis)) +
  geom_point(data = pi_BH_lstat_0, aes(color = dis)) +
  geom_line(data = pi_BH_lstat_1, aes(color = dis)) +
  geom_point(data = pi_BH_lstat_1, aes(color = dis)) +
  theme(legend.position = "bottom", legend.box = "vertical")


### add the condition that lstat < 10

use00_BH_lstat <- pfi_BH[features == "lstat" & feature.value <= 10, unique(replace.id)]
use01_BH_lstat <- pfi_BH[features == "lstat" & feature.value > 10, unique(replace.id)]

ici_BH_lstat[["lstat_cond"]] <- as.factor(as.numeric(ici_BH_lstat$row.id %in% use01_BH_lstat))
ici_BH_lstat[["factorC"]] <- with(ici_BH_lstat, interaction(dis,  lstat_cond))

ici_BH_lstat[["factorC"]] <- as.factor(as.numeric(ici_BH_lstat$factorC) - 1)

## factor 0: X_2 > 2 & X_3 = 0
## factor 1: X_2 <= 2 & X_3 = 1
## factor 2: X_2 <= 2 & X_3 = 0
## factor 3: X_2 

#pi_BH_lstat_0_0 <- subset(ici_BH_lstat, row.id %in% use0_BH_lstat & replace.id %in% use0_BH_lstat & row.id %in% use00_BH_lstat & replace.id %in% use00_BH_lstat)
#pi_BH_lstat_0_1 <- subset(ici_BH_lstat, row.id %in% use0_BH_lstat & replace.id %in% use0_BH_lstat & row.id %in% use01_BH_lstat & replace.id %in% use01_BH_lstat)
#pi_BH_lstat_1_0 <- subset(ici_BH_lstat, row.id %in% use1_BH_lstat & replace.id %in% use1_BH_lstat & row.id %in% use00_BH_lstat & replace.id %in% use00_BH_lstat)
#pi_BH_lstat_1_1 <- subset(ici_BH_lstat, row.id %in% use1_BH_lstat & replace.id %in% use1_BH_lstat & row.id %in% use01_BH_lstat & replace.id %in% use01_BH_lstat)


pi_BH_lstat_0_0 <- subset(ici_BH_lstat, row.id %in% use0_BH_lstat & replace.id %in% use0_BH_lstat & row.id %in% use00_BH_lstat)
pi_BH_lstat_0_1 <- subset(ici_BH_lstat, row.id %in% use0_BH_lstat & replace.id %in% use0_BH_lstat & row.id %in% use01_BH_lstat)
pi_BH_lstat_1_0 <- subset(ici_BH_lstat, row.id %in% use1_BH_lstat & replace.id %in% use1_BH_lstat & row.id %in% use00_BH_lstat)
pi_BH_lstat_1_1 <- subset(ici_BH_lstat, row.id %in% use1_BH_lstat & replace.id %in% use1_BH_lstat & row.id %in% use01_BH_lstat)


by <- c("replace.id", "features", "feature.value", "factorC")
pi_BH_lstat_0_0 <- pi_BH_lstat_0_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_BH_lstat_0_1 <- pi_BH_lstat_0_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_BH_lstat_1_0 <- pi_BH_lstat_1_0[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]
pi_BH_lstat_1_1 <- pi_BH_lstat_1_1[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = by]

## rename factor labels with name

levels(pi_BH_lstat_0_0$factorC)[pi_BH_lstat_0_0$factorC == "interaction 1"] <- "dis >= 2.06 & lstat <= 10"
levels(pi_BH_lstat_0_0$factorC)[pi_BH_lstat_0_0$factorC == "interaction 2"] <- "dis >= 2.06 & lstat > 10"
levels(pi_BH_lstat_0_0$factorC)[pi_BH_lstat_0_0$factorC == "interaction 3"] <- "dis < 2.06 & lstat <= 10"
levels(pi_BH_lstat_0_0$factorC)[pi_BH_lstat_0_0$factorC == "interaction 4"] <- "dis < 2.06 & lstat > 10"


levels(pi_BH_lstat_0_1$factorC)[pi_BH_lstat_0_1$factorC == "interaction 1"] <- "dis >= 2.06 & lstat <= 10"
levels(pi_BH_lstat_0_1$factorC)[pi_BH_lstat_0_1$factorC == "interaction 2"] <- "dis >= 2.06 & lstat > 10"
levels(pi_BH_lstat_0_1$factorC)[pi_BH_lstat_0_1$factorC == "interaction 3"] <- "dis < 2.06 & lstat <= 10"
levels(pi_BH_lstat_0_1$factorC)[pi_BH_lstat_0_1$factorC == "interaction 4"] <- "dis < 2.06 & lstat > 10"


levels(pi_BH_lstat_1_0$factorC)[pi_BH_lstat_1_0$factorC == "interaction 1"] <- "dis >= 2.06 & lstat <= 10"
levels(pi_BH_lstat_1_0$factorC)[pi_BH_lstat_1_0$factorC == "interaction 2"] <- "dis >= 2.06 & lstat > 10"
levels(pi_BH_lstat_1_0$factorC)[pi_BH_lstat_1_0$factorC == "interaction 3"] <- "dis < 2.06 & lstat <= 10"
levels(pi_BH_lstat_1_0$factorC)[pi_BH_lstat_1_0$factorC == "interaction 4"] <- "dis < 2.06 & lstat > 10"


levels(pi_BH_lstat_1_1$factorC)[pi_BH_lstat_1_1$factorC == "interaction 1"] <- "dis >= 2.06 & lstat <= 10"
levels(pi_BH_lstat_1_1$factorC)[pi_BH_lstat_1_1$factorC == "interaction 2"] <- "dis >= 2.06 & lstat > 10"
levels(pi_BH_lstat_1_1$factorC)[pi_BH_lstat_1_1$factorC == "interaction 3"] <- "dis < 2.06 & lstat <= 10"
levels(pi_BH_lstat_1_1$factorC)[pi_BH_lstat_1_1$factorC == "interaction 4"] <- "dis < 2.06 & lstat > 10"


cond_plot_dis_BH_lstat <- plotImportance(pfi_BH, feat = "lstat", mid = "mse", hline = FALSE) +
  geom_line(data = pi_BH_lstat_0_0, aes(color = factorC)) +
  geom_point(data = pi_BH_lstat_0_0, aes(color = factorC)) +
  geom_line(data = pi_BH_lstat_0_1, aes(color = factorC)) +
  geom_point(data = pi_BH_lstat_0_1, aes(color = factorC)) +
  geom_line(data = pi_BH_lstat_1_0, aes(color = factorC)) +
  geom_point(data = pi_BH_lstat_1_0, aes(color = factorC)) +
  geom_line(data = pi_BH_lstat_1_1, aes(color = factorC)) +
  geom_point(data = pi_BH_lstat_1_1, aes(color = factorC)) +
  theme(legend.position = "bottom", legend.box = "vertical")
  

cond_plot_dis_BH_lstat


cond_plot_BH_lstat_arranged <- grid.arrange(cond_plot_aggr_BH_lstat, cond_plot_dis_BH_lstat, nrow = 1)








## Real Data Simulation with Titanic Datas
## Goal: to detect and explain interaction between age and sex

data("Titanic")

titanic_data <- titanic_train

## Recode variables to factor 

titanic_data$Survived <- as.factor(titanic_data$Survived)
titanic_data$Pclass <- as.factor(titanic_data$Survived)
titanic_data$Sex <- as.factor(titanic_data$Sex)

## drop irrelevant variables

titanic_data$Name <- NULL
titanic_data$Cabin <- NULL
titanic_data$Ticket <- NULL


## Impute missing values of variable age

titanic_data$Age[which(is.na(titanic_data$Age))] <- mean(titanic_data$Age,na.rm = TRUE)
titanic_data[is.na(titanic_data$Age),]# "na" values have been replaces by mean of rest of the values


## Impute missing values of variable Embarked "" <- S
titanic_data[titanic_data$Embarked == "", "Embarked"] <- "S"

## Recode variable "Embarked" to factor

titanic_data$Embarked <- as.factor(titanic_data$Embarked)



head(titanic_data)



## Create Classification Task for Titanic dataset

titanic_task <- makeClassifTask(data = titanic_data, target = "Survived")


## Specify the machine learning algorithm with the mlr package
lrn_titanic = makeLearner("classif.randomForest", predict.type = "prob", fix.factors.prediction = TRUE)

# Create indices for train and test data

n_titanic = getTaskSize(titanic_task)
titanic_train.ind = sample(n_titanic, size = 0.8*n_titanic)
titanic_test.ind = setdiff(1:n_titanic, titanic_train.ind)

## Create test data using test indices
titanic_test = getTaskData(titanic_task, subset = titanic_test.ind)

## Fit model on train data using train indices
titanic_mod = train(lrn_titanic, titanic_task, subset = titanic_train.ind)


## Use feature values of 100 randomly chosen observations from test data to plot the importance curves
titanic_obs.id = sample(1:nrow(test), 100)



BH_features <- as.vector(names(BostonHousing))
BH_predictors <- BH_features[1: (length(BH_features)-1)]

pfi_titanic <- calculate_PFI(data = test, features = BH_predictors, target = "Survived", mod = mod, mid ="mse", local = TRUE, replace.ids = obs.id)


## plot Partial Importance and Individual Conditional Importance


pi.curve_BH_lstat <- plotImportance(pfi_BH, feat ="lstat", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_lstat <- plotImportance(pfi_BH, feat ="lstat", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_lstat, ici.curves_BH_lstat, nrow = 1)

pi.curve_BH_rm <- plotImportance(pfi_BH, feat ="rm", mid = "mse", individual = FALSE, hline = TRUE) +
  expand_limits(c(5,8))
ici.curves_BH_rm <- plotImportance(pfi_BH, feat ="rm", mid = "mse", individual = TRUE, hline = FALSE) +
  expand_limits(c(5,8))
grid.arrange(pi.curve_BH_rm, ici.curves_BH_rm, nrow = 1)

pi.curve_BH_age <- plotImportance(pfi_BH, feat ="age", mid = "mse", individual = FALSE, hline = TRUE)
ici.curves_BH_age <- plotImportance(pfi_BH, feat ="age", mid = "mse", individual = TRUE, hline = FALSE)
grid.arrange(pi.curve_BH_age, ici.curves_BH_age, nrow = 1)


## calculate derivative
d_ICI_BH_lstat <- dICI_2(data = pfi_BH, feature = "lstat", measure = "mse")
d_ICI_BH_rm <- dICI_2(data = pfi_BH, feature = "rm", measure = "mse")
d_ICI_BH_age <- dICI_2(data = pfi_BH, feature = "age", measure = "mse")


## plot dICI

d_ICI_BH_lstat_plot <- dICI_plot(data = d_ICI_BH_lstat, feature = "lstat")
d_ICI_BH_rm_plot <- dICI_plot(data = d_ICI_BH_rm, feature = "rm")
d_ICI_BH_age_plot <- dICI_plot(data = d_ICI_BH_age, feature = "age")


### test for Interaction Detection

ici_BH_test <- subset(pfi_BH, features == "lstat")
ici_BH_test.integral <- ici_BH_test[, lapply(.SD, mean, na.rm = TRUE), .SDcols = "mse", by = "row.id"]

## Interaction Detection 

interaction_BH <- PFI_interaction_identifier(pfi = pfi_BH, data = test, mid = "mse", features = c("lstat", "rm", "age"), model = "Decision Tree", tree_depth = 1)
interaction_BH_rf <- PFI_interaction_identifier(pfi = pfi_BH, data = test, mid = "mse", features = c("lstat", "rm", "age"), model = "RandomForest", n_tree = 50)

interaction

## plot 

inter_BH_age_model <- interaction_BH$model_results["age"]
plot(as.party(inter_BH_age_model))
plot(as.party(interaction_BH$model_results["age"]))









