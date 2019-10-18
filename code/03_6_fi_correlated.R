#### R Code Chapter 10: PFI, LOCO and Correlated Features

## All libraries
library(mvtnorm)
library(matrixcalc)
library(mlr)
library(iml)
library(ggplot2)
library(randomForest)
library(ggpubr)
library(corrplot)
library(kernlab)
library(ggcorrplot)

### Example bikesharing 

# You can download the data set from http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
bike = read.csv("day.csv")

# For details on the data set please refer to https://christophm.github.io/interpretable-ml-book/bike-data.html

bike$weekday = factor(bike$weekday, levels=0:6, labels = c("SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT"))
bike$holiday = factor(bike$holiday, levels = c(0,1), labels = c("NO HOLIDAY", "HOLIDAY"))
bike$workingday = factor(bike$workingday, levels = c(0,1), labels = c("NO WORKING DAY", "WORKING DAY"))
bike$season = factor(bike$season, levels = 1:4, labels = c("SPRING", "SUMMER", "FALL", "WINTER"))
bike$weathersit = factor(bike$weathersit, levels = 1:3, labels = c("GOOD", "MISTY", "RAIN/SNOW/STORM"))


# Denormalize weather features:
# temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
bike$temp = bike$temp * (39 - (-8)) + (-8)
# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
bike$atemp = bike$atemp * (50 - (16)) + (16)


# Figure 10.1:
newbike <- bike[ ,c("dteday", "season", "holiday", "weekday", "workingday", "weathersit", "temp", "cnt")]
head(newbike) 

# Careful not exactly the same outcome as now seed was set
newbike2 <- transform(newbike, weekday = sample(weekday))
head(newbike2)


### Simulation section

## Simulation setting

# You should run the code of the simulation setting before runing the code of the scenarios 
# (please repeat it before every scenario)

## Simulation of uncorrelated features X1 and X2 (r=0):
set.seed(456) #to get same results

# specify entries for covariance matrix
sigma_u <- diag(1, nrow = 4)
# simulate data from multivariate normal distribution 
data_u <- as.data.frame(rmvnorm(n = 1000, 
                                  mean = rep(0, times = 4), 
                                  sigma = sigma_u))
colnames(data_u) <- c("X1", "X2", "X3", "X4")

# Added the uniformative feature "Uninf"
data_u <- as.data.frame(data_u)
data_u["Uninf"] <- runif(nrow(data_u))

# A covariance and correlation matrix should be positive definit
cov(data_u)
is.positive.definite(cov(data_u))
  
cor(data_u)
is.positive.definite(cor(data_u))


## Simulation of correlated features X1 and X2 (r=0.25)  
set.seed(456) #to get same results
# specify entries for covariance matrix
sigma_c25 <- diag(1, nrow = 4)
sigma_c25[1,2] <-  0.25
sigma_c25[2,1] <- 0.25
# simulate data from multivariate normal distribution 
data_c25 <- as.data.frame(rmvnorm(n = 1000, 
                                    mean = rep(0, times = 4), 
                                    sigma = sigma_c25))
colnames(data_c25) <- c("X1", "X2", "X3", "X4")

# Added the uniformative feature "Uninf"
data_c25 <- as.data.frame(data_c25)
data_c25["Uninf"] <- runif(nrow(data_c25))

# A covariance and correlation matrix should be positive definit
cov(data_c25)
is.positive.definite(cov(data_c25))
  
cor(data_c25)
is.positive.definite(cor(data_c25))
 
  
## Simulation of medium correlated features 1 and 2 (r=0.5)
set.seed(456) #to get same results
# specify entries for covariance matrix
sigma_c5 <- diag(1, nrow = 4)
sigma_c5[1,2] <-  0.5
sigma_c5[2,1] <- 0.5
# simulate data from multivariate normal distribution 
data_c5 <- as.data.frame(rmvnorm(n = 1000, 
                                   mean = rep(0, times = 4), 
                                   sigma = sigma_c5))
colnames(data_c5) <- c("X1", "X2", "X3", "X4")


# Added the uniformative feature "Uninf"
data_c5 <- as.data.frame(data_c5)
data_c5["Uninf"] <- runif(nrow(data_c5))

# A covariance and correlation matrix should be positive definit
cov(data_c5)
is.positive.definite(cov(data_c5))
  
cor(data_c5)
is.positive.definite(cor(data_c5))


## Simulation of high correlated features 1 and 2 (r=0.75)
set.seed(456) #to get same results
# specify entries for covariance matrix
sigma_c75 <- diag(1, nrow = 4)
sigma_c75[1,2] <-  0.75
sigma_c75[2,1] <- 0.75
# simulate data from multivariate normal distribution 
data_c75 <- as.data.frame(rmvnorm(n = 1000, 
                                    mean = rep(0, times = 4), 
                                    sigma = sigma_c75))
colnames(data_c75) <- c("X1", "X2", "X3", "X4")

# Added the uniformative feature "Uninf"
data_c75 <- as.data.frame(data_c75)
data_c75["Uninf"] <- runif(nrow(data_c75))

# A covariance and correlation matrix should be positive definit
cov(data_c75)
is.positive.definite(cov(data_c75))
  
cor(data_c75)
is.positive.definite(cor(data_c75))

  
## Simulation of complete correlated features 1 and 2 (r=0.99)
set.seed(456) #to get same results
# specify entries for covariance matrix
sigma_c <- diag(1, nrow = 4)
sigma_c[1,2] <-  0.99
sigma_c[2,1] <- 0.99
# simulate data from multivariate normal distribution 
data_c <- as.data.frame(rmvnorm(n = 1000, 
                                  mean = rep(0, times = 4), 
                                  sigma = sigma_c))
colnames(data_c) <- c("X1", "X2", "X3", "X4")

# Added the uniformative feature "Uninf"
data_c <- as.data.frame(data_c)
data_c["Uninf"] <- runif(nrow(data_c))

# A covariance and correlation matrix should be positive definit
cov(data_c)
is.positive.definite(cov(data_c))
  
cor(data_c)
is.positive.definite(cor(data_c))
  
## Scenario 1 Linear Dependence with differrent correlation of feature 1 and 2 

# Defining the variance of the error term epsilon

sd_u   <- abs(mean(data_u$X1 + data_u$X2 + data_u$X3 + data_u$X4))*0.1
sd_c25 <- abs(mean(data_c25$X1 + data_c25$X2 + data_c25$X3 + data_c25$X4))*0.1
sd_c5  <- abs(mean(data_c5$X1 + data_c5$X2 + data_c5$X3 + data_c5$X4))*0.1
sd_c75 <- abs(mean(data_c75$X1 + data_c75$X2 + data_c75$X3 + data_c75$X4))*0.1
sd_c   <- abs(mean(data_c$X1 + data_c$X2 + data_c$X3 + data_c$X4))*0.1

# Define the target value y: 

y1u   <- data_u$X1 + data_u$X2 + data_u$X3 + data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd_u) 
y1c25 <- data_c25$X1 + data_c25$X2 + data_c25$X3 + data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd_c25)
y1c5  <- data_c5$X1 + data_c5$X2 + data_c5$X3 + data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd_c5)
y1c75 <- data_c75$X1 + data_c75$X2 + data_c75$X3 + data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd_c75)
y1c   <- data_c$X1 + data_c$X2 + data_c$X3 + data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd_c) 

# Add the target to the data.frame
  
data_u["y1u"]     <- y1u
data_c25["y1c25"] <- y1c25
data_c5["y1c5"]   <- y1c5
data_c75["y1c75"] <- y1c75
data_c["y1c"]     <- y1c

# Benchmark results scenario 1) for all correlation intensities. 
tasks = list(makeRegrTask("p=0", data = data_u, target = "y1u"), makeRegrTask(id = "p=0.25",data = data_c25, target = "y1c25"),
             makeRegrTask(id = "p=0.5", data = data_c5, target = "y1c5"), makeRegrTask(id = "p=0.75",data = data_c75, target = "y1c75"),
             makeRegrTask(id = "p=0.99", data = data_c, target = "y1c"))

lrns = list(makeLearner(id="Random Forest", "regr.randomForest"),  
            makeLearner(id="SVM", "regr.ksvm"),makeLearner(id="Linear Model", "regr.lm"))
meas = list(mse,rsq, timetrain)
set.seed(456)
bmr01 = benchmark(lrns, tasks ,res_desc, meas , show.info = FALSE)

bmr01mse <- plotBMRBoxplots(bmr01, measure = mse, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmr01)) +
            aes(color = learner.id) +
            theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
                  legend.position = c(0.82, 0.3), plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
            labs(y = "MSE", x= "", colour = "Learners")

bmr01rsq <- plotBMRBoxplots(bmr01, measure = rsq, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmr01)) +
            aes(color = learner.id) +
            theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
                  legend.position = c(0.82, 0.3),plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
            labs(y = "Rsquared", x= "", colour = "Learners")

ggarrange(bmr01mse, bmr01rsq  , nrow=2)

# Benchmark results scenario 1), only for p = 0, 0.5 and 1 
tasks = list(makeRegrTask("p=0", data = data_u, target = "y1u"),
             makeRegrTask(id = "p=0.5", data = data_c5, target = "y1c5"),
             makeRegrTask(id = "p=0.99", data = data_c, target = "y1c"))

lrns = list(makeLearner(id="Random Forest", "regr.randomForest"),  
            makeLearner(id="SVM", "regr.ksvm"),makeLearner(id="Linear Model", "regr.lm"))
meas = list(mse,rsq, timetrain)
set.seed(456)
bmr01b = benchmark(lrns, tasks ,res_desc, meas , show.info = FALSE)

bmr01bmse <- plotBMRBoxplots(bmr01b, measure = mse, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmr01b)) +
             aes(color = learner.id) +
             theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
             legend.position = c(0.82, 0.3), plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
             labs(y = "MSE", x= "", colour = "Learners")


bmr01brsq <- plotBMRBoxplots(bmr01b, measure = rsq, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmr01b)) +
             aes(color = learner.id) +
             theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
             legend.position = c(0.82, 0.3),plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
             labs(y = "Rsquared", x= "", colour = "Learners")

# Figure 10.2:
ggarrange(bmr01bmse, bmr01brsq  , nrow = 2, common.legend = TRUE, legend = "bottom")
ggsave("bmr01b.png", width = 12, height = 8)

## Permutation Feature Importance Algorithm 
 
set.seed(456) # to get same results
res_desc <- makeResampleDesc("Subsample", iters = 10, split = 4/5) # Define the resampling method

# In principle you want to compare 2 models, once without the permuted feature and once with permuted feature. 
# Therefore we should use the same train-test splits in each iteration. 
# The output of the function is a data.frame containing the importance values averaged over the
# the 10 iteration (importance), the 0.05 - 0.95 quantile of the importance value based on the 
# 10 iterations (minimp-maximp), the importance rank of the importance value (rank) and the 
# average importance rank over the ten 10 iterations (averagerank)

PFI <- function(data,learner,target){
  d = data
  task = makeRegrTask(data = data, target = target)
  learnerPFI = makeLearner(learner)
  feat = getTaskFeatureNames(task)
  res = resample(learner = learnerPFI, task = task, 
                   resampling = res_desc, show.info = FALSE, models = TRUE)
  resultPFI = data.frame(matrix(nrow = length(feat), ncol = 1))
  rankPFI = data.frame(matrix(nrow = 5, ncol = 1))
  for(i in 1:10){
    mod = Predictor$new(res$models[[i]],
                           data = d[c(res$pred$instance$test.inds[[i]]),1:5], 
                           y = d[c(res$pred$instance$test.inds[[i]]),6])
    
    imp = FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo = imp$results
    impo = impo[order(impo$feature),]
    resultPFI[i] = data.frame(impo$importance)
    rankPFI[i] = data.frame(rank(-resultPFI[i]))
  }
  imp.dat = data.frame(rowSums(resultPFI)/10)
  rank.dat = data.frame(rowSums(rankPFI)/10)
  imp.dat["features"] = impo$feature
  imp.dat["averagerank"] = rank.dat
  imp.dat["rank"] = rank(-rowSums(resultPFI)/10)
  minimp0 = data.frame(apply(resultPFI, 1, quantile, probs = 0.05))
  maximp0 = data.frame(apply(resultPFI, 1, quantile, probs = 0.95))
  imp.dat["minimp"] = minimp0
  imp.dat["maximp"] = maximp0
  colnames(imp.dat) = c("importance","features","averagerank", "rank", "minimp", "maximp")
  return(imp.dat)
}

## Leave-One-Covariate-Out Algorithm

set.seed(456) # to get same results
res_desc <- makeResampleDesc("Subsample", iters = 10, split = 4/5) # Define the resampling method

# In principle you want to compare 2 models, once without the feature and once with the feature. 
# Therefore we should use the same train-test splits in each iteration. 
# The output of the function is a data.frame containing the importance values averaged over the
# the 10 iteration (importance), the 0.05 - 0.95 quantile of the importance value based on the 
# 10 iterations (minimp-maximp), the importance rank of the importance value (rank) and the 
# average importance rank over the ten 10 iterations (averagerank)

LOCO <- function(data,learner,target){
  d = data
  task = makeRegrTask(data = data , target = target)
  rin = makeResampleInstance("Subsample", iters = 10, split = 4/5, task = task)
  learnerLOCO = makeLearner(learner)
  feat = getTaskFeatureNames(task)
  res = resample(learner = learnerLOCO, task = task, 
                 resampling = rin ,show.info = FALSE)
  resultLOCO = data.frame(matrix(nrow = 1, ncol = length(feat)))
  resinstanceLOCO = data.frame(matrix(nrow = 10, ncol=length(feat)))
  for(i in 1:length(feat)){
    taskfeat = dropFeatures(task, feat[i])
    mod.feat = train(learnerLOCO, dropFeatures(task, feat[i]))
    resfeat = resample(learner = learnerLOCO, task = taskfeat, resampling = rin ,show.info = FALSE);
    importance = data.frame(resfeat$aggr/res$aggr)
    feature = c(getTaskFeatureNames(task))
    resultLOCO[i] = importance
    resinstanceLOCO[,i] = data.frame(resfeat$measures.test[,2]/res$measures.test[,2])
  }
  rankLOCO = data.frame(matrix(nrow = length(feat), ncol = 10))
  rank03 = rank(-resultLOCO)
  rownames(resultLOCO) = "importance"
  rankLOCO = data.frame(apply(-resinstanceLOCO, 1, rank))
  rank.datLOCO = data.frame(rowSums((rankLOCO)/10))
  minimp1 = data.frame(apply(resinstanceLOCO, 2, quantile, probs = 0.05))
  maximp1 = data.frame(apply(resinstanceLOCO, 2, quantile, probs = 0.95))
  rank = rank(-resultLOCO)
  FIP = data.frame(importance = t(resultLOCO),
                    feature = getTaskFeatureNames(task),
                    averagerank = rank.datLOCO,
                    rank = rank03,
                    minimp = minimp1,
                    maximp = maximp1)
  colnames(FIP) = c("importance", "features", "averagerank", "rank", "minimp", "maximp")
  rownames(FIP) = c(rep(1:length(feat)))
  return(FIP)
}


## PFI Scenario 1) RF

set.seed(456)
rfPFIu    <- PFI(data_u, "regr.randomForest", "y1u")
rfPFIc25  <- PFI(data_c25, "regr.randomForest", "y1c25")
rfPFIc5   <- PFI(data_c5, "regr.randomForest", "y1c5")
rfPFIc75  <- PFI(data_c75, "regr.randomForest", "y1c75")
rfPFIc    <- PFI(data_c, "regr.randomForest", "y1c")

# Making one data frame out of it
drfPFI = rbind(rfPFIu, rfPFIc25, rfPFIc5, rfPFIc75, rfPFIc)
drfPFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
drfPFI

save(drfPFI, file = "drfPFI.RData") # For usage in Markdown 

# Plot of importance values including quantile bands
  
prferr <- ggplot(data = drfPFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white",size = 0, shape = 21, 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x = "") + 
  ylim(0,10)+
  labs(color="Correlation:")  

# Average rank plot

prfrank01 <- ggplot(drfPFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 2.5, linetype = "dashed")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
  theme(legend.title = element_blank())+ 
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.3:
ggarrange(prferr, prfrank01 , ncol = 2, legend = "bottom", widths = c(2, 1))


## Extrapolation:

# Running random forest on the highly data set
set.seed(456)
extrapolation_task = makeRegrTask(data = data_c, target = "y1c")
rf_learner = makeLearner("regr.randomForest")

save(extrapolation_task, file = "exptask.RData" ) # For usage in Markdown
save(rf_learner, file = "exp.RData")            #For usage in Markdown

# Visualization of the prediction and data distribution
exp <- plotLearnerPrediction(rf_learner, extrapolation_task)+
        geom_segment(aes(x = -2, y = -2, xend = 2.3, yend = -2),color="white",size=1.2,
               arrow = arrow(length = unit(0.5, "cm")))+
        geom_point(aes(x = 2.5, y=-2), colour="blue")+
        ggtitle("Random Forest on Correlated Data Set")


# Running random forest on the uncorrelated correlated data set
set.seed(456)
extrapolation_task01 = makeRegrTask(data = data_u, target = "y1u")
rf_learner01 = makeLearner("regr.randomForest")

save(extrapolation_task01,file = "exptask01.RData" ) # For usage in Markdown
save(rf_learner01, file = "exp01.RData") # For usage in Markdown

# Visualization of the prediction and data distribution
exp01 <- plotLearnerPrediction(rf_learner01, extrapolation_task01)+
          geom_segment(aes(x = -2, y = -2, xend = 2.3, yend = -2),color="white",size=1.2,
               arrow = arrow(length = unit(0.5, "cm")))+
          geom_point(aes(x = 2.5, y=-2), colour="blue")+
          ggtitle("Random Forest on Independent Data Set")


# Figure 10.4:
ggarrange(exp01, exp, ncol=2, legend = "bottom")


## PFI Scenario 1) SVM

set.seed(456)
svmPFIu   <- PFI(data_u, "regr.ksvm", "y1u")
svmPFIc25 <- PFI(data_c25, "regr.ksvm", "y1c25")
svmPFIc5  <- PFI(data_c5, "regr.ksvm", "y1c5")
svmPFIc75 <- PFI(data_c75, "regr.ksvm", "y1c75")
svmPFIc   <- PFI(data_c, "regr.ksvm", "y1c")

# Making one data frame out of it
dsvmPFI = rbind(svmPFIu, svmPFIc25, svmPFIc5, svmPFIc75, svmPFIc)
dsvmPFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99),each = 5))
dsvmPFI

save(dsvmPFI, file = "dsvmPFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

psvmerr <- ggplot(data = dsvmPFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp))+
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x= "")+
  labs(color = "Correlation:") 

# Average rank plot

psvmrank01 <- ggplot(dsvmPFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 2.5, linetype="dashed")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14),legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.5:
ggarrange(psvmerr, psvmrank01 , ncol=2, legend = "bottom", widths = c(2, 1))

## PFI Scenario 1) LM

set.seed(456)
lmPFIu    <- PFI(data_u, "regr.lm", "y1u")
lmPFIc25  <- PFI(data_c25, "regr.lm", "y1c25")
lmPFIc5   <- PFI(data_c5, "regr.lm", "y1c5")
lmPFIc75  <- PFI(data_c75, "regr.lm", "y1c75")
lmPFIc    <- PFI(data_c, "regr.lm", "y1c")

# Making one data frame out of it
dlmPFI = rbind(lmPFIu, lmPFIc25, lmPFIc5, lmPFIc75, lmPFIc)
dlmPFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dlmPFI

save(dlmPFI, file = "dlmPFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plmerr <- ggplot(data = dlmPFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke= 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values =c ("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x= "")+
  labs(color="Correlation:")  

# Average rank plot

plmrank01 <- ggplot(dlmPFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 2.5, linetype = "dashed")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.6:
ggarrange(plmerr, plmrank01 , ncol = 2, legend = "bottom", widths = c(2, 1))

## LOCO Scenario 1) RF

set.seed(456)
rfLOCOua   <- LOCO(data_u, "regr.randomForest", "y1u")
rfLOCOc25a <- LOCO(data_c25, "regr.randomForest", "y1c25")
rfLOCOc5a  <- LOCO(data_c5, "regr.randomForest", "y1c5")
rfLOCOc75a <- LOCO(data_c75, "regr.randomForest", "y1c75")
rfLOCOca   <- LOCO(data_c, "regr.randomForest", "y1c")

# Making one data frame out of it
drfLOCO = rbind(rfLOCOua, rfLOCOc25a, rfLOCOc5a, rfLOCOc75a, rfLOCOca)
drfLOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99),each = 5))
drfLOCO

save(drfLOCO, file = "drfLOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plocoerr <- ggplot(drfLOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp))+ 
  geom_point(fill = "white", size = 0, shape = 21 , 
             stroke= 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:")

# Average rank plot

plocorank01 <- ggplot(drfLOCO, aes(x = averagerank , y = Corr, colour = features, group = features))+
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 2.5, linetype = "dashed")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.7 
ggarrange(plocoerr, plocorank01 , ncol = 2, legend = "bottom", widths = c(2, 1))


## LOCO Scenario 1) SVM

set.seed(456)
svmLOCOua   <- LOCO(data_u, "regr.ksvm", "y1u")
svmLOCOc25a <- LOCO(data_c25, "regr.ksvm", "y1c25")
svmLOCOc5a  <- LOCO(data_c5, "regr.ksvm", "y1c5")
svmLOCOc75a <- LOCO(data_c75, "regr.ksvm", "y1c75")
svmLOCOca   <- LOCO(data_c, "regr.ksvm", "y1c")

# Making one data frame out of it
dsvmLOCO = rbind(svmLOCOua, svmLOCOc25a, svmLOCOc5a, svmLOCOc75a, svmLOCOca)
dsvmLOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dsvmLOCO

save(dsvmLOCO, file = "dsvmLOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands
plocosvmerr <- ggplot(dsvmLOCO, aes(x = features, y = importance, colour = Corr, ymin= minimp, ymax=maximp)) +      geom_point(fill = "white", size = 0, shape = 21 , 
                                                                                                                               stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:")  

# Average rank plot

plocorank02 <- ggplot(dsvmLOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 2.5, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.8
ggarrange(plocosvmerr, plocorank02 , ncol = 2, legend = "bottom", widths = c(2, 1))


## LOCO Scenario 1) LM
  
set.seed(456)
lmLOCOua    <- LOCO(data_u, "regr.lm", "y1u")
lmLOCOc25a  <- LOCO(data_c25, "regr.lm", "y1c25")
lmLOCOc5a   <- LOCO(data_c5, "regr.lm", "y1c5")
lmLOCOc75a  <- LOCO(data_c75, "regr.lm", "y1c75")
lmLOCOca    <- LOCO(data_c, "regr.lm", "y1c")

# Making one data frame out of it
dlmLOCO = rbind(lmLOCOua, lmLOCOc25a, lmLOCOc5a, lmLOCOc75a, lmLOCOca)
dlmLOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99),each = 5))
dlmLOCO

save(dlmLOCO, file = "dlmLOCO.RData") # For usage in Markdown
  
# Plot of importance values including quantile bands
plocolmerr <- ggplot(dlmLOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) +
  geom_point(fill = "white", size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:")

# Average rank plot

plocorank03 <- ggplot(dlmLOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 2.5, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14),legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.9
ggarrange(plocolmerr, plocorank03 , ncol = 2, legend = "bottom", widths = c(2, 1))


## Scenario 2) Linear Dependence with a larger coefficient for $X4$:

# Defining the variance of the error term epsilon

sd2_u <- abs(mean(data_u$X1 + data_u$X2 + data_u$X3 + 1.2*data_u$X4))*0.1
sd2_c25 <- abs(mean(data_c25$X1 + data_c25$X2 + data_c25$X3 + 1.2*data_c25$X4))*0.1
sd2_c5 <- abs(mean(data_c5$X1 + data_c5$X2 + data_c5$X3 + 1.2*data_c5$X4))*0.1
sd2_c75 <- abs(mean(data_c75$X1 + data_c75$X2 + data_c75$X3 + 1.2*data_c75$X4))*0.1
sd2_c <- abs(mean(data_c$X1 + data_c$X2 + data_c$X3 + 1.2*data_c$X4))*0.1

# Define the target value y: 

y2u <- data_u$X1 + data_u$X2 + data_u$X3 + 1.2*data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd2_u) 
y2c25 <- data_c25$X1 + data_c25$X2 + data_c25$X3 + 1.2*data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c25)
y2c5 <- data_c5$X1 + data_c5$X2 + data_c5$X3 + 1.2*data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c5)
y2c75 <- data_c75$X1 + data_c75$X2 + data_c75$X3 + 1.2*data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c75)
y2c <- data_c$X1 + data_c$X2 + data_c$X3 + 1.2*data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c) 

# Add the target value to the data.frame

data_u["y2u"] <- y2u
data_c25["y2c25"] <- y2c25
data_c5["y2c5"] <- y2c5
data_c75["y2c75"] <- y2c75
data_c["y2c"] <- y2c

# # In case y1 still exists in your data.frame, you can use this or you rerun the simulation setting code

data_u$y1u <- NULL
data_c25$y1c25 <- NULL
data_c5$y1c5 <- NULL
data_c75$y1c75 <- NULL
data_c$y1c <- NULL


# Benchmarking
tasks = list(makeRegrTask("p=0", data = data_u, target = "y2u"), 
             makeRegrTask(id = "p=0.25",data = data_c25, target = "y2c25"),
             makeRegrTask(id = "p=0.5", data = data_c5, target = "y2c5"),
             makeRegrTask(id = "p=0.75",data = data_c75, target = "y2c75"),
             makeRegrTask(id = "p=0.99", data = data_c, target = "y2c"))

lrns = list(makeLearner(id="Random Forest", "regr.randomForest"),  makeLearner(id="Linear Model", "regr.lm"), 
            makeLearner(id="SVM", "regr.ksvm"))
meas = list(mse,rsq, timetrain)
bmr03 = benchmark(lrns, tasks ,res_desc,meas, show.info = FALSE, models = TRUE)
bmr03

## PFI Scenario 2) RF

set.seed(456)
rf2PFIu   <- PFI(data_u, "regr.randomForest", "y2u")
rf2PFIc25 <- PFI(data_c25, "regr.randomForest", "y2c25")
rf2PFIc5  <- PFI(data_c5, "regr.randomForest", "y2c5")
rf2PFIc75 <- PFI(data_c75, "regr.randomForest", "y2c75")
rf2PFIc   <- PFI(data_c, "regr.randomForest", "y2c")

# Making one data frame out of it
drf2PFI = rbind(rf2PFIu, rf2PFIc25, rf2PFIc5, rf2PFIc75, rf2PFIc)
drf2PFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
drf2PFI

save(drf2PFI, file = "drf2PFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

prf2err <- ggplot(drf2PFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke= 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x = "") + 
  ylim(0,10)+
  labs(color = "Correlation:") 

# Average rank plot

prfrank04 <- ggplot(drf2PFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size=  2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 3, linetype = "dashed")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")


# Figure 10.10
ggarrange(prf2err, prfrank04 , ncol=2, legend = "bottom", widths = c(2, 1))

## PFI Scenario 2) SVM

set.seed(456)
svm2PFIu <- PFI(data_u, "regr.ksvm", "y2u")
svm2PFIc25 <- PFI(data_c25, "regr.ksvm", "y2c25")
svm2PFIc5 <- PFI(data_c5, "regr.ksvm", "y2c5")
svm2PFIc75 <- PFI(data_c75, "regr.ksvm", "y2c75")
svm2PFIc <- PFI(data_c, "regr.ksvm", "y2c")

# Making one data frame out of it
dsvm2PFI = rbind(svm2PFIu, svm2PFIc25, svm2PFIc5, svm2PFIc75, svm2PFIc)
dsvm2PFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dsvm2PFI

save(dsvm2PFI, file = "dsvm2PFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

psvm2err <- ggplot(dsvm2PFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x= "")+
  labs(color= "Correlation:")

# Average rank plot

psvmrank05 <- ggplot(dsvm2PFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 3, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14),legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")


# Figure 10.11
ggarrange(psvm2err, psvmrank05 , ncol = 2, legend = "bottom", widths = c(2, 1))

## PFI Scenario 2) LM

set.seed(456)
lm2PFIu <- PFI(data_u, "regr.lm", "y2u")
lm2PFIc25 <- PFI(data_c25, "regr.lm", "y2c25")
lm2PFIc5 <- PFI(data_c5, "regr.lm", "y2c5")
lm2PFIc75 <- PFI(data_c75, "regr.lm", "y2c75")
lm2PFIc <- PFI(data_c, "regr.lm", "y2c")

# Making one data frame out of it
dlm2PFI = rbind(lm2PFIu, lm2PFIc25, lm2PFIc5, lm2PFIc75, lm2PFIc)
dlm2PFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99),each = 5))
dlm2PFI

save(dlm2PFI, file = "dlm2PFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plm2err <- ggplot(dlm2PFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white", size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x= "")+
  labs(color = "Correlation:") 

# Average rank plot

plmrank06 <- ggplot(dlm2PFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 3, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
  theme(legend.title = element_blank())+ 
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.12
ggarrange(plm2err, plmrank06 , ncol = 2, legend = "bottom", widths = c(2, 1))

## LOCO Scenario 2) RF

set.seed(456)

rf2LOCOua   <- LOCO(data_u, "regr.randomForest", "y2u")
rf2LOCOc25a <- LOCO(data_c25, "regr.randomForest", "y2c25")
rf2LOCOc5a  <- LOCO(data_c5, "regr.randomForest", "y2c5")
rf2LOCOc75a <- LOCO(data_c75, "regr.randomForest", "y2c75")
rf2LOCOca   <- LOCO(data_c, "regr.randomForest", "y2c")

# Making one data frame out of it
drf2LOCO = rbind(rf2LOCOua, rf2LOCOc25a, rf2LOCOc5a, rf2LOCOc75a, rf2LOCOca)
drf2LOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
drf2LOCO

save(drf2LOCO, file = "drf2LOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

ploco2err <- ggplot(drf2LOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white", size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color="Correlation:") 

# Average rank plot

plocorank04 <- ggplot(drf2LOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 3, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14),legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.13
ggarrange(ploco2err, plocorank04 , ncol = 2, legend = "bottom", widths = c(2, 1))

## LOCO Scenario 2) SVM

set.seed(456)
svm2LOCOua <- LOCO(data_u, "regr.ksvm", "y2u")
svm2LOCOc25a <- LOCO(data_c25, "regr.ksvm", "y2c25")
svm2LOCOc5a <- LOCO(data_c5, "regr.ksvm", "y2c5")
svm2LOCOc75a <- LOCO(data_c75, "regr.ksvm", "y2c75")
svm2LOCOca <- LOCO(data_c, "regr.ksvm", "y2c")

# Making one data frame out of it
dsvm2LOCO = rbind(svm2LOCOua, svm2LOCOc25a, svm2LOCOc5a, svm2LOCOc75a, svm2LOCOca)
dsvm2LOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99),each = 5))
dsvm2LOCO

save(dsvm2LOCO, file = "dsvm2LOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plocosvm2err <- ggplot(dsvm2LOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp))+   geom_point(fill = "white", size = 0, shape = 21 , 
                                                                                                                                stroke= 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:")

# Average rank plot

plocorank05 <- ggplot(dsvm2LOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 3, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")


# Figure 10.14
ggarrange(plocosvm2err, plocorank05 , ncol = 2, legend = "bottom", widths = c(2, 1))

## LOCO Scenario 2) LM

set.seed(456)
lm2LOCOua   <- LOCO(data_u, "regr.lm", "y2u")
lm2LOCOc25a <- LOCO(data_c25, "regr.lm", "y2c25")
lm2LOCOc5a  <- LOCO(data_c5, "regr.lm", "y2c5")
lm2LOCOc75a <- LOCO(data_c75, "regr.lm", "y2c75")
lm2LOCOca   <- LOCO(data_c, "regr.lm", "y2c")

# Making one data frame out of it
dlm2LOCO = rbind(lm2LOCOua, lm2LOCOc25a, lm2LOCOc5a, lm2LOCOc75a, lm2LOCOca)
dlm2LOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dlm2LOCO

save(dlm2LOCO, file = "dlm2LOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plocolm2err <- ggplot(dlm2LOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) +
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke= 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue",
                                 "0.75"="navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:")  

# Average rank plot

plocorank06 <- ggplot(dlm2LOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 3, linetype = "dashed")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.15
ggarrange(plocolm2err, plocorank06 , ncol = 2, legend = "bottom", widths = c(2, 1))


## Scenario 3) Non-Linear Dependence:

# Defining the variance of the error term epsilon

sd3_u <- abs(mean(sin(data_u$X1) + data_u$X2 + sin(data_u$X3) + data_u$X4))*0.1
sd3_c25 <- abs(mean(sin(data_c25$X1) + data_c25$X2 + sin(data_c25$X3)) + data_c25$X4)*0.1
sd3_c5 <- abs(mean(sin(data_c5$X1) + data_c5$X2 + sin(data_c5$X3) + data_c5$X4))*0.1
sd3_c75 <- abs(mean(sin(data_c75$X1) + data_c75$X2 + sin(data_c75$X3) + data_c75$X4))*0.1
sd3_c <- abs(mean(sin(data_c$X1) + data_c$X2 + sin(data_c$X3) + data_c$X4))*0.1
  
  
y3u <- sin(data_u$X1) + data_u$X2 + sin(data_u$X3) + data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd3_u) 
y3c25 <- sin(data_c25$X1) + data_c25$X2 + sin(data_c25$X3) + data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c25)
y3c5 <- sin(data_c5$X1) + data_c5$X2 + sin(data_c5$X3) + data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c5)
y3c75 <- sin(data_c75$X1) + data_c75$X2 + sin(data_c75$X3) + data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c75)
y3c <- sin(data_c$X1) + data_c$X2 + sin(data_c$X3) + data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c)  

data_u["y3u"] <- y3u
data_c25["y3c25"] <- y3c25
data_c5["y3c5"] <- y3c5
data_c75["y3c75"] <- y3c75
data_c["y3c"] <- y3c

# In case y1 or y2 still exist in your data.frame, you can use this or you rerun the simulation setting code

data_u$y1u <- NULL
data_c25$y1c25 <- NULL
data_c5$y1c5 <- NULL
data_c75$y1c75 <- NULL
data_c$y1c <- NULL

data_u$y2u <- NULL
data_c25$y2c25 <- NULL
data_c5$y2c5 <- NULL
data_c75$y2c75 <- NULL
data_c$y2c <- NULL

## Benchmark results scenario 3), only for p = 0, 0.5 and 1 
tasks = list(makeRegrTask("p=0", data = data_u, target = "y3u"), 
             makeRegrTask(id = "p=0.5", data = data_c5, target = "y3c5"),
             makeRegrTask(id = "p=0.99", data = data_c, target = "y3c"))

set.seed(456)
lrns = list(makeLearner(id="Random Forest", "regr.randomForest"), makeLearner(id="SVM", "regr.ksvm"),
            makeLearner(id="Linear Model", "regr.lm"))
meas = list(mse,rsq, timetrain)
bmr04 = benchmark(lrns, tasks ,res_desc,meas, show.info = FALSE)


bmr04mse <- plotBMRBoxplots(bmr04, measure = mse, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmr04)) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
        legend.position = c(0.82, 0.3), plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
  labs(y = "MSE", x= "", colour = "Learners")

bmr04rsq <- plotBMRBoxplots(bmr04, measure = rsq, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmr04)) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
        legend.position = c(0.82, 0.3),plot.subtitle = element_text(size = 9), text = element_text(size = 14))+
  labs(y = "Rsquared", x= "", colour = "Learners")

# Figure 10.16
ggarrange(bmr04mse, bmr04rsq  , nrow=2 , common.legend = TRUE, legend = "bottom")
#ggsave("bmr04.png", width = 12, height = 8)


## PFI Scenario 3) RF

set.seed(456)
rf3PFIu <- PFI(data_u, "regr.randomForest", "y3u")
rf3PFIc25 <- PFI(data_c25, "regr.randomForest", "y3c25")
rf3PFIc5 <- PFI(data_c5, "regr.randomForest", "y3c5")
rf3PFIc75 <- PFI(data_c75, "regr.randomForest", "y3c75")
rf3PFIc <- PFI(data_c, "regr.randomForest", "y3c")

# Making one data frame out of it
drf3PFI = rbind(rf3PFIu, rf3PFIc25, rf3PFIc5, rf3PFIc75, rf3PFIc)
drf3PFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
drf3PFI

save(drf3PFI, file = "drf3PFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

prf3err <- ggplot(drf3PFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white",size=0, shape=21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x = "") + 
  ylim(0,10)+
  labs(color = "Correlation:")

# Average rank plot

prfrank07 <- ggplot(drf3PFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group=features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 1.5, linetype = "dashed")+
  geom_vline(xintercept = 3.5, linetype = "dotted")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")


# Figure 10.17
ggarrange(prf3err, prfrank07 , ncol = 2, legend = "bottom", widths = c(2, 1))

## PFI Scenario 3) SVM

set.seed(456)
svm3PFIu    <- PFI(data_u, "regr.ksvm", "y3u")
svm3PFIc25  <- PFI(data_c25, "regr.ksvm", "y3c25")
svm3PFIc5   <- PFI(data_c5, "regr.ksvm", "y3c5")
svm3PFIc75  <- PFI(data_c75, "regr.ksvm", "y3c75")
svm3PFIc    <- PFI(data_c, "regr.ksvm", "y3c")

# Making one data frame out of it
dsvm3PFI = rbind(svm3PFIu, svm3PFIc25,svm3PFIc5, svm3PFIc75, svm3PFIc)
dsvm3PFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dsvm3PFI

save(dsvm3PFI, file = "dsvm3PFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

psvm3err <- ggplot(dsvm3PFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white", size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values=c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                             "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x = "")+
  labs(color = "Correlation:") 

# Average rank plot

psvmrank08 <- ggplot(dsvm3PFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group=features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 1.5, linetype = "dashed")+
  geom_vline(xintercept = 3.5, linetype = "dotted")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.18
ggarrange(psvm3err, psvmrank08 , ncol = 2, legend = "bottom", widths = c(2, 1))

## PFI Scenario 3) LM

set.seed(456)
lm3PFIu   <- PFI(data_u, "regr.lm", "y3u")
lm3PFIc25 <- PFI(data_c25, "regr.lm", "y3c25")
lm3PFIc5  <- PFI(data_c5, "regr.lm", "y3c5")
lm3PFIc75 <- PFI(data_c75, "regr.lm", "y3c75")
lm3PFIc   <- PFI(data_c, "regr.lm", "y3c")

# Making one data frame out of it
dlm3PFI = rbind(lm3PFIu, lm3PFIc25, lm3PFIc5, lm3PFIc75, lm3PFIc)
dlm3PFI$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dlm3PFI

save(dlm3PFI, file = "dlm3PFI.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plm3err <- ggplot(dlm3PFI, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width  =0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "PFI (Loss: MSE)", x= "")+
  labs(color = "Correlation:")

# Average rank plot

plmrank09 <- ggplot(dlm3PFI, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group=features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 1.5, linetype = "dashed")+
  geom_vline(xintercept = 3.5, linetype = "dotted")+
  theme(legend.position="bottom", plot.subtitle = element_text(size = 9), 
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.19
ggarrange(plm3err, plmrank09 , ncol = 2, legend = "bottom", widths = c(2, 1))

## LOCO Scenario 3) RF

set.seed(456)
rf3LOCOua   <- LOCO(data_u, "regr.randomForest", "y3u")
rf3LOCOc25a <- LOCO(data_c25, "regr.randomForest", "y3c25")
rf3LOCOc5a  <- LOCO(data_c5, "regr.randomForest", "y3c5")
rf3LOCOc75a <- LOCO(data_c75, "regr.randomForest", "y3c75")
rf3LOCOca   <- LOCO(data_c, "regr.randomForest", "y3c")

# Making one data frame out of it
drf3LOCO = rbind(rf3LOCOua, rf3LOCOc25a, rf3LOCOc5a, rf3LOCOc75a, rf3LOCOca)
drf3LOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
drf3LOCO

save(drf3LOCO, file = "drf3LOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

ploco3err <- ggplot(drf3LOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) + 
  geom_point(fill = "white", size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x= "")+
  labs(color = "Correlation:") 

# Average rank plot

plocorank07 <- ggplot(drf3LOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group=features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 1.5, linetype = "dashed")+
  geom_vline(xintercept = 3.5, linetype = "dotted")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.20
ggarrange(ploco3err, plocorank07 , ncol = 2, legend = "bottom", widths = c(2, 1))


## LOCO Scenario 3) SVM

set.seed(456)
svm3LOCOua <- LOCO(data_u, "regr.ksvm", "y3u")
svm3LOCOc25a <- LOCO(data_c25, "regr.ksvm", "y3c25")
svm3LOCOc5a <- LOCO(data_c5, "regr.ksvm", "y3c5")
svm3LOCOc75a <- LOCO(data_c75, "regr.ksvm", "y3c75")
svm3LOCOca <- LOCO(data_c, "regr.ksvm", "y3c")

# Making one data frame out of it
dsvm3LOCO = rbind(svm3LOCOua, svm3LOCOc25a, svm3LOCOc5a, svm3LOCOc75a, svm3LOCOca)
dsvm3LOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dsvm3LOCO

save(dsvm3LOCO, file = "dsvm3LOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plocosvm3err <- ggplot(dsvm3LOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax=maximp))+    geom_point(fill = "white", size = 0, shape = 21 , 
                                                                                                                               stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:") 

# Average rank plot

plocorank08 <- ggplot(dsvm3LOCO, aes(x=averagerank , y=Corr, colour = features, group= features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 1.5, linetype = "dashed")+
  geom_vline(xintercept = 3.5, linetype = "dotted")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

# Figure 10.21
ggarrange(plocosvm3err, plocorank08 , ncol = 2, legend = "bottom", widths = c(2, 1))

## LOCO Scenario 3) LM

set.seed(456)
lm3LOCOua <- LOCO(data_u, "regr.lm", "y3u")
lm3LOCOc25a <- LOCO(data_c25, "regr.lm", "y3c25")
lm3LOCOc5a <- LOCO(data_c5, "regr.lm", "y3c5")
lm3LOCOc75a <- LOCO(data_c75, "regr.lm", "y3c75")
lm3LOCOca <- LOCO(data_c, "regr.lm", "y3c")

# Making one data frame out of it
dlm3LOCO = rbind(lm3LOCOua, lm3LOCOc25a, lm3LOCOc5a, lm3LOCOc75a, lm3LOCOca)
dlm3LOCO$Corr = factor(rep(c(0, 0.25, 0.5, 0.75, 0.99), each = 5))
dlm3LOCO

save(dlm3LOCO, file = "dlm3LOCO.RData") # For usage in Markdown

# Plot of importance values including quantile bands

plocolm3err <- ggplot(dlm3LOCO, aes(x = features, y = importance, colour = Corr, ymin = minimp, ymax = maximp)) +
  geom_point(fill = "white",size = 0, shape = 21 , 
             stroke = 2.5, show.legend = TRUE, position = position_dodge(width = 0.7)) +
  geom_errorbar(width = 0.5, size = 1, position = position_dodge(width = 0.7)) + 
  coord_flip() +
  scale_colour_manual(values = c("0" = "skyblue", "0.25" = "royalblue", "0.5" = "blue",
                                 "0.75" = "navy", "0.99" = "black"))+
  scale_fill_manual(values =c ("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue",
                               "0.75" = "navy", "0.99" = "black"))+
  theme(plot.subtitle = element_text(size = 9), text = element_text(size = 14), legend.position = "bottom") +
  labs(y = "LOCO (Loss: MSE)", x = "")+
  labs(color = "Correlation:") 

# Average rank plot

plocorank09 <- ggplot(dlm3LOCO, aes(x = averagerank , y = Corr, colour = features, group = features)) +
  geom_point(size = 2)+
  geom_path(aes(group = features), size = 1)+
  coord_flip() +
  scale_colour_manual(values = c("Uninf" = "#999999", "X1" = "darkred", "X2" = "red",
                                 "X3" = "#006600", "X4" = "#33CC33"))+
  scale_x_reverse()+
  geom_vline(xintercept = 1.5, linetype = "dashed")+
  geom_vline(xintercept = 3.5, linetype = "dotted")+
  theme(legend.position = "bottom", plot.subtitle = element_text(size = 9),
        text = element_text(size = 14), legend.title = element_blank())+
  xlab("Average Rank")+
  ylab("Correlation")

#Figure 10.22
ggarrange(plocolm3err, plocorank09 , ncol = 2, legend = "bottom", widths = c(2, 1))


### Real Data Application:

data("Boston", package = "MASS")

# Reducing the data set to 6 features
Boston <- Boston[ ,c("dis", "rm", "age", "nox", "lstat", "crim", "medv")]
save(Boston, file = "Boston.RData") # For usage in Markdown

# Correlation plot
corrplot.mixed(cor(Boston),lower="circle",upper="number")
corrplot.mixed(cor(Boston),lower="circle",upper="number", tl.cex = 1.7, number.font = 1, tl.pos="lt", number.cex=1.2)


# Benchmarking
tasks = list(makeRegrTask(data = Boston, target = "medv"))

lrns = list(makeLearner(id="Random Forest", "regr.randomForest"),   
            makeLearner(id="SVM", "regr.ksvm"),makeLearner(id="Linear Model", "regr.lm"))
meas = list(mse,rsq, timetrain)
set.seed(456)
bmreal = benchmark(lrns, tasks ,res_desc,meas, show.info = FALSE, models = TRUE)
bmreal


pbbmr <- plotBMRBoxplots(bmreal, measure = mse, pretty.names = FALSE, order.lrn = getBMRLearnerIds(bmreal)) +
  aes(color = learner.id) +
  theme(strip.text.x = element_text(size = 8),axis.text.x=element_blank(),
        legend.position = "right")+
  labs(y = "MSE", x= "", colour = "Learners")
pbbmr

# Corrplot for all features (including n_stat and dup_lstat)
data("Boston", package = "MASS")
Boston <- Boston[ ,c("dis","rm","age", "nox", "lstat","crim", "medv")]
Boston["dup_lstat"] <- Boston$lstat
sd_lstat <- abs(mean(Boston$lstat))
set.seed(456)
noise <- rnorm(length(Boston$dup_lstat), mean = 0, sd = sd_lstat)*0.3
Boston["n_lstat"] <- Boston$dup_lstat + noise
Boston <- Boston[ , c(1:4,6,5,8,9,7)]
head(Boston)
save(Boston, file = "Boston.RData") # For usage in Markdown

corr <- cor(Boston)
corb <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
                   lab = TRUE)


# Figure 10.23
ggarrange(pbbmr, corb, ncol=2,  legend = "bottom")
ggsave("bmreal.png", width = 12, height = 5)

## PFI original Boston

# We have to slightly modify the function from before, because we have more features

PFIb <- function(data,learner,target){
  d <- data
  task = makeRegrTask(data = data , target = target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc, show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow = 6, ncol = 1))
  rankPFI <- data.frame(matrix(nrow = 6, ncol = 1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]],
                         data=d[c(res$pred$instance$test.inds[[i]]),1:6], 
                         y = d[c(res$pred$instance$test.inds[[i]]),7])
    
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
    rankPFI[i] <- data.frame(rank(-resultPFI[i]))
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  rank.dat <- data.frame(rowSums(rankPFI)/10)
  imp.dat["features"]<- impo$feature
  imp.dat["averagerank"]<- rank.dat
  imp.dat["rank"] <- rank(-rowSums(resultPFI)/10)
  minimp0 <- data.frame(apply(resultPFI, 1, quantile, probs = 0.05))
  maximp0 <- data.frame(apply(resultPFI, 1, quantile, probs = 0.95))
  imp.dat["minimp"] <- minimp0
  imp.dat["maximp"] <- maximp0
  colnames(imp.dat) <- c("importance","features","averagerank", "rank", "minimp", "maximp")
  return(imp.dat)
}

# PFI on original data set

data("Boston", package = "MASS")
Boston <- Boston[ ,c("dis", "rm", "age", "nox", "lstat", "crim", "medv")]
set.seed(456)
BostonPFI01 <- PFIb(Boston,"regr.randomForest","medv")

save(BostonPFI01, file = "BostonPFI01.RData") # For usage in Markdown

pb1 <- ggplot(data = BostonPFI01, aes(x = reorder(features, importance), ymin = minimp, ymax = maximp)) + 
  geom_errorbar(width = 0.3, size = 0.8, color = "darkblue") + 
  geom_point(mapping = aes(x = features, y = importance), size = 1.1, shape = 21, fill = "white") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10), plot.title = element_text(size = 12)) +
  labs(y = "PFI (Loss MSE)", x = "") + 
  theme(text = element_text(size = 10))+
  ylim(0,7)+
  ggtitle("Boston")

## PFI on data set Boston with duplicate of lstat

data("Boston", package = "MASS")
Boston <- Boston[ ,c("dis", "rm", "age", "nox", "lstat", "crim", "medv")]
Boston["dup_lstat"] <- Boston$lstat
Boston <- Boston[ , c(1:6,8,7)] # rearrange variables

# Now one more feature, so again sligthly modifying the PFI function

PFIbd <- function(data,learner,target){
  d <- data
  task = makeRegrTask(data = data , target = target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc, show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow = 7, ncol = 1))
  rankPFI <- data.frame(matrix(nrow = 7, ncol = 1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]],
                         data=d[c(res$pred$instance$test.inds[[i]]), 1:7], 
                         y = d[c(res$pred$instance$test.inds[[i]]), 8])
    
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
    rankPFI[i] <- data.frame(rank(-resultPFI[i]))
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  rank.dat <- data.frame(rowSums(rankPFI)/10)
  imp.dat["features"]<- impo$feature
  imp.dat["averagerank"]<- rank.dat
  imp.dat["rank"] <- rank(-rowSums(resultPFI)/10)
  minimp0 <- data.frame(apply(resultPFI, 1, quantile, probs = 0.05))
  maximp0 <- data.frame(apply(resultPFI, 1, quantile, probs = 0.95))
  imp.dat["minimp"] <- minimp0
  imp.dat["maximp"] <- maximp0
  colnames(imp.dat) <- c("importance","features","averagerank", "rank", "minimp", "maximp")
  return(imp.dat)
}

set.seed(456)

# PFI on Boston + dup_lstat
BostonPFI02 <- PFIbd(Boston,"regr.randomForest","medv")

save(BostonPFI02, file = "BostonPFI02.RData") # For usage in Markdown

pb2 <- ggplot(data = BostonPFI02, aes(x = reorder(features, importance), ymin = minimp, ymax = maximp)) + 
  geom_errorbar(width = 0.3, size = 0.8, color = "darkblue") + 
  geom_point(mapping = aes(x = features, y = importance), size = 1.1, shape = 21, fill = "white") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10), plot.title = element_text(size = 12)) +
  labs(y = "PFI (Loss MSE)", x = "") + 
  theme(text = element_text(size = 10))+
  ylim(0,7)+
  ggtitle("Boston + dup_lstat")

## PFI on data set Boston + n_lstat
data("Boston", package = "MASS")

Boston <- Boston[ ,c("dis","rm","age", "nox", "lstat","crim", "medv")]
Boston["dup_lstat"] <- Boston$lstat
sd_lstat <- abs(mean(Boston$lstat))
noise <- rnorm(length(Boston$dup_lstat), mean = 0, sd = sd_lstat)*0.3
Boston["n_lstat"] <- Boston$dup_lstat + noise
Boston["dup_lstat"] <- NULL
Boston <- Boston[ , c(1:6,8,7)]

# We can use the same function for PFI as before (7 features)
set.seed(456)
BostonPFI03 <- PFIbd(Boston,"regr.randomForest","medv")
save(BostonPFI03, file = "BostonPFI03.RData") # For usage in Markdown

pb3 <- ggplot(data = BostonPFI03, aes(x = reorder(features, importance), ymin = minimp, ymax = maximp)) + 
  geom_errorbar(width = 0.3, size = 0.8, color = "darkblue") + 
  geom_point(mapping = aes(x = features, y = importance), size = 1.1, shape = 21, fill = "white") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10), plot.title = element_text(size = 12)) +
  labs(y = "PFI (Loss MSE)", x = "") + 
  theme(text = element_text(size = 10))+
  ylim(0,7)+
  ggtitle("Boston + n_lstat")

# Figure 10.24
ggarrange(pb1, pb2, pb3, ncol = 3, widths = c(1, 1, 1))

## LOCO on original Boston data set:

data("Boston", package = "MASS")
Boston <- Boston[ ,c("dis", "rm", "age", "nox", "lstat", "crim", "medv")]

set.seed(456)
BostonLOCO01 <- LOCO(Boston, "regr.randomForest", "medv")
save(BostonLOCO01, file = "BostonLOCO01.RData") # For usage in Markdown

pb4 <- ggplot(data = BostonLOCO01, aes(x = reorder(features, importance), ymin = minimp, ymax = maximp)) + 
  geom_errorbar(width = 0.3, size = 0.8, color = "darkblue") + 
  geom_point(mapping = aes(x = features, y = importance), size = 1.1, shape = 21, fill = "white") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10), plot.title = element_text(size = 12)) +
  labs(y = "LOCO (Loss MSE)", x = "") + 
  theme(text = element_text(size = 10))+
  ylim(0.5,2.5)+
  ggtitle("Boston")


# LOCO on data set Boston + dup_lstat

data("Boston", package = "MASS")
Boston <- Boston[ ,c("dis","rm","age", "nox", "lstat","crim", "medv")]
Boston["dup_lstat"] <- Boston$lstat
BostonLOCO02 <- LOCO(Boston, "regr.randomForest", "medv")

save(BostonLOCO02, file = "BostonLOCO02.RData") # For usage in Markdown

pb5 <- ggplot(data = BostonLOCO02, aes(x = reorder(features, importance), ymin = minimp, ymax = maximp)) + 
  geom_errorbar(width = 0.3, size = 0.8, color = "darkblue") + 
  geom_point(mapping = aes(x=features, y = importance), size = 1.1, shape = 21, fill = "white") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10), plot.title = element_text(size = 12)) +
  labs(y = "LOCO (Loss MSE)", x = "") + 
  theme(text = element_text(size = 10))+
  ylim(0.5,2.5)+
  ggtitle("Boston + dup_lstat")


# LOCO on data set Boston + n_lstat

data("Boston", package = "MASS")
Boston <- Boston[ ,c("dis","rm","age", "nox", "lstat","crim", "medv")]
Boston["dup_lstat"] <- Boston$lstat
sd_lstat <- abs(mean(Boston$lstat))
noise <- rnorm(length(Boston$dup_lstat), mean = 0, sd = sd_lstat)*0.3
Boston["n_lstat"] <- Boston$dup_lstat + noise
Boston["dup_lstat"] <- NULL

BostonLOCO03 <- LOCO(Boston, "regr.randomForest", "medv")

save(BostonLOCO03, file = "BostonLOCO03.RData") # For usage in Markdown

pb6 <- ggplot(data = BostonLOCO03, aes(x = reorder(features, importance), ymin = minimp, ymax = maximp)) + 
  geom_errorbar(width = 0.3, size = 0.8, color = "darkblue") + 
  geom_point(mapping = aes(x=features, y = importance), size = 1.1, shape = 21, fill = "white") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10), plot.title = element_text(size = 12)) +
  labs(y = "LOCO (Loss MSE)", x = "") + 
  theme(text = element_text(size = 10))+
  ylim(0.5,2.5)+
  ggtitle("Boston + n_lstat")


# Figure 10.25
ggarrange(pb4, pb5, pb6, ncol = 3, widths = c(1, 1, 1))

