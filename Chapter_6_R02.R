# All libraries
library(mvtnorm)
library(matrixcalc)
library(mlr)
library(iml)
library(ggplot2)
library(randomForest)
library(ggpubr)
library(corrplot)

# PFI, LOCO and Correlated Features

## Simulation
### Simulation of uncorrelated features 1 and 2 (r=0)

set.seed(456)
# specify entries for covariance matrix
sigma_u <- diag(1, nrow = 4)
# simulate data from normal distribution 
data_u <- as.data.frame(rmvnorm(n = 1000, 
                                  mean = rep(0, times = 4), 
                                  sigma = sigma_u))
colnames(data_u) <- c("X1", "X2", "X3", "X4")
# check covariance and correlation matrices
  
data_u <- as.data.frame(data_u)
data_u['Uninf'] <- runif(nrow(data_u))
  
cov(data_u)
is.positive.definite(cov(data_u))
  
cor(data_u)
is.positive.definite(cor(data_u))

### Simulation of correlated features 1 and 2 (r=0.25)  
  
set.seed(456)
# specify entries for covariance matrix
sigma_c25 <- diag(1, nrow = 4)
sigma_c25[1,2] <-  0.25
sigma_c25[2,1] <- 0.25
# simulate data from normal distribution 
data_c25 <- as.data.frame(rmvnorm(n = 1000, 
                                    mean = rep(0, times = 4), 
                                    sigma = sigma_c25))
colnames(data_c25) <- c("X1", "X2", "X3", "X4")
# check covariance and correlation matrices 
  
data_c25 <- as.data.frame(data_c25)
data_c25['Uninf'] <- runif(nrow(data_c25))
  
cov(data_c25)
is.positive.definite(cov(data_c25))
  
cor(data_c25)
is.positive.definite(cor(data_c25))
 
  
### Simulation of medium correlated features 1 and 2 (r=0.5)

set.seed(456)
# specify entries for covariance matrix
sigma_c5 <- diag(1, nrow = 4)
sigma_c5[1,2] <-  0.5
sigma_c5[2,1] <- 0.5
# simulate data from normal distribution 
data_c5 <- as.data.frame(rmvnorm(n = 1000, 
                                   mean = rep(0, times = 4), 
                                   sigma = sigma_c5))
colnames(data_c5) <- c("X1", "X2", "X3", "X4")
# check covariance and correlation matrices 
  
data_c5 <- as.data.frame(data_c5)
data_c5['Uninf'] <- runif(nrow(data_c5))
  
cov(data_c5)
is.positive.definite(cov(data_c5))
  
cor(data_c5)
is.positive.definite(cor(data_c5))

### Simulation of high correlated features 1 and 2 (r=0.75)
  

set.seed(456)
# specify entries for covariance matrix
sigma_c75 <- diag(1, nrow = 4)
sigma_c75[1,2] <-  0.75
sigma_c75[2,1] <- 0.75
# simulate data from normal distribution 
data_c75 <- as.data.frame(rmvnorm(n = 1000, 
                                    mean = rep(0, times = 4), 
                                    sigma = sigma_c75))
colnames(data_c75) <- c("X1", "X2", "X3", "X4")
# check covariance and correlation matrices 
  
data_c75 <- as.data.frame(data_c75)
data_c75['Uninf'] <- runif(nrow(data_c75))
  
cov(data_c75)
is.positive.definite(cov(data_c75))
  
cor(data_c75)
is.positive.definite(cor(data_c75))

  
### Simulation of complete correlated features 1 and 2 (r=0.99)
  
  
set.seed(456)
# specify entries for covariance matrix
sigma_c <- diag(1, nrow = 4)
sigma_c[1,2] <-  0.99
sigma_c[2,1] <- 0.99
# simulate data from normal distribution 
data_c <- as.data.frame(rmvnorm(n = 1000, 
                                  mean = rep(0, times = 4), 
                                  sigma = sigma_c))
colnames(data_c) <- c("X1", "X2", "X3", "X4")
# check covariance and correlation matrices 
data_c <- as.data.frame(data_c)
data_c['Uninf'] <- runif(nrow(data_c))
  
cov(data_c)
is.positive.definite(cov(data_c))
  
cor(data_c)
is.positive.definite(cor(data_c))

## Define the target value y:  
  
### Szenario Linear Dependence with differrent correlation of feature 1 and 2 
 
sd_u <- mean(data_u$X1 + data_u$X2 + data_u$X3 + data_u$X4)*0.1
sd_c25 <- mean(data_c25$X1 + data_c25$X2 + data_c25$X3 + data_c25$X4)*0.1
sd_c5 <- mean(data_c5$X1 + data_c5$X2 + data_c5$X3 + data_c5$X4)*0.1
sd_c75 <- mean(data_c75$X1 + data_c75$X2 + data_c75$X3 + data_c75$X4)*0.1
sd_c <- mean(data_c$X1 + data_c$X2 + data_c$X3 + data_c$X4)*0.1
  
#The choice of noise variance should be hold small in order to make the behavior we observe clearer and there will be no misinterpretation.
  
y1u <- data_u$X1 + data_u$X2 + data_u$X3 + data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd_u) 
y1c25 <- data_c25$X1 + data_c25$X2 + data_c25$X3 + data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd_c25)
y1c5 <- data_c5$X1 + data_c5$X2 + data_c5$X3 + data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd_c5)
y1c75 <- data_c75$X1 + data_c75$X2 + data_c75$X3 + data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd_c75)
y1c <- data_c$X1 + data_c$X2 + data_c$X3 + data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd_c) 
  
data_u["y1u"] <- y1u
data_c25["y1c25"] <- y1c25
data_c5["y1c5"] <- y1c5
data_c75["y1c75"] <- y1c75
data_c["y1c"] <- y1c

## Permutation Feature Importance Algorithm 
 
set.seed(456)
res_desc <- makeResampleDesc("Subsample", iters = 10, split = 4/5) #Defining the resampling method
  
  
PFI <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                   resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=5,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                           data=d[c(res$pred$instance$test.inds[[i]]),1:5], 
                           y = d[c(res$pred$instance$test.inds[[i]]),6])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}
  
## Leave-One-Covariate-Out Algorithmus

library(mlr)
library(iml)
library(ggplot2)
library(randomForest)
set.seed(456)
res_desc <- makeResampleDesc("Subsample", iters = 10, split = 4/5)
  
  
LOCO <- function(data,learner,target){
  d <- data
  task = makeRegrTask(data = data , target= target)
  rin <- makeResampleInstance("Subsample", iters = 10, split = 4/5, task = task)
  learnerLOCO = makeLearner(learner)
  feat = getTaskFeatureNames(task)
  res = resample(learner = learnerLOCO, task = task, 
                   resampling = rin ,show.info = FALSE)
  resultLOCO <- data.frame(matrix(nrow=1,ncol=length(feat))) 
  for(i in 1:length(feat)){
    taskfeat = dropFeatures(task, feat[i])
    mod.feat = train(learnerLOCO, dropFeatures(task, feat[i]))
    resfeat = resample(learner = learnerLOCO, task = taskfeat, resampling = rin ,show.info = FALSE);
    importance = data.frame(resfeat$aggr/res$aggr)
    feature = c(getTaskFeatureNames(task))
    resultLOCO[i] <- importance
    }
  resultLOCO
  rownames(resultLOCO) <- "importance"
  FIP <- data.frame(
    importance= t(resultLOCO),
    feature= getTaskFeatureNames(task))
  return(FIP)
}


### PFI with different correlations of features 1 and 2 on a random forest model

set.seed(456)
rfu <- randomForest(y1u~. , data = data_u)
rfc25 <- randomForest(y1c25~. , data = data_c25)
rfc5 <- randomForest(y1c5~. , data = data_c5)
rfc75 <- randomForest(y1c75~. , data = data_c75)
rfc <- randomForest(y1c~. , data = data_c)
  
  
rfPFIu <- PFI(data_u, "regr.randomForest", "y1u" , rfu)
rfPFIc25 <- PFI(data_c25, "regr.randomForest", "y1c25" , rfc25)
rfPFIc5 <- PFI(data_c5, "regr.randomForest", "y1c5" , rfc5)
rfPFIc75 <- PFI(data_c75, "regr.randomForest", "y1c75" , rfc75)
rfPFIc <- PFI(data_c, "regr.randomForest", "y1c" , rfc)
  
prf <- ggplot(rfPFIu, aes(x=features, y= importance, colour = "0")) +
    geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
    geom_point(mapping=aes(x=rfPFIc25$features, y=rfPFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
    geom_point(mapping=aes(x=rfPFIc5$features, y=rfPFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
    geom_point(mapping=aes(x=rfPFIc75$features, y=rfPFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
    geom_point(mapping=aes(x=rfPFIc$features, y=rfPFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
    coord_flip() +
    theme(plot.subtitle = element_text(size = 25)) +
    labs(y = "PFI (Loss: MSE)", x= "") + 
    theme(text = element_text(size = 30))+
    scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
    theme(
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
      panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
    theme(  legend.title = element_text( size = 30),
            legend.text = element_text(size = 30)  )+
    theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30,face="bold"))
  
  
prf
  
ggsave("PFI01.jpeg",width = 18, height = 8)
  
 
### PFI with different correlations of features 1 and 2 on a linear model

set.seed(456)
lmu <- lm(y1u~., data=data_u)
lmc25 <- lm(y1c25~., data = data_c25)
lmc5 <- lm(y1c5~., data = data_c5)
lmc75 <- lm(y1c75~., data = data_c75)
lmc <- lm(y1c~., data = data_c)
  
lmPFIu <- PFI(data_u, "regr.lm", "y1u" , lmu)
lmPFIc25 <- PFI(data_c25, "regr.lm", "y1c25" , lmc25)
lmPFIc5 <- PFI(data_c5, "regr.lm", "y1c5" , lmc5)
lmPFIc75 <- PFI(data_c75, "regr.lm", "y1c75" , lmc75)
lmPFIc <- PFI(data_c, "regr.lm", "y1c" , lmc)
  
#Plot:
plm <- ggplot(lmPFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=lmPFIc25$features, y=lmPFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmPFIc5$features, y=lmPFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmPFIc75$features, y=lmPFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmPFIc$features, y=lmPFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
        theme(legend.title = element_text( size = 30),
            legend.text = element_text(size = 30)  )+
        theme(axis.text=element_text(size=30),
            axis.title=element_text(size=30,face="bold"))
  
plm
  
ggsave("PFI02.jpeg",width = 18, height = 8)

  
## LOCO Feature Importance with different kind of correlation intensity on a random forest model
  
rfLOCOua <- LOCO(data_u, "regr.randomForest", "y1u")
rfLOCOc25a <- LOCO(data_c25, "regr.randomForest", "y1c25")
rfLOCOc5a <- LOCO(data_c5, "regr.randomForest", "y1c5")
rfLOCOc75a <- LOCO(data_c75, "regr.randomForest", "y1c75")
rfLOCOca <- LOCO(data_c, "regr.randomForest", "y1c")
  
#Plot:
ploco <- ggplot(data = rfLOCOua ,aes(x=feature,y = importance, colour = "0")) + 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=rfLOCOc25a$feature, y=rfLOCOc25a$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc5a$feature, y=rfLOCOc5a$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc75a$feature, y=rfLOCOc75a$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOca$feature, y=rfLOCOca$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))
  
ploco
  
ggsave("LOCO01.jpeg",width = 18, height = 8)
  
## LOCO Feature Importance with different kind of correlation intensity on a linear model
  
lmLOCOua <- LOCO(data_u, "regr.lm", "y1u")
lmLOCOc25a <- LOCO(data_c25, "regr.lm", "y1c25")
lmLOCOc5a <- LOCO(data_c5, "regr.lm", "y1c5")
lmLOCOc75a <- LOCO(data_c75, "regr.lm", "y1c75")
lmLOCOca <- LOCO(data_c, "regr.lm", "y1c")
  
#Plot:
plocolm <- ggplot(data = lmLOCOua ,aes(x=feature,y = importance, colour = "0")) +  
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=lmLOCOc25a$feature, y=lmLOCOc25a$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc5a$feature, y=lmLOCOc5a$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc75a$feature, y=lmLOCOc75a$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOca$feature, y=lmLOCOca$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
    theme(legend.title = element_text( size = 30),
          legend.text = element_text(size = 30)  )+
    theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30,face="bold"))
  
plocolm
  
ggsave("LOCO12.jpeg",width = 18, height = 8)
  
## Comparison Linear Dependencies 
library(ggpubr)
ggarrange(prf, plm, ploco, plocolm , ncol=2, nrow = 2, legend = "bottom", common.legend = TRUE)
  
ggsave("arrange01.jpeg",width = 18, height = 8)



### Szenario Linear Dependence with differrent correlation of feature 1 and 2 and X4 coefficient 1.2

sd_u <- mean(data_u$X1 + data_u$X2 + data_u$X3 + 1.2*data_u$X4)*0.1
sd_c25 <- mean(data_c25$X1 + data_c25$X2 + data_c25$X3 + 1.2*data_c25$X4)*0.1
sd_c5 <- mean(data_c5$X1 + data_c5$X2 + data_c5$X3 + 1.2*data_c5$X4)*0.1
sd_c75 <- mean(data_c75$X1 + data_c75$X2 + data_c75$X3 + 1.2*data_c75$X4)*0.1
sd_c <- mean(data_c$X1 + data_c$X2 + data_c$X3 + 1.2*data_c$X4)*0.1

#The choice of noise variance should be hold small in order to make the behavior we observe clearer and there will be no misinterpretation.

y1u <- data_u$X1 + data_u$X2 + data_u$X3 + 1.2*data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd_u) 
y1c25 <- data_c25$X1 + data_c25$X2 + data_c25$X3 + 1.2*data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd_c25)
y1c5 <- data_c5$X1 + data_c5$X2 + data_c5$X3 + 1.2*data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd_c5)
y1c75 <- data_c75$X1 + data_c75$X2 + data_c75$X3 + 1.2*data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd_c75)
y1c <- data_c$X1 + data_c$X2 + data_c$X3 + 1.2*data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd_c) 

data_u["y1u"] <- y1u
data_c25["y1c25"] <- y1c25
data_c5["y1c5"] <- y1c5
data_c75["y1c75"] <- y1c75
data_c["y1c"] <- y1c

### PFI with different correlations of features 1 and 2 on a random forest model

set.seed(456)
rfu <- randomForest(y1u~. , data = data_u)
rfc25 <- randomForest(y1c25~. , data = data_c25)
rfc5 <- randomForest(y1c5~. , data = data_c5)
rfc75 <- randomForest(y1c75~. , data = data_c75)
rfc <- randomForest(y1c~. , data = data_c)

rfPFIu <- PFI(data_u, "regr.randomForest", "y1u" , rfu)
rfPFIc25 <- PFI(data_c25, "regr.randomForest", "y1c25" , rfc25)
rfPFIc5 <- PFI(data_c5, "regr.randomForest", "y1c5" , rfc5)
rfPFIc75 <- PFI(data_c75, "regr.randomForest", "y1c75" , rfc75)
rfPFIc <- PFI(data_c, "regr.randomForest", "y1c" , rfc)

prf <- ggplot(rfPFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=rfPFIc25$features, y=rfPFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfPFIc5$features, y=rfPFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfPFIc75$features, y=rfPFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfPFIc$features, y=rfPFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30))+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

prf

ggsave("PFI11.jpeg",width = 18, height = 8)


### PFI with different correlations of features 1 and 2 on a linear model

set.seed(456)
lmu <- lm(y1u~., data=data_u)
lmc25 <- lm(y1c25~., data = data_c25)
lmc5 <- lm(y1c5~., data = data_c5)
lmc75 <- lm(y1c75~., data = data_c75)
lmc <- lm(y1c~., data = data_c)

lmPFIu <- PFI(data_u, "regr.lm", "y1u" , lmu)
lmPFIc25 <- PFI(data_c25, "regr.lm", "y1c25" , lmc25)
lmPFIc5 <- PFI(data_c5, "regr.lm", "y1c5" , lmc5)
lmPFIc75 <- PFI(data_c75, "regr.lm", "y1c75" , lmc75)
lmPFIc <- PFI(data_c, "regr.lm", "y1c" , lmc)

#Plot:
plm <- ggplot(lmPFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=lmPFIc25$features, y=lmPFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmPFIc5$features, y=lmPFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmPFIc75$features, y=lmPFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmPFIc$features, y=lmPFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

plm

ggsave("PFI12.jpeg",width = 18, height = 8)

## LOCO Feature Importance with different kind of correlation intensity on a random forest model
rfLOCOua <- LOCO(data_u, "regr.randomForest", "y1u")
rfLOCOc25a <- LOCO(data_c25, "regr.randomForest", "y1c25")
rfLOCOc5a <- LOCO(data_c5, "regr.randomForest", "y1c5")
rfLOCOc75a <- LOCO(data_c75, "regr.randomForest", "y1c75")
rfLOCOca <- LOCO(data_c, "regr.randomForest", "y1c")

#Plot:
ploco <- ggplot(data = rfLOCOua ,aes(x=feature,y = importance, colour = "0")) + 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=rfLOCOc25a$feature, y=rfLOCOc25a$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc5a$feature, y=rfLOCOc5a$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc75a$feature, y=rfLOCOc75a$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOca$feature, y=rfLOCOca$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

ploco

ggsave("LOCO11.jpeg",width = 18, height = 8)
## LOCO Feature Importance with different kind of correlation intensity on a linear model

lmLOCOua <- LOCO(data_u, "regr.lm", "y1u")
lmLOCOc25a <- LOCO(data_c25, "regr.lm", "y1c25")
lmLOCOc5a <- LOCO(data_c5, "regr.lm", "y1c5")
lmLOCOc75a <- LOCO(data_c75, "regr.lm", "y1c75")
lmLOCOca <- LOCO(data_c, "regr.lm", "y1c")

#Plot:
plocolm <- ggplot(data = lmLOCOua ,aes(x=feature,y = importance, colour = "0")) + 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=lmLOCOc25a$feature, y=lmLOCOc25a$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc5a$feature, y=lmLOCOc5a$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc75a$feature, y=lmLOCOc75a$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOca$feature, y=lmLOCOca$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

plocolm

ggsave("LOCO12.jpeg",width = 18, height = 8)



## Comparison Linear Dependencies 
library(ggpubr)
ggarrange(prf, plm, ploco, plocolm , ncol=2, nrow = 2, legend = "bottom", common.legend = TRUE)

ggsave("arrange01.jpeg",width = 18, height = 8)

### 2) Nonlinear Dependence - Simulation of Nonlinear Dependence:

sd2_u <- mean(sin(data_u$X1) + data_u$X2 + sin(data_u$X3) + data_u$X4)*0.1
  
sd2_c25 <- mean(sin(data_c25$X1) + data_c25$X2 + sin(data_c25$X3) + data_c25$X4)*0.1
  
sd2_c5 <- mean(sin(data_c5$X1) + data_c5$X2 + sin(data_c5$X3) + data_c5$X4)*0.1
  
sd2_c75 <- mean(sin(data_c75$X1) + data_c75$X2 + sin(data_c75$X3) + data_c75$X4)*0.1
  
sd2_c <- mean(sin(data_c$X1) + data_c$X2 + sin(data_c$X3) + data_c$X4)*0.1
  
  
y2u <- sin(data_u$X1) + data_u$X2 + sin(data_u$X3) + data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd2_u) 
  
y2c25 <- sin(data_c25$X1) + data_c25$X2 + sin(data_c25$X3) + data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c25)
  
y2c5 <- sin(data_c5$X1) + data_c5$X2 + sin(data_c5$X3) + data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c5)

y2c75 <- sin(data_c75$X1) + data_c75$X2 + sin(data_c75$X3) + data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c75)
  
y2c <- sin(data_c$X1) + data_c$X2 + sin(data_c$X3) + data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c)  
  
data_u$y1u <- NULL
data_u["y2u"] <- y2u
data_c25$y1c25 <- NULL
data_c25["y2c25"] <- y2c25
data_c5$y1c5 <- NULL
data_c5["y2c5"] <- y2c5
data_c75$y1c75 <- NULL
data_c75["y2c75"] <- y2c75
data_c$y1c <- NULL
data_c["y2c"] <- y2c


### PFI with different correlations of features 1 and 2 on a random forest
library(mlr)
library(iml)
library(ggplot2)
library(randomForest)
  
rfub <- randomForest(y2u~. , data = data_u)
rfc25b <- randomForest(y2c25~. , data = data_c25)
rfc5b <- randomForest(y2c5~. , data = data_c5)
rfc75b <- randomForest(y2c75~. , data = data_c75)
rfcb <- randomForest(y2c~. , data = data_c)
  
rf2PFIu <- PFI(data_u, "regr.randomForest", "y2u" , rfub)
rf2PFIc25 <- PFI(data_c25, "regr.randomForest", "y2c25" , rfc25b)
rf2PFIc5 <- PFI(data_c5, "regr.randomForest", "y2c5" , rfc5b)
rf2PFIc75 <- PFI(data_c75, "regr.randomForest", "y2c75" , rfc75b)
rf2PFIc <- PFI(data_c, "regr.randomForest", "y2c" , rfcb)
  
prf <- ggplot(rf2PFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=rf2PFIc25$features, y=rf2PFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf2PFIc5$features, y=rf2PFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf2PFIc75$features, y=rf2PFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf2PFIc$features, y=rf2PFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
        theme(legend.title = element_text( size = 30),
          legend.text = element_text(size = 30)  )+
        theme(axis.text=element_text(size=30),
          axis.title=element_text(size=30,face="bold"))
  
prf
  
ggsave("PFI03.jpeg",width = 18, height = 8)
  
  
## PFI with different correlations of features 1 and 2 on a linear model
  
library(mlr)
library(iml)
library(ggplot2)
library(randomForest)
  
lmub <- lm(y2u~., data=data_u)
lmc25b <- lm(y2c25~., data = data_c25)
lmc5b <- lm(y2c5~., data = data_c5)
lmc75b <- lm(y2c75~., data = data_c75)
lmcb <- lm(y2c~., data = data_c)
  
lm2PFIu <- PFI(data_u, "regr.lm", "y2u" , lmub)
lm2PFIc25 <- PFI(data_c25, "regr.lm", "y2c25" , lmc25b)
lm2PFIc5 <- PFI(data_c5, "regr.lm", "y2c5" , lmc5b)
lm2PFIc75 <- PFI(data_c75, "regr.lm", "y2c75" , lmc75b)
lm2PFIc <- PFI(data_c, "regr.lm", "y2c" , lmcb)
  
  
#Plot:
plm <- ggplot(lm2PFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=lm2PFIc25$features, y=lm2PFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm2PFIc5$features, y=lm2PFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm2PFIc75$features, y=lm2PFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm2PFIc$features, y=lm2PFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30)  )+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))
  
plm
  
ggsave("PFI04.jpeg",width = 18, height = 8)

### LOCO Feature Importance with different kind of correlation intensity on a random forest model


  
rfLOCOu <- LOCO(data_u, "regr.randomForest", "y2u")
rfLOCOc25 <- LOCO(data_c25, "regr.randomForest", "y2c25")
rfLOCOc5 <- LOCO(data_c5, "regr.randomForest", "y2c5")
rfLOCOc75 <- LOCO(data_c75, "regr.randomForest", "y2c75")
rfLOCOc <- LOCO(data_c, "regr.randomForest", "y2c")
  
#Plot:
ploco <- ggplot(data = rfLOCOu ,aes(x=featureu,y = importance_u, colour = "0")) + #### hier fehler 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=rfLOCOc25$feature, y=rfLOCOc25$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc5$feature, y=rfLOCOc5$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc75$feature, y=rfLOCOc75$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc$feature, y=rfLOCOc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30)  )+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))
ploco
  
ggsave("LOCO03.jpeg",width = 18, height = 8)
  
# LOCO Feature Importance with different kind of correlation intensity on a linear model
  
lmLOCOu <- LOCO(data_u, "regr.lm", "y2u")
lmLOCOc25 <- LOCO(data_c25, "regr.lm", "y2c25")
lmLOCOc5 <- LOCO(data_c5, "regr.lm", "y2c5")
lmLOCOc75 <- LOCO(data_c75, "regr.lm", "y2c75")
lmLOCOc <- LOCO(data_c, "regr.lm", "y2c")
  
#Plot:
plocolm <- ggplot(data = lmLOCOu ,aes(x=featureu,y = importance_u, colour = "0")) + #### hier fehler 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=lmLOCOc25$feature, y=lmLOCOc25$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc5$feature, y=lmLOCOc5$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc75$feature, y=lmLOCOc75$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc$feature, y=lmLOCOc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                      colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30)  )+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))
plocolm
  
ggsave("LOCO04.jpeg",width = 18, height = 8)


# Comparison Non-Linear Dependecies
library(ggpubr)
ggarrange(prf, plm, ploco, plocolm , ncol=2, nrow = 2, legend = "bottom", common.legend = TRUE)

ggsave("arrange02.jpeg",width = 18, height = 8)








### 2) Nonlinear Dependence - Simulation of Nonlinear Dependence:

sd2_u <- mean(sin(data_u$X1) + data_u$X2 + sin(data_u$X3) + data_u$X4)*0.1
sd2_c25 <- mean(sin(data_c25$X1) + data_c25$X2 + sin(data_c25$X3) + data_c25$X4)*0.1
sd2_c5 <- mean(sin(data_c5$X1) + data_c5$X2 + sin(data_c5$X3) + data_c5$X4)*0.1
sd2_c75 <- mean(sin(data_c75$X1) + data_c75$X2 + sin(data_c75$X3) + data_c75$X4)*0.1
sd2_c <- mean(sin(data_c$X1) + data_c$X2 + sin(data_c$X3) + data_c$X4)*0.1


y2u <- sin(data_u$X1) + data_u$X2 + sin(data_u$X3) + data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd2_u) 
y2c25 <- sin(data_c25$X1) + data_c25$X2 + sin(data_c25$X3) + data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c25)
y2c5 <- sin(data_c5$X1) + data_c5$X2 + sin(data_c5$X3) + data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c5)
y2c75 <- sin(data_c75$X1) + data_c75$X2 + sin(data_c75$X3) + data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c75)
y2c <- sin(data_c$X1) + data_c$X2 + sin(data_c$X3) + data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd2_c)  

data_u$y1u <- NULL
data_u["y2u"] <- y2u
data_c25$y1c25 <- NULL
data_c25["y2c25"] <- y2c25
data_c5$y1c5 <- NULL
data_c5["y2c5"] <- y2c5
data_c75$y1c75 <- NULL
data_c75["y2c75"] <- y2c75
data_c$y1c <- NULL
data_c["y2c"] <- y2c


### PFI with different correlations of features 1 and 2 on a random forest

rfub <- randomForest(y2u~. , data = data_u)
rfc25b <- randomForest(y2c25~. , data = data_c25)
rfc5b <- randomForest(y2c5~. , data = data_c5)
rfc75b <- randomForest(y2c75~. , data = data_c75)
rfcb <- randomForest(y2c~. , data = data_c)

rf2PFIu <- PFI(data_u, "regr.randomForest", "y2u" , rfub)
rf2PFIc25 <- PFI(data_c25, "regr.randomForest", "y2c25" , rfc25b)
rf2PFIc5 <- PFI(data_c5, "regr.randomForest", "y2c5" , rfc5b)
rf2PFIc75 <- PFI(data_c75, "regr.randomForest", "y2c75" , rfc75b)
rf2PFIc <- PFI(data_c, "regr.randomForest", "y2c" , rfcb)

prf <- ggplot(rf2PFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=rf2PFIc25$features, y=rf2PFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf2PFIc5$features, y=rf2PFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf2PFIc75$features, y=rf2PFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf2PFIc$features, y=rf2PFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

prf

ggsave("PFI03.jpeg",width = 18, height = 8)


## PFI with different correlations of features 1 and 2 on a linear model

lmub <- lm(y2u~., data=data_u)
lmc25b <- lm(y2c25~., data = data_c25)
lmc5b <- lm(y2c5~., data = data_c5)
lmc75b <- lm(y2c75~., data = data_c75)
lmcb <- lm(y2c~., data = data_c)

lm2PFIu <- PFI(data_u, "regr.lm", "y2u" , lmub)
lm2PFIc25 <- PFI(data_c25, "regr.lm", "y2c25" , lmc25b)
lm2PFIc5 <- PFI(data_c5, "regr.lm", "y2c5" , lmc5b)
lm2PFIc75 <- PFI(data_c75, "regr.lm", "y2c75" , lmc75b)
lm2PFIc <- PFI(data_c, "regr.lm", "y2c" , lmcb)


#Plot:
plm <- ggplot(lm2PFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=lm2PFIc25$features, y=lm2PFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm2PFIc5$features, y=lm2PFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm2PFIc75$features, y=lm2PFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm2PFIc$features, y=lm2PFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

plm

ggsave("PFI04.jpeg",width = 18, height = 8)

### LOCO Feature Importance with different kind of correlation intensity on a random forest model
rfLOCOu <- LOCO(data_u, "regr.randomForest", "y2u")
rfLOCOc25 <- LOCO(data_c25, "regr.randomForest", "y2c25")
rfLOCOc5 <- LOCO(data_c5, "regr.randomForest", "y2c5")
rfLOCOc75 <- LOCO(data_c75, "regr.randomForest", "y2c75")
rfLOCOc <- LOCO(data_c, "regr.randomForest", "y2c")

#Plot:
ploco <- ggplot(data = rfLOCOu ,aes(x=featureu,y = importance_u, colour = "0")) + #### hier fehler 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=rfLOCOc25$feature, y=rfLOCOc25$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc5$feature, y=rfLOCOc5$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc75$feature, y=rfLOCOc75$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc$feature, y=rfLOCOc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))
ploco

ggsave("LOCO03.jpeg",width = 18, height = 8)

# LOCO Feature Importance with different kind of correlation intensity on a linear model

lmLOCOu <- LOCO(data_u, "regr.lm", "y2u")
lmLOCOc25 <- LOCO(data_c25, "regr.lm", "y2c25")
lmLOCOc5 <- LOCO(data_c5, "regr.lm", "y2c5")
lmLOCOc75 <- LOCO(data_c75, "regr.lm", "y2c75")
lmLOCOc <- LOCO(data_c, "regr.lm", "y2c")

#Plot:
plocolm <- ggplot(data = lmLOCOu ,aes(x=featureu,y = importance_u, colour = "0")) + #### hier fehler 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=lmLOCOc25$feature, y=lmLOCOc25$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc5$feature, y=lmLOCOc5$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc75$feature, y=lmLOCOc75$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc$feature, y=lmLOCOc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))+
  theme(legend.title = element_text( size = 30),
        legend.text = element_text(size = 30)  )+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))
plocolm

ggsave("LOCO04.jpeg",width = 18, height = 8)

# Comparison Non-Linear Dependecies
library(ggpubr)
ggarrange(prf, plm, ploco, plocolm , ncol=2, nrow = 2, legend = "bottom", common.legend = TRUE)

ggsave("arrange02.jpeg",width = 18, height = 8)

### Uninformative Feature: No interesting results yet

sd3_u <- mean(data_u$X1 + data_u$X2 + data_u$X3 + data_u$X4)*0.1

sd3_c25 <- mean(data_c25$X1 + data_c25$X2 + data_c25$X3 + data_c25$X4)*0.1

sd3_c5 <- mean(data_c5$X1 + data_c5$X2 + data_c5$X3 + data_c5$X4)*0.1

sd3_c75 <- mean(data_c75$X1 + data_c75$X2 + data_c75$X3 + data_c75$X4)*0.1

sd3_c <- mean(data_c$X1 + data_c$X2 + data_c$X3 + data_c$X4)*0.1



y3u <- data_u$X1 + data_u$X2 + data_u$X3 + data_u$X4 + rnorm(n = 1000, mean = 0, sd = sd3_u) 

y3c25 <- data_c25$X1 + data_c25$X2 + data_c25$X3 + data_c25$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c25)

y3c5 <- data_c5$X1 + data_c5$X2 + data_c5$X3 + data_c5$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c5)

y3c75 <- data_c75$X1 + data_c75$X2 + data_c75$X3 + data_c75$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c75)

y3c <- data_c$X1 + data_c$X2 + data_c$X3 + data_c$X4 + rnorm(n = 1000, mean = 0, sd = sd3_c)


data_u$y2u <- NULL
data_u$y1u <- NULL
data_u["y3u"] <- y3u
data_c25$y2c25 <- NULL
data_c25$y1c25 <- NULL
data_c25["y3c25"] <- y3c25
data_c5$y2c5 <- NULL
data_c5$y1c5 <- NULL
data_c5["y3c5"] <- y3c5
data_c75$y2c75 <- NULL
data_c75$y1c75 <- NULL
data_c75["y3c75"] <- y3c75
data_c$y2c <- NULL
data_c$y1c <- NULL
data_c["y3c"] <- y3c
head(data_u)
head(data_c25)
head(data_c75)

## PFI with different correlations of features 1 and 2 on a random forest model

rfuc <- randomForest(y3u~X1 + X2 + X3 , data = data_u)
rfc25c <- randomForest(y3c25~X1 + X2 + X3 , data = data_c25)
rfc5c <- randomForest(y3c5~X1 + X2 + X3 , data = data_c5)
rfc75c <- randomForest(y3c75~X1 + X2 + X3 , data = data_c75)
rfcc <- randomForest(y3c~X1 + X2 + X3 , data = data_c)

rf3PFIu <- PFIu(data_u, "regr.randomForest", "y3u" , rfuc)
rf3PFIc25 <- PFIu(data_c25, "regr.randomForest", "y3c25" , rfc25c)
rf3PFIc5 <- PFIu(data_c5, "regr.randomForest", "y3c5" , rfc5c)
rf3PFIc75 <- PFIu(data_c75, "regr.randomForest", "y3c75" , rfc75c)
rf3PFIc <- PFIu(data_c, "regr.randomForest", "y3c" , rfcc)

prf <- ggplot(rf3PFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=rf3PFIc25$features, y=rf3PFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf3PFIc5$features, y=rf3PFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf3PFIc75$features, y=rf3PFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rf3PFIc$features, y=rf3PFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30)  )+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

prf

ggsave("PFI05.jpeg",width = 18, height = 8)

## PFI with different correlations of features 1 and 2 on a linear model

lmuc <- lm(y3u~data_u$X1 + data_u$X2 + data_u$X3 , data=data_u)
lmc25c <- lm(y3c25~data_c25$X1 + data_c25$X2 + data_c25$X3, data = data_c25)
lmc5c <- lm(y3c5~data_c5$X1 + data_c5$X2 + data_c5$X3, data = data_c5)
lmc75c <- lm(y3c75~data_c75$X1 + data_c75$X2 + data_c75$X3, data = data_c75)
lmcc <- lm(y3c~data_c$X1 + data_c$X2 + data_c$X3, data = data_c)

lm3PFIu <- PFI(data_u, "regr.lm", "y3u" , lmuc)
lm3PFIc25 <- PFI(data_c25, "regr.lm", "y3c25" , lmc25c)
lm3PFIc5 <- PFI(data_c5, "regr.lm", "y3c5" , lmc5c)
lm3PFIc75 <- PFI(data_c75, "regr.lm", "y3c75" , lmc75c)
lm3PFIc <- PFI(data_c, "regr.lm", "y3c" , lmcc)


#Plot:
plm <- ggplot(lm3PFIu, aes(x=features, y= importance, colour = "0")) +
  geom_point(mapping=aes(x=features, y=importance), size=0, stroke = 5, shape=21, fill="white") +
  geom_point(mapping=aes(x=lm3PFIc25$features, y=lm3PFIc25$importance, colour = "0.25"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm3PFIc5$features, y=lm3PFIc5$importance, colour = "0.5"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm3PFIc75$features, y=lm3PFIc75$importance, colour = "0.75"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lm3PFIc$features, y=lm3PFIc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30))+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

plm

ggsave("PFI06.jpeg",width = 18, height = 8)


## LOCO Feature Importance with different kind of correlation intensity on a random forest model

rfLOCOu <- LOCO(data_u, "regr.randomForest", "y3u")
rfLOCOc25 <- LOCO(data_c25, "regr.randomForest", "y3c25")
rfLOCOc5 <- LOCO(data_c5, "regr.randomForest", "y3c5")
rfLOCOc75 <- LOCO(data_c75, "regr.randomForest", "y3c75")
rfLOCOc <- LOCO(data_c, "regr.randomForest", "y3c")

#Plot:
ploco <- ggplot(data = rfLOCOu ,aes(x=featureu,y = importance_u, colour = "0")) + #### hier fehler 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=rfLOCOc25$feature, y=rfLOCOc25$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc5$feature, y=rfLOCOc5$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc75$feature, y=rfLOCOc75$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=rfLOCOc$feature, y=rfLOCOc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(legend.title = element_text( size = 30),
          legend.text = element_text(size = 30))+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

ploco

ggsave("LOCO05.jpeg",width = 18, height = 8)


## LOCO Feature Importance with different kind of correlation intensity on a linear model

lmLOCOu <- LOCO(data_u, "regr.lm", "y3u")
lmLOCOc25 <- LOCO(data_c25, "regr.lm", "y3c25")
lmLOCOc5 <- LOCO(data_c5, "regr.lm", "y3c5")
lmLOCOc75 <- LOCO(data_c75, "regr.lm", "y3c75")
lmLOCOc <- LOCO(data_c, "regr.lm", "y3c")

#Plot:
plocolm <- ggplot(data = lmLOCOu ,aes(x=featureu,y = importance_u, colour = "0")) + #### hier fehler 
  geom_point(mapping=aes(x=feature, y=importance), size=0, shape=21,stroke = 5, fill="white") +
  geom_point(mapping=aes(x=lmLOCOc25$feature, y=lmLOCOc25$importance, colour = "0.25"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc5$feature, y=lmLOCOc5$importance, colour = "0.5"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc75$feature, y=lmLOCOc75$importance, colour = "0.75"), size=0,stroke = 5, shape=21, fill="white", show.legend = TRUE)+
  geom_point(mapping=aes(x=lmLOCOc$feature, y=lmLOCOc$importance, colour = "0.99"), size=0, shape=21,stroke = 5, fill="white", show.legend = TRUE)+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  scale_color_manual(labels = c("0","0.25","0.5","0.75","0.99"), values=c("0" = "skyblue", "0.25" = "royalblue", "0.5"= "blue", "0.75"="navy", "0.99" = "black"), name = "Correlation Intensity",breaks=c("0", "0.25", "0.5", "0.75", "0.99"))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(legend.title = element_text( size = 30),
              legend.text = element_text(size = 30))+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

plocolm

ggsave("LOCO06.jpeg",width = 18, height = 8)

## Comparison Uniformative Case

library(ggpubr)
ggarrange(prf, plm, ploco, plocolm , ncol=2, nrow = 2, legend = "bottom", common.legend = TRUE)

ggsave("arrange03.jpeg",width = 14, height = 8)

## 4) Simulation with high amount of correlated data
library(mvtnorm)

set.seed(455)

sigma_sim10 <- diag(1, nrow = 12)
sigma_sim10[which(sigma_sim10 != "1")] = 0.99 
sigma_sim10[11,] <- 0
sigma_sim10[,11] <- 0
sigma_sim10[12,] <- 0
sigma_sim10[,12] <- 0
sigma_sim10[11,11] <- 1
sigma_sim10[12,12] <- 1
sigma_sim10

data_sim10 <- as.data.frame(rmvnorm(n = 1000, 
                                    mean = rep(0, times = 12), 
                                    sigma = sigma_sim10))

data_sim10 <- as.data.frame(data_sim10)
data_sim10['Uninf'] <- runif(nrow(data_sim10))

sigma_sim5 <- diag(1, nrow = 7)
sigma_sim5[which(sigma_sim5 != "1")] = 0.99 
sigma_sim5[6,] <- 0
sigma_sim5[,6] <- 0
sigma_sim5[7,] <- 0
sigma_sim5[,7] <- 0
sigma_sim5[6,6] <- 1
sigma_sim5[7,7] <- 1
sigma_sim5

data_sim5 <- as.data.frame(rmvnorm(n = 1000, 
                                   mean = rep(0, times = 7), 
                                   sigma = sigma_sim5))
head(data_sim5)
head(data_sim10)

data_sim5 <- as.data.frame(data_sim5)
data_sim5['Uninf'] <- runif(nrow(data_sim5))


colnames(data_sim5) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "random")
colnames(data_sim10) <- c("X1", "X2", "X3", "X4","X5", "X6", "X7", "X8","X9", "X10", "X11", "X12", "random")

sd_sim5 <- mean(data_sim5$X1 + data_sim5$X2 + data_sim5$X3 + data_sim5$X4+ data_sim5$X5+ data_sim5$X6+ data_sim5$X7)*0.05

sd_sim10 <- mean(data_sim10$X1 + data_sim10$X2 + data_sim10$X3 + data_sim10$X4+ data_sim10$X5+ data_sim10$X6+ data_sim10$X7+ data_sim10$X8+ data_sim10$X9+ data_sim10$X10+ data_sim10$X11+ data_sim10$X12)*0.02

y1sim5 <- data_sim5$X1 + data_sim5$X2 + data_sim5$X3 + data_sim5$X4+ data_sim5$X5+ data_sim5$X6+ data_sim5$X7 + rnorm(n = 1000, mean = 0, sd = sd_sim5) 

y1sim10 <- data_sim10$X1 + data_sim10$X2 + data_sim10$X3 + data_sim10$X4+ data_sim10$X5+ data_sim10$X6+ data_sim10$X7+ data_sim10$X8+ data_sim10$X9+ data_sim10$X10+ data_sim10$X11+ data_sim10$X12 + rnorm(n = 1000, mean = 0, sd = sd_sim10)


data_sim5["y1sim5"] <- y1sim5

data_sim10["y1sim10"] <- y1sim10
is.positive.definite(cor(data_sim5))
is.positive.definite(cor(data_sim10))


## Simulation of 10 correlated features (r=0.99)

rfsim5 <- randomForest(y1sim5~. , data = data_sim5)

PFI04 <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=8,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),1:8], 
                         y = d[c(res$pred$instance$test.inds[[i]]),9])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

PFI04imp <- PFI04(data_sim5, "regr.randomForest","y1sim5",rfsim5)



psim5 <- ggplot(data = PFI04imp, aes(x=reorder(features, importance),y = importance)) +
  geom_point(size=0, stroke = 5, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "Feature Importance MSE", x= "") + 
  theme(plot.subtitle = element_text(size = 25)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

psim5

ggsave("PFI07.jpeg",width = 18, height = 8)

## Simulation of 10 correlated features (r=0.99)

rfsim10 <- randomForest(y1sim10~. , data = data_sim10)

PFI10 <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=13,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),1:13], 
                         y = d[c(res$pred$instance$test.inds[[i]]),14])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

PFI10imp <- PFI10(data_sim10, "regr.randomForest","y1sim10",rfsim10)

psim10 <- ggplot(data = PFI10imp, aes(x=reorder(features, importance),y = importance)) +
  geom_point(size=0, stroke = 3, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "Feature Importance MSE", x= "") + 
  theme(text = element_text(size = 14))+
  ggtitle("PFI on Random Forest")

psim10

ggsave("PFI08.jpeg",width = 14, height = 8)

### Comparison scenario 4) PFI
library(ggpubr)
ggarrange(psim5, psim10 , ncol=2, legend = "bottom", common.legend = TRUE)

ggsave("arrange4.jpeg",width = 14, height = 8)

## LOCO Feature Importance with different kind of correlation intensity on a random forest model

rfLOCOb <- LOCO(data_sim5, "regr.randomForest", "y1sim5")

plocosim5 <- ggplot(data = rfLOCOb ,aes(x=reorder(feature,importance),y = importance)) + #### hier fehler 
  geom_point(size=0, shape=21,stroke = 3, fill="black") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "Feature Importance MSE", x= "") + 
  theme(text = element_text(size = 14))+
  ggtitle("LOCO on Random Forest")

plocosim5


ggsave("LOCO07.jpeg",width = 14, height = 8)

## LOCO Feature Importance with different kind of correlation intensity on a random forest model

rfLOCOb <- LOCO(data_sim10, "regr.randomForest", "y1sim10")

plocosim10 <- ggplot(data = rfLOCOb ,aes(x=reorder(feature,importance),y = importance)) + #### hier fehler 
  geom_point(size=0, shape=21,stroke = 3, fill="black") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "Feature Importance MSE", x= "") + 
  theme(text = element_text(size = 14))+
  ggtitle("LOCO on Random Forest")

plocosim10


ggsave("LOCO08.jpeg",width = 14, height = 8)


## Comparison LOCO scenario 4)
library(ggpubr)
ggarrange(plocosim5, plocosim10 , ncol=2, legend = "bottom", common.legend = TRUE)

ggsave("arrange05.jpeg",width = 14, height = 8)


##### Real Data
library(ggpubr)
library(corrplot)
library(randomForest)
library(mlr)

data("Boston", package = "MASS")


#windows(width = 12,height = 12)
corrplot.mixed(cor(Boston),lower="circle",upper="number", tl.cex = 1.7, number.font = 1, tl.pos="lt")
#savePlot(filename="Corrplotb.jpg", type="jpeg")


head(Boston)

rfboston <- randomForest(medv~. , data = Boston)


PFIb <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=13,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),1:13], 
                         y = d[c(res$pred$instance$test.inds[[i]]),14])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

bostonimp <- PFIb(Boston,"regr.randomForest","medv", rfboston)

pb1 <- ggplot(data = bostonimp, aes(x=reorder(features, importance),y = importance)) +
  geom_point(size=0, stroke = 5, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
        theme(axis.text=element_text(size=30),
              axis.title=element_text(size=30,face="bold"))

pb1

ggsave("PFIreal01.jpeg",width = 18, height = 8)

library(ggpubr)
library(corrplot)

Boston["dup_lstat"] <- Boston$lstat
Boston <- Boston[ , c(14, 15, 1:13) ]
corrplot.mixed(cor(Boston),lower="circle",upper="number")


rfBoston2 <- randomForest(medv~. , data = Boston)

PFIb2 <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=14,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),2:15], 
                         y = d[c(res$pred$instance$test.inds[[i]]),1])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

rfBostonimp <- PFIb2(Boston,"regr.randomForest","medv", rfBoston2)


pb2 <- ggplot(data = rfBostonimp, aes(x= reorder(features, importance), y = importance)) +
  geom_point( size=0, stroke = 5, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

pb2
ggsave("PFIreal02.jpeg",width = 18, height = 8)


ggarrange(pb1, pb2, ncol=2, legend = "bottom", common.legend = TRUE)

ggsave("arrange06.jpeg",width = 18, height = 8)



data("Boston", package = "MASS")
head(Boston)


newboston <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', "medv")]


#windows(width = 12,height = 14)
#corrplot.mixed(cor(newboston2),lower="circle",upper="number", tl.cex = 1.7, number.font = 1, tl.pos = "lt")
#savePlot(filename="Corrplotb3.jpg", type="jpeg")

rfBoston22 <- randomForest(medv~. , data = newboston)

PFIb2 <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=5,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),1:5], 
                         y = d[c(res$pred$instance$test.inds[[i]]),6])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

rfBostonimp <- PFIb2(newboston,"regr.randomForest","medv", rfBoston22)


pb3 <- ggplot(data = rfBostonimp, aes(x= reorder(features, importance), y = importance)) +
  geom_point( size=0, stroke = 5, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

pb3
ggsave("PFIreal21.jpeg",width = 18, height = 8)

library(ggpubr)
library(corrplot)
library(randomForest)
library(iml)
library(mlr)

data("Boston", package = "MASS")
head(Boston)

Boston["d_lstat"] <- Boston$lstat

Boston <- Boston[ , c(1:13, 15, 14)]

newboston <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'd_lstat', "medv")]

noise <- rnorm(length(Boston$d_lstat), mean = 0, sd = 1)
Boston["n_lstat"] <- Boston$d_lstat+noise

newboston2 <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'n_lstat', "medv")]
head(newboston2)
#windows(width = 12,height = 14)
corrplot.mixed(cor(newboston2),lower="circle",upper="number", tl.cex = 1.7, number.font = 1, tl.pos = "lt")
#savePlot(filename="Corrplotb3.jpg", type="jpeg")

rfBoston21 <- randomForest(medv~. , data = newboston2)

PFIb2 <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=6,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),1:6], 
                         y = d[c(res$pred$instance$test.inds[[i]]),7])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

rfBostonimp <- PFIb2(newboston2,"regr.randomForest","medv", rfBoston21)


pb4 <- ggplot(data = rfBostonimp, aes(x= reorder(features, importance), y = importance)) +
  geom_point( size=0, stroke = 5, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

pb4
ggsave("PFIreal22.jpeg",width = 18, height = 8)






ggarrange(pb3, pb4, ncol=2, legend = "bottom", common.legend = TRUE)

ggsave("arrange08.jpeg",width = 20, height = 8)



library(ggpubr)
library(corrplot)
library(randomForest)
library(iml)
library(mlr)

data("Boston", package = "MASS")
head(Boston)

Boston["dup_lstat"] <- Boston$lstat

Boston <- Boston[ , c(1:13, 15, 14)]

newboston <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'dup_lstat', "medv")]

noise <- rnorm(length(Boston$dup_lstat), mean = 10, sd = 2)
Boston["dup_lstat"] <- Boston$dup_lstat+noise

newboston2 <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'dup_lstat', "medv")]

#windows(width = 12,height = 14)
corrplot.mixed(cor(newboston2),lower="circle",upper="number", tl.cex = 1.7, number.font = 1, tl.pos = "lt")
#savePlot(filename="Corrplotb2.jpg", type="jpeg")

rfBoston21 <- randomForest(medv~. , data = newboston2)

PFIb2 <- function(data,learner,target,model){
  d <- data
  task = makeRegrTask(data = data , target= target)
  learnerPFI = makeLearner(learner)
  res = resample(learner = learnerPFI, task = task, 
                 resampling = res_desc,show.info = FALSE, models = TRUE)
  resultPFI <- data.frame(matrix(nrow=6,ncol=1))
  for(i in 1:10){
    mod <- Predictor$new(res$models[[i]]$learner.model,
                         data=d[c(res$pred$instance$test.inds[[i]]),1:6], 
                         y = d[c(res$pred$instance$test.inds[[i]]),7])
    imp <- FeatureImp$new(mod, loss = "mse" , compare = "ratio")
    impo <- imp$results
    impo <- impo[order(impo$feature),]
    resultPFI[i] <- data.frame(impo$importance)
  }
  imp.dat <- data.frame(rowSums(resultPFI)/10)
  imp.dat["features"]<- impo$feature
  colnames(imp.dat) <- c("importance","features")
  return(imp.dat)
}

rfBostonimp <- PFIb2(newboston2,"regr.randomForest","medv", rfBoston21)


pb2 <- ggplot(data = rfBostonimp, aes(x= reorder(features, importance), y = importance)) +
  geom_point( size=0, stroke = 5, shape=21, fill="blue")+
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "PFI (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

pb2
ggsave("PFIreal22.jpeg",width = 14, height = 8)





data("Boston", package = "MASS")

rfLOCOb <- LOCO(Boston, "regr.randomForest", "medv")


plocob1 <- ggplot(data = rfLOCOb ,aes(x=reorder(feature,importance),y = importance)) + #### hier fehler 
  geom_point(size=0, shape=21,stroke = 5, fill="black") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

plocob1

ggsave("LOCOb1.jpeg",width = 18, height = 8)


data("Boston", package = "MASS")
Boston["dup_lstat"] <- Boston$lstat
rfLOCOb2 <- LOCO(Boston, "regr.randomForest", "medv")


plocob2 <- ggplot(data = rfLOCOb2 ,aes(x=reorder(feature,importance),y = importance)) + #### hier fehler 
  geom_point(size=0, shape=21,stroke = 5, fill="black") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

plocob2

ggsave("LOCOb2.jpeg",width = 18, height = 8)



ggarrange(plocob1, plocob2, ncol=2, legend = "bottom", common.legend = TRUE)

ggsave("arrange09.jpeg",width = 20, height = 8)


data("Boston", package = "MASS")
head(Boston)

Boston["d_lstat"] <- Boston$lstat

Boston <- Boston[ , c(1:13, 15, 14)]

newboston <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'd_lstat', "medv")]



rfLOCOb2 <- LOCO(newboston, "regr.randomForest", "medv")

locob3 <- ggplot(data = rfLOCOb2 ,aes(x=reorder(feature,importance),y = importance)) + #### hier fehler 
  geom_point(size=0, shape=21,stroke = 5, fill="black") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

locob3

ggsave("LOCOb3.jpeg",width = 18, height = 8)




data("Boston", package = "MASS")
head(Boston)

Boston["d_lstat"] <- Boston$lstat

Boston <- Boston[ , c(1:13, 15, 14)]

newboston <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'd_lstat', "medv")]


noise <- rnorm(length(Boston$d_lstat), mean = 0, sd = 3)
Boston["n_lstat"] <- Boston$d_lstat+noise

newboston2 <- Boston[ ,c("rm",'tax', "nox", 'dis','lstat', 'n_lstat', "medv")]
head(newboston2)


rfLOCOb2 <- LOCO(newboston2, "regr.randomForest", "medv")

locob4 <- ggplot(data = rfLOCOb2 ,aes(x=reorder(feature,importance),y = importance)) + #### hier fehler 
  geom_point(size=0, shape=21,stroke = 5, fill="black") +
  coord_flip() +
  theme(plot.subtitle = element_text(size = 10)) +
  labs(y = "LOCO (Loss: MSE)", x= "") + 
  theme(text = element_text(size = 30))+
  theme(
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"))+
  theme(axis.text=element_text(size=30),
        axis.title=element_text(size=30,face="bold"))

locob4

ggsave("LOCOb4.jpeg",width = 18, height = 8)


library(ggpubr)
ggarrange(locob3, locob4, ncol=2, legend = "bottom", common.legend = TRUE)

ggsave("arrange10.jpeg",width = 20, height = 8)







## Unrealistic data instances - Rental bike



###Example


bike = read.csv("day.csv")
head(bike)

bike$weekday = factor(bike$weekday, levels=0:6, labels = c('SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT'))
bike$holiday = factor(bike$holiday, levels = c(0,1), labels = c('NO HOLIDAY', 'HOLIDAY'))
bike$workingday = factor(bike$workingday, levels = c(0,1), labels = c('NO WORKING DAY', 'WORKING DAY'))
bike$season = factor(bike$season, levels = 1:4, labels = c('SPRING', 'SUMMER', 'FALL', 'WINTER'))
bike$weathersit = factor(bike$weathersit, levels = 1:3, labels = c('GOOD', 'MISTY', 'RAIN/SNOW/STORM'))
bike$mnth = factor(bike$mnth, levels = 1:12, labels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OKT', 'NOV', 'DEZ'))
bike$yr[bike$yr == 0] = 2011
bike$yr[bike$yr == 1] = 2012
bike$yr = factor(bike$yr)

# denormalize weather features:
# temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
bike$temp = bike$temp * (39 - (-8)) + (-8)
# atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
bike$atemp = bike$atemp * (50 - (16)) + (16)

#windspeed: Normalized wind speed. The values are divided to 67 (max)
bike$windspeed = 67 * bike$windspeed
#hum: Normalized humidity. The values are divided to 100 (max)
bike$hum = 100 * bike$hum


newbike <- bike[ ,c("dteday", 'season','holiday', "weekday", 'workingday', 'weathersit', 'temp', 'hum', 'windspeed', "cnt")]

head(newbike)
newbike

bike2 <- transform(newbike, season = sample(season))
head(bike2)
bike3 <- transform(newbike, weekday = sample(weekday))
head(bike3)

## Prevention of Correlation Problems


