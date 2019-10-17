library(dplyr) # for group by
library(Ecdat) # icecream datasets
library(ggdag) # for structural causal graphs
library(ggpubr) # ggarrange
library(iml)  # pdp
library(randomForest)

################
# Ice Cream
################
data("Icecream")
rf.icecream = randomForest(cons ~ temp
                           , data = Icecream, importance = T)
mod = Predictor$new(rf.icecream, data = Icecream)
pdp.obj = Partial$new(mod, feature = c("temp"))  
p1 <- plot(pdp.obj) 

rf.icecream_reverse = randomForest(temp ~ cons
                                   , data = Icecream, importance = T)
mod_reverse = Predictor$new(rf.icecream_reverse, data = Icecream)
pdp.obj_reverse = Partial$new(mod_reverse, feature = c("cons"))  
p2 <- plot(pdp.obj_reverse) 

##############################
# Average vs Individual Effect
##############################
set.seed(13)
N <- 1000
e <- rnorm(N, 0, 0.1)
x1 <- runif(N, -1, 1)
x2 <- runif(N, -1, 1)
y <- x1^2 - 15*x1*x2 + e

df <- data.frame(x1, x2, y)
rf.test = randomForest(y ~ . , data = df, importance = T, ntree = 500)
mod = Predictor$new(rf.test, data = df)

pdp.obj = FeatureEffect$new(mod, feature = c("x1"), method = "pdp+ice")  
plot(pdp.obj)

####################
# Scenario 1
###################
#DAG (Chain)
dagify(Y ~ X
) %>% 
  ggdag() 
dagify(Y ~ Z
) %>% 
  ggdag() 

# pdp

pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Y_hat", "run_number"))
  all_dfs <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Z", "Y"))
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    Z <- runif(N, -1, 1)
    X <- Z + e
    Y <- X + e
    df <- data.frame(X, Z, Y)
    all_dfs <<- rbind.data.frame(all_dfs, df)
    
    rf.basic = randomForest(Y ~ . , data = df, importance = T)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
    
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
  }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
scen1_plot_1 <- pdp_simulation(runs=20, N = 100, error_std=0.1)
scen1_plot_1 <- scen1_plot_1 + labs(x = "X", y = "Y_hat")
# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$Y
plot(all_dfs$X, all_dfs$EY)

scen1_plot_2 <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
scen1_plot_2 <- scen1_plot_2 + labs(x = "X", y = "Y_hat")
scen1_plot_3 <- pdp_simulation(runs=20, N = 10000, error_std=0.1)
scen1_plot_3 <- scen1_plot_3 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.3
scen1_plot_4 <- pdp_simulation(runs=20, N = 100, error_std=0.3)
scen1_plot_4 <- scen1_plot_4 + labs(x = "X", y = "Y_hat")
scen1_plot_5 <- pdp_simulation(runs=20, N = 1000, error_std=0.3)
scen1_plot_5 <- scen1_plot_5 + labs(x = "X", y = "Y_hat")
scen1_plot_6 <- pdp_simulation(runs=20, N = 10000, error_std=0.3)
scen1_plot_6 <- scen1_plot_6 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.5
scen1_plot_7 <- pdp_simulation(runs=20, N = 100, error_std=0.5)
scen1_plot_7 <- scen1_plot_7 + labs(x = "X", y = "Y_hat")
scen1_plot_8 <- pdp_simulation(runs=20, N = 1000, error_std=0.5)
scen1_plot_8 <- scen1_plot_8 + labs(x = "X", y = "Y_hat")
scen1_plot_9 <- pdp_simulation(runs=20, N = 10000, error_std=0.5)
scen1_plot_9 <- scen1_plot_9 + labs(x = "X", y = "Y_hat")


# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
  for(i in 1:runs){
    
    e <- rnorm(N,0, error_std)
    Z <- runif(N, -1, 1)
    X <- Z + e
    Y <- X + e
    df <- data.frame(X, Z, Y)
    for(j in 1:length(unique(df$X))){
      X <- unique(df$X)[j]
      Y <- X + e
      df_intervention <- data.frame(X, Z, Y)
      df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
    }
  }
  
  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
              n = n()) %>%
    
    arrange(X)
  
  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
scen1_int_plot_2 <- intervention(runs=1, N = 1000, error_std=0.1)
# standard deviation eq 0.3
scen1_int_plot_5 <- intervention(runs=1, N = 1000, error_std=0.3)
# standard deviation eq 0.5
scen1_int_plot_8 <- intervention(runs=1, N = 1000, error_std=0.5)


scenario_1_all <- ggarrange(scen1_plot_1, # N = 100,  std = 0.1
                            scen1_plot_2, # N = 1000, std = 0.1
                            scen1_plot_3, # N = 2000, std = 0.1
                            scen1_int_plot_2,
                            
                            scen1_plot_4, # N = 100,  std = 0.3
                            scen1_plot_5, # N = 1000, std = 0.3
                            scen1_plot_6, # N = 2000, std = 0.3
                            scen1_int_plot_5,
                            
                            scen1_plot_7, # N = 100,  std = 0.5
                            scen1_plot_8, # N = 1000, std = 0.5
                            scen1_plot_9, # N = 2000, std = 0.5
                            scen1_int_plot_8,
                            
                            nrow= 3, ncol = 4)
annotate_figure(scenario_1_all,
                top = text_grob("Increasing number of observations to the right", color = "black"),
                left = text_grob("Increasing standard deviation of error to the bottom", color = "black", rot = 90),
                 fig.lab.face = "bold")

####################
# Scenario 2
###################
#DAG (Chain)
dagify(Y ~ Z,
       Z ~ X
) %>% 
  ggdag() 

# pdp
pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Y_hat", "run_number"))
  all_dfs <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Z", "Y"))
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- X + e
    Y <- Z + e
    df <- data.frame(X, Z, Y)
    all_dfs <<- rbind.data.frame(all_dfs, df)
    rf.basic = randomForest(Y ~ . , data = df, importance = T)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
  
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
    }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
scen2_plot_1 <- pdp_simulation(runs=20, N = 100, error_std=0.1)
scen2_plot_1 <- scen2_plot_1 + labs(x = "X", y = "Y_hat")
scen2_plot_2 <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
scen2_plot_2 <- scen2_plot_2 + labs(x = "X", y = "Y_hat")
scen2_plot_3 <- pdp_simulation(runs=20, N = 10000, error_std=0.1)
scen2_plot_3 <- scen2_plot_3 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.3
scen2_plot_4 <- pdp_simulation(runs=20, N = 100, error_std=0.3)
scen2_plot_4 <- scen2_plot_4 + labs(x = "X", y = "Y_hat")
scen2_plot_5 <- pdp_simulation(runs=20, N = 1000, error_std=0.3)
scen2_plot_5 <- scen2_plot_5 + labs(x = "X", y = "Y_hat")
scen2_plot_6 <- pdp_simulation(runs=20, N = 10000, error_std=0.3)
scen2_plot_6 <- scen2_plot_6 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.5
scen2_plot_7 <- pdp_simulation(runs=20, N = 100, error_std=0.5)
scen2_plot_7 <- scen2_plot_7 + labs(x = "X", y = "Y_hat")
scen2_plot_8 <- pdp_simulation(runs=20, N = 1000, error_std=0.5)
scen2_plot_8 <- scen2_plot_8 + labs(x = "X", y = "Y_hat")
scen2_plot_9 <- pdp_simulation(runs=20, N = 10000, error_std=0.5)
scen2_plot_9 <- scen2_plot_9 + labs(x = "X", y = "Y_hat")

# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
    for(i in 1:runs){
  
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- X + e
    Y <- Z + e
    df <- data.frame(X, Z, Y)
      for(j in 1:length(unique(df$X))){
        X <- unique(df$X)[j]
        Z <- X + e
        Y <- Z + e
        df_intervention <- data.frame(X, Z, Y)
        df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
      }
    }

  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
    n = n()) %>%
    
    arrange(X)

  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
scen2_int_plot_2 <- intervention(runs=1, N = 1000, error_std=0.1)
# standard deviation eq 0.3
scen2_int_plot_5 <- intervention(runs=1, N = 1000, error_std=0.3)
# standard deviation eq 0.5
scen2_int_plot_8 <- intervention(runs=1, N = 1000, error_std=0.5)


scenario_2_all <- ggarrange(scen2_plot_1, # N = 100,  std = 0.1
                            scen2_plot_2, # N = 1000, std = 0.1
                            scen2_plot_3, # N = 2000, std = 0.1
                            scen2_int_plot_2,
                            
                            scen2_plot_4, # N = 100,  std = 0.3
                            scen2_plot_5, # N = 1000, std = 0.3
                            scen2_plot_6, # N = 2000, std = 0.3
                            scen2_int_plot_5,
                            
                            scen2_plot_7, # N = 100,  std = 0.5
                            scen2_plot_8, # N = 1000, std = 0.5
                            scen2_plot_9, # N = 2000, std = 0.5
                            scen2_int_plot_8,
                            
                        nrow= 3, ncol = 4)
annotate_figure(scenario_2_all,
                top = text_grob("Increasing number of observations to the right", color = "black"),
                left = text_grob("Increasing standard deviation of error to the bottom", color = "black", rot = 90),
                 fig.lab.face = "bold")


####################
# Scenario 3
###################

dagify(Y ~ X,
       Y ~ Z,
       Z ~ X
) %>% 
  ggdag() 

# pdp
pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Y_hat", "run_number"))
  all_dfs <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Z", "Y"))
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- X + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    all_dfs <<- rbind.data.frame(all_dfs, df)
    rf.basic <<- randomForest(Y ~ . , data = df, importance = T)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
    
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
  }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
scen3_plot_1 <- pdp_simulation(runs=20, N = 100, error_std=0.1)
scen3_plot_1 <- scen3_plot_1 + labs(x = "X", y = "Y_hat")
scen3_plot_2 <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
scen3_plot_2 <- scen3_plot_2 + labs(x = "X", y = "Y_hat")
# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]), alpha = 0.5)
ggarrange(scen3_plot_2, theoretpdp, ncol= 2)
cor(all_dfs)

scen3_plot_3 <- pdp_simulation(runs=20, N = 10000, error_std=0.1)
scen3_plot_3 <- scen3_plot_3 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.3
scen3_plot_4 <- pdp_simulation(runs=20, N = 100, error_std=0.3)
scen3_plot_4 <- scen3_plot_4 + labs(x = "X", y = "Y_hat")
scen3_plot_5 <- pdp_simulation(runs=20, N = 1000, error_std=0.3)
scen3_plot_5 <- scen3_plot_5 + labs(x = "X", y = "Y_hat")
scen3_plot_6 <- pdp_simulation(runs=20, N = 10000, error_std=0.3)
scen3_plot_6 <- scen3_plot_6 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.5
scen3_plot_7 <- pdp_simulation(runs=20, N = 100, error_std=0.5)
scen3_plot_7 <- scen3_plot_7 + labs(x = "X", y = "Y_hat")
scen3_plot_8 <- pdp_simulation(runs=20, N = 1000, error_std=0.5)
scen3_plot_8 <- scen3_plot_8 + labs(x = "X", y = "Y_hat")

# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]), alpha = 0.5)
ggarrange(scen3_plot_8, theoretpdp, ncol= 2)
cor(all_dfs)

importance(rf.basic)
varImpPlot(rf.basic, type = 2)


scen3_plot_9 <- pdp_simulation(runs=20, N = 10000, error_std=0.5)
scen3_plot_9 <- scen3_plot_9 + labs(x = "X", y = "Y_hat")

# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
  for(i in 1:runs){
    
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- X + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    for(j in 1:length(unique(df$X))){
      X <- unique(df$X)[j]
      Z <- X + e
      Y <- X + Z + e
      df_intervention <- data.frame(X, Z, Y)
      df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
    }
  }
  
  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
              n = n()) %>%
    
    arrange(X)
  
  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
scen3_int_plot_2 <- intervention(runs=1, N = 1000, error_std=0.1)
# standard deviation eq 0.3
scen3_int_plot_5 <- intervention(runs=1, N = 1000, error_std=0.3)
# standard deviation eq 0.5
scen3_int_plot_8 <- intervention(runs=1, N = 1000, error_std=0.5)


scenario_3_all <- ggarrange(scen3_plot_1, # N = 100,  std = 0.1
                            scen3_plot_2, # N = 1000, std = 0.1
                            scen3_plot_3, # N = 2000, std = 0.1
                            scen3_int_plot_2,
                            
                            scen3_plot_4, # N = 100,  std = 0.3
                            scen3_plot_5, # N = 1000, std = 0.3
                            scen3_plot_6, # N = 2000, std = 0.3
                            scen3_int_plot_5,
                            
                            scen3_plot_7, # N = 100,  std = 0.5
                            scen3_plot_8, # N = 1000, std = 0.5
                            scen3_plot_9, # N = 2000, std = 0.5
                            scen3_int_plot_8,
                            
                            nrow= 3, ncol = 4)
annotate_figure(scenario_3_all,
                top = text_grob("Increasing number of observations to the right", color = "black"),
                left = text_grob("Increasing standard deviation of error to the bottom", color = "black", rot = 90),
                fig.lab.face = "bold")

####################
# Scenario 4
###################

# pdp
pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 2, nrow = 0)),  c("X1", "y_hat"))
  
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- runif(N, -1, 1)
    Y <- X + e
    df <- data.frame(X, Z, Y)
    
    rf.basic = randomForest(Y ~ . , data = df, importance = T)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
    
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
  }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
scen4_plot_1 <- pdp_simulation(runs=20, N = 100, error_std=0.1)
scen4_plot_1 <- scen4_plot_1 + labs(x = "X", y = "Y_hat")
scen4_plot_2 <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
scen4_plot_2 <- scen4_plot_2 + labs(x = "X", y = "Y_hat")
scen4_plot_3 <- pdp_simulation(runs=20, N = 10000, error_std=0.1)
scen4_plot_3 <- scen4_plot_3 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.3
scen4_plot_4 <- pdp_simulation(runs=20, N = 100, error_std=0.3)
scen4_plot_4 <- scen4_plot_4 + labs(x = "X", y = "Y_hat")
scen4_plot_5 <- pdp_simulation(runs=20, N = 1000, error_std=0.3)
scen4_plot_5 <- scen4_plot_5 + labs(x = "X", y = "Y_hat")
scen4_plot_6 <- pdp_simulation(runs=20, N = 10000, error_std=0.3)
scen4_plot_6 <- scen4_plot_6 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.5
scen4_plot_7 <- pdp_simulation(runs=20, N = 100, error_std=0.5)
scen4_plot_7 <- scen4_plot_7 + labs(x = "X", y = "Y_hat")
scen4_plot_8 <- pdp_simulation(runs=20, N = 1000, error_std=0.5)
scen4_plot_8 <- scen4_plot_8 + labs(x = "X", y = "Y_hat")
scen4_plot_9 <- pdp_simulation(runs=20, N = 10000, error_std=0.5)
scen4_plot_9 <- scen4_plot_9 + labs(x = "X", y = "Y_hat")

# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
  for(i in 1:runs){
    
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- runif(N, -1, 1)
    Y <- X + e
    df <- data.frame(X, Z, Y)
    for(j in 1:length(unique(df$X))){
      X <- unique(df$X)[j]
      Y <- X + e
      df_intervention <- data.frame(X, Z, Y)
      df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
    }
  }
  
  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
              n = n()) %>%
    
    arrange(X)
  
  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
scen4_int_plot_2 <- intervention(runs=1, N = 1000, error_std=0.1)
# standard deviation eq 0.3
scen4_int_plot_5 <- intervention(runs=1, N = 1000, error_std=0.3)
# standard deviation eq 0.5
scen4_int_plot_8 <- intervention(runs=1, N = 1000, error_std=0.5)


scenario_4_all <- ggarrange(scen4_plot_1, # N = 100,  std = 0.1
                            scen4_plot_2, # N = 1000, std = 0.1
                            scen4_plot_3, # N = 2000, std = 0.1
                            scen4_int_plot_2,
                            
                            scen4_plot_4, # N = 100,  std = 0.3
                            scen4_plot_5, # N = 1000, std = 0.3
                            scen4_plot_6, # N = 2000, std = 0.3
                            scen4_int_plot_5,
                            
                            scen4_plot_7, # N = 100,  std = 0.5
                            scen4_plot_8, # N = 1000, std = 0.5
                            scen4_plot_9, # N = 2000, std = 0.5
                            scen4_int_plot_8,
                            
                            nrow= 3, ncol = 4)
annotate_figure(scenario_4_all,
                top = text_grob("Increasing number of observations to the right", color = "black"),
                left = text_grob("Increasing standard deviation of error to the bottom", color = "black", rot = 90),
                fig.lab.face = "bold")


####################
# scenario 5
###################

dagify(Y ~ X,
       Y ~ Z,
       X ~ Z
) %>% 
  ggdag() 

# pdp
pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Y_hat", "run_number"))
  all_dfs <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Z", "Y"))
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    Z <- runif(N, -1, 1)
    X <- Z + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    all_dfs <<- rbind.data.frame(all_dfs, df)
    rf.basic <<- randomForest(Y ~ . , data = df, importance = T)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
    
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
  }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
scen5_plot_1 <- pdp_simulation(runs=20, N = 100, error_std=0.1)
scen5_plot_1 <- scen5_plot_1 + labs(x = "X", y = "Y_hat")
# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]), alpha = 0.5)
ggarrange(scen5_plot_1, theoretpdp, ncol= 2)
cor(all_dfs)

scen5_plot_2 <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
scen5_plot_2 <- scen5_plot_2 + labs(x = "X", y = "Y_hat")
# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]), alpha = 0.5)

cor(all_dfs)

scen5_plot_3 <- pdp_simulation(runs=20, N = 10000, error_std=0.1)
scen5_plot_3 <- scen5_plot_3 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.3
scen5_plot_4 <- pdp_simulation(runs=20, N = 100, error_std=0.3)
scen5_plot_4 <- scen5_plot_4 + labs(x = "X", y = "Y_hat")
scen5_plot_5 <- pdp_simulation(runs=20, N = 1000, error_std=0.3)
scen5_plot_5 <- scen5_plot_5 + labs(x = "X", y = "Y_hat")
scen5_plot_6 <- pdp_simulation(runs=20, N = 10000, error_std=0.3)
scen5_plot_6 <- scen5_plot_6 + labs(x = "X", y = "Y_hat")
# standard deviation eq 0.5
scen5_plot_7 <- pdp_simulation(runs=20, N = 100, error_std=0.5)
scen5_plot_7 <- scen5_plot_7 + labs(x = "X", y = "Y_hat")
scen5_plot_8 <- pdp_simulation(runs=20, N = 1000, error_std=0.5)
scen5_plot_8 <- scen5_plot_8 + labs(x = "X", y = "Y_hat")

# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]), alpha = 0.5)
ggarrange(scen5_plot_8, theoretpdp, ncol= 2)
cor(all_dfs)

scen5_plot_9 <- pdp_simulation(runs=20, N = 10000, error_std=0.5)
scen5_plot_9 <- scen5_plot_9 + labs(x = "X", y = "Y_hat")

# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
  for(i in 1:runs){
    
    e <- rnorm(N,0, error_std)
    Z <- runif(N, -1, 1)
    X <- Z + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    for(j in 1:length(unique(df$X))){
      X <- unique(df$X)[j]
      Y <- X + Z + e
      df_intervention <- data.frame(X, Z, Y)
      df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
    }
  }
  
  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
              n = n()) %>%
    
    arrange(X)
  
  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
scen5_int_plot_2 <- intervention(runs=1, N = 1000, error_std=0.1)

ggarrange(scen5_plot_2, theoretpdp, scen5_int_plot_2, ncol= 3)


# standard deviation eq 0.3
scen5_int_plot_5 <- intervention(runs=1, N = 1000, error_std=0.3)
# standard deviation eq 0.5
scen5_int_plot_8 <- intervention(runs=1, N = 1000, error_std=0.5)


scenario_5_all <- ggarrange(scen5_plot_1+ coord_cartesian(ylim=(-1:1)), # N = 100,  std = 0.1
                            scen5_plot_2+ coord_cartesian(ylim=(-1:1)), # N = 1000, std = 0.1
                            scen5_plot_3+ coord_cartesian(ylim=(-1:1)), # N = 2000, std = 0.1
                            scen5_int_plot_2+ coord_cartesian(ylim=(-1:1)),
                            
                            scen5_plot_4+ coord_cartesian(ylim=(-2:2)), # N = 100,  std = 0.3
                            scen5_plot_5+ coord_cartesian(ylim=(-2:2)), # N = 1000, std = 0.3
                            scen5_plot_6+ coord_cartesian(ylim=(-2:2)), # N = 2000, std = 0.3
                            scen5_int_plot_5+ coord_cartesian(ylim=(-2:2)),
                            
                            scen5_plot_7+ coord_cartesian(ylim=(-3:3)), # N = 100,  std = 0.5
                            scen5_plot_8 + coord_cartesian(ylim=(-3:3)), # N = 1000, std = 0.5
                            scen5_plot_9+ coord_cartesian(ylim=(-3:3)), # N = 2000, std = 0.5
                            scen5_int_plot_8+ coord_cartesian(ylim=(-3:3)),
                            
                            nrow= 3, ncol = 4)
annotate_figure(scenario_5_all,
                top = text_grob("Increasing number of observations to the right", color = "black"),
                left = text_grob("Increasing standard deviation of error to the bottom", color = "black", rot = 90),
                fig.lab.face = "bold")


##############################
# Example with theoretical PDP
##############################
dagify(Y ~ X,
       Z ~ X,
       Y ~ Z
) %>% 
  ggdag() 

# pdp
pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Y_hat", "run_number"))
  all_dfs <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Z", "Y"))
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- X + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    all_dfs <<- rbind.data.frame(all_dfs, df)
    rf.basic <<- randomForest(Y ~ . , data = df)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
    
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
  }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
test_plot <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
test_plot <- test_plot + labs(x = "X", y = "Y_hat")

# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]),  alpha = 0.5)

# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
  for(i in 1:runs){
    
    e <- rnorm(N,0, error_std)
    X <- runif(N, -1, 1)
    Z <- X + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    for(j in 1:length(unique(df$X))){
      X <- unique(df$X)[j]
      Z <- X + e
      Y <- X + Z + e
      df_intervention <- data.frame(X, Z, Y)
      df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
    }
  }
  
  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
              n = n()) %>%
    
    arrange(X)
  
  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
test_int_plot <- intervention(runs=1, N = 1000, error_std=0.1)
ggarrange(test_plot + coord_cartesian(ylim=(-2:2)), theoretpdp + labs(x = "X", y = "E[Y]"),test_int_plot, ncol= 3)

##############################
# Example with theoretical PDP2
##############################

dagify(Y ~ X,
       X ~ Z,
       Y ~ Z
) %>% 
  ggdag() 

# pdp
pdp_simulation <- function(runs, N, error_std){ 
  pdp_simulations <- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Y_hat", "run_number"))
  all_dfs <<- setNames(data.frame(matrix(ncol = 3, nrow = 0)),  c("X", "Z", "Y"))
  for(i in 1:runs){
    e <- rnorm(N,0, error_std)
    Z <- runif(N, -1, 1)
    X <- Z + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    all_dfs <<- rbind.data.frame(all_dfs, df)
    rf.basic <<- randomForest(Y ~ . , data = df)
    mod = Predictor$new(rf.basic, data = df)
    pdp.obj = FeatureEffect$new(mod, feature = c("X"), method = "pdp") 
    p_pdp_mod <- plot(pdp.obj)
    
    simulation <- as.data.frame(pdp.obj$results[,1:2])
    simulation$run <- i
    colnames(simulation) <- c("X", "Y_hat", "run_number")
    pdp_simulations <- rbind.data.frame(pdp_simulations, simulation)
  }
  ggplot(data = pdp_simulations) + geom_line(data = pdp_simulations,aes(x = pdp_simulations[,1], y = pdp_simulations[,2], group = pdp_simulations[,3]), alpha = 0.5)
}
# standard deviation eq 0.1
test_plot_2 <- pdp_simulation(runs=20, N = 1000, error_std=0.1)
test_plot_2 <- test_plot_2 + labs(x = "X", y = "Y_hat")

# Theoretical PDP
all_dfs$EY <- all_dfs$Z+all_dfs$X
theoretpdp<- ggplot(data = all_dfs)+ geom_line(data = all_dfs,aes(x = all_dfs[,1], y = all_dfs[,4]),  alpha = 0.5)

# intervention

intervention <- function(runs, N, error_std){ 
  df_intervention_all <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("X", "Z", "Y"))
  for(i in 1:runs){
    
    e <- rnorm(N,0, error_std)
    Z <- runif(N, -1, 1)
    X <- Z + e
    Y <- X + Z + e
    df <- data.frame(X, Z, Y)
    for(j in 1:length(unique(df$X))){
      X <- unique(df$X)[j]
      Y <- X + Z + e
      df_intervention <- data.frame(X, Z, Y)
      df_intervention_all <- rbind.data.frame(df_intervention_all, df_intervention)
    }
  }
  
  df_intervention_group <- df_intervention_all %>%
    group_by(X) %>%
    summarise(Y_intervention = mean(Y),
              n = n()) %>%
    
    arrange(X)
  
  ggplot(aes(X, Y_intervention), data = df_intervention_group) + geom_line(colour="gold2", size = 1.5)
}
# standard deviation eq 0.1
test_int_plot_2 <- intervention(runs=1, N = 1000, error_std=0.1)
ggarrange(test_plot_2+ coord_cartesian(ylim=(-2:2)), theoretpdp  + labs(x = "X", y = "E[Y]"),test_int_plot_2 + coord_cartesian(ylim=(-2:2)), ncol= 3)
