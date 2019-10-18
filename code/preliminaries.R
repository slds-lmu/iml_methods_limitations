

## load packages
library(data.table)
library(mlr)
library(featureImportance)
library(mlbench)
library(ggplot2)
library(gridExtra)
library(dplyr)



pp_plot1_1 <- ggplot(data = NULL) +
  geom_point(aes(x=2, y=0.65), ) +
  xlim(1, 3) +
  ylim(0, 1) +
  ylab("delta L")

pp_plot1_1


## plot 2

x_plot <- c(1, 2, 3, 1, 2, 3)
y_plot <- c(0, 0.65, 0.85, 0.15, 0, 0.75, 0.35, 0.1, 0)
obs_id_plot <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)

data_plot <- as.data.frame(cbind( obs_id_plot, x_plot, y_plot))
data_plot

data_plot2 <- data_plot[1:3, ]




pp_plot2 <- ggplot(data = data_plot2, aes(x_plot, y_plot)) +
  geom_point() +
  geom_line(linetype = "dashed") +
  xlim(1, 3) +
  ylim(0, 1) +
  ylab("delta L")


data_plot3 <- data_plot[1:6, ]


pp_plot3 <- ggplot(data = data_plot3, aes(x_plot, y_plot)) +
  geom_point() +
  geom_line(aes(group = factor(data_plot3$obs_id_plot)), linetype = "dashed") +
  xlim(1, 3) +
  ylim(0, 1) +
  ylab("delta L")


## plot 3

pp_plot4 <- ggplot(data = data_plot, aes(x_plot, y_plot)) +
  geom_point() +
  geom_line(aes(group = factor(data_plot$obs_id_plot)), linetype = "dashed") +
  xlim(1, 3) +
  ylim(0, 1) +
  ylab("delta L")

x_average <- c(1, 2, 3)
y_average <- c(0.17, 0.25, 0.533)
data_average <- as.data.frame(cbind(x_average, y_average))



pp_plot5 <- ggplot(data = data_plot, aes(x_plot, y_plot)) +
  geom_point() +
  geom_line(aes(group = factor(data_plot$obs_id_plot)), linetype = "dashed") +
  geom_hline(yintercept = 0.317, color = "red") +
  geom_point(data = data_average, aes(x = x_average, y = y_average)) +
  geom_line(data = data_average, aes(x = x_average, y = y_average)) +
  xlim(1, 3) +
  ylim(0, 1) +
  ylab("delta L")
