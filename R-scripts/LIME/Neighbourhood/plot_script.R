### Figure 1
panel_1_1 <- readRDS("R-results/LIME/Neighbourhood/fig1_1.RDS")
panel_1_2 <- readRDS("R-results/LIME/Neighbourhood/fig1_2.RDS")
png("images/04-09-01.png", width = 1300, height = 848)
grid.arrange(panel_1_1, panel_1_2, nrow = 1)
dev.off()

### Figure 2
fig2 <- readRDS("R-results/LIME/Neighbourhood/fig2.RDS")
png("images/04-09-02.png", width = 1000, height = 848)
fig2
dev.off()

### Figure 3
fig3 <- readRDS("R-results/LIME/Neighbourhood/fig3.RDS")
png("images/04-09-03.png", width = 1000, height = 848)
fig3
dev.off()

### Figure 4
fig4 <- readRDS("R-results/LIME/Neighbourhood/fig4.RDS")
png("images/04-09-04.png", width = 1000, height = 848)
fig4
dev.off()

### Figure 5
local_model1 <- readRDS("R-results/LIME/Neighbourhood/univariate_local_1.RDS")
local_model2 <- readRDS("R-results/LIME/Neighbourhood/univariate_local_2.RDS")
kw <- c(0.08, 2)
Fun <- function(x, local_model) {
  local_model[1] + local_model[2] * x
}
fig5 <- fig3 + stat_function(fun = Fun, size = 2, 
                             args = list(local_model = local_model1),
                             aes(colour = as.character(kw[1])))  +
  stat_function(fun = Fun, size = 2, 
                args = list(local_model = local_model2),
                aes(colour = as.character(kw[2]))) +
  coord_cartesian(ylim = c(-1, 9)) +
  geom_point(data = test_1, colour = "green", size = 8) +
  scale_colour_manual("Kernel width", 
                      values = c("red", "yellow")) + ylab("Predicted Value")

png("04-09-05.png", width = 1000, height = 848)
fig5
dev.off()

### Figure 6
kernel_widths_8_6 <- readRDS("R-results/LIME/Neighbourhood/kw_univariate.RDS")
pred_frame <- readRDS("R-results/LIME/Neighbourhood/univariate_predframe.RDS")
test_1 <- readRDS("R-results/LIME/Neighbourhood/univariate_test_1.RDS")
test_2 <- readRDS("R-results/LIME/Neighbourhood/univariate_test_2.RDS")
result_1 <- readRDS("R-results/LIME/Neighbourhood/univariate_1.RDS")
result_2 <- readRDS("R-results/LIME/Neighbourhood/univariate_2.RDS")

fig6_1 <- ggplot(data = pred_frame, aes(x1, y_hat)) +
  geom_point(size = 5) +
  stat_function(fun = function(x) result_1[3, 1] + result_1[3, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[3]))) + 
  stat_function(fun = function(x) result_1[4, 1] + result_1[4, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[4]))) + 
  stat_function(fun = function(x) result_1[7, 1] + result_1[7, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[7]))) + 
  stat_function(fun = function(x) result_1[8, 1] + result_1[8, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[8]))) + 
  stat_function(fun = function(x) result_1[12, 1] + result_1[12, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[12]))) +
  stat_function(fun = function(x) result_1[22, 1] + result_1[22, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[22]))) +
  stat_function(fun = function(x) result_1[42, 1] + result_1[42, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[42])))
fig6_1 <- fig6_1 + scale_colour_manual("Kernel width", 
                                       values = c("red", "orange", "yellow", 
                                                  "darkgreen", "lightblue", 
                                                  "blue", "purple")) +
  geom_point(data = test_1, colour = "green", size = 8) +
  theme(text = element_text(size = 35)) +
  ylim(-1, 9) + labs(title = "True slope is negative.") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Predicted Value")

### This creates the second panel of plot 6: Different kernel widths for an
### observation where positive slope is appropriate.

fig6_2 <- ggplot(data = pred_frame, aes(x1, y_hat)) +
  geom_point(size = 5) +
  stat_function(fun = function(x) result_2[3, 1] + result_2[3, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[3]))) + 
  stat_function(fun = function(x) result_2[4, 1] + result_2[4, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[4]))) + 
  stat_function(fun = function(x) result_2[7, 1] + result_2[7, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[7]))) + 
  stat_function(fun = function(x) result_2[8, 1] + result_2[8, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[8]))) + 
  stat_function(fun = function(x) result_2[12, 1] + result_2[12, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[12]))) +
  stat_function(fun = function(x) result_2[22, 1] + result_2[22, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[22]))) +
  stat_function(fun = function(x) result_2[42, 1] + result_2[42, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths_8_6[42])))
fig6_2 <- fig6_2 + scale_colour_manual("Kernel width", 
                                       values = c("red", "orange", "yellow",
                                                  "darkgreen", "lightblue", 
                                                  "blue", "purple")) +
  geom_point(data = test_2, colour = "green", size = 8) +
  theme(text = element_text(size = 35)) +
  ylim(-1, 9) + labs(title = "True slope is positive.") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Predicted Value")

png("images/04-09-06.png", width = 1000, height = 1760)
grid.arrange(fig6_1, fig6_2, nrow = 2)
dev.off()

### Figure 7
task_pred_lm <- readRDS("R-results/LIME/Neighbourhood/task_pred_lm.RDS")
png("images/04-09-07.png", width = 1000, height = 848)
ggplot(data = task_pred_lm$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35))
dev.off()

### Figure 8
kernel_widths_8 <- readRDS("R-results/LIME/Neighbourhood/kw_global_linear.RDS")
kernel_matrix1 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-global_linear1.RDS")
kernel_matrix2 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-global_linear2.RDS")
kernel_matrix3 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-global_linear3.RDS")
kernel_matrix4 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-global_linear4.RDS")

fig8_1 <- plot_kernels(kernel_matrix1, kernel_widths_8, c(4, -3, 5), "",
                       ymin = -7, ymax = 13)
fig8_2 <- plot_kernels(kernel_matrix2, kernel_widths_8, c(4, -3, 5), "",
                       ymin = -5, ymax = 8)
fig8_3 <- plot_kernels(kernel_matrix3, kernel_widths_8, c(4, -3, 5), "",
                       ymin = -5, ymax = 8)
fig8_4 <- plot_kernels(kernel_matrix4, kernel_widths_8, c(4, -3, 5), "",
                       ymin = -5, ymax = 8)

png("images/04-09-08.png", width = 2000, height = 1700)
grid.arrange(fig8_1, fig8_2, fig8_3, fig8_4, ncol = 2)
dev.off()

### Figure 9
task_pred_mars1 <- readRDS("R-results/LIME/Neighbourhood/task_pred_mars1.RDS")
png("images/04-09-09.png", width = 1000, height = 848)
ggplot(data = task_pred_mars1$data, aes(x = response, y = truth)) +
  geom_point(size = 3) +
  theme(text = element_text(size = 35)) + stat_function(fun = function(x) x)
dev.off()

### Figure 10

km_1_1 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-local_linear1_1.RDS")
km_1_2 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-local_linear1_2.RDS")
km_2_1 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-local_linear2_1.RDS")
km_2_2 <- 
  readRDS("R-results/LIME/Neighbourhood/kernelmatrix-local_linear2_2.RDS")
kernel_widths_10 <- readRDS("R-results/LIME/Neighbourhood/kw_local_linear.RDS")

km_nc_1_1 <- km_1_1[[1]]
km_nc_1_2 <- km_1_2[[1]]
km_nc_2_1 <- km_2_1[[1]]
km_nc_2_2 <- km_2_2[[1]]

fig10_1 <- plot_kernels(km_nc_1_1, 
                       kernel_widths_10, 
                       true_coefficients = c(5, -4, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x1 is 5.")

fig10_2 <- plot_kernels(km_nc_1_2, 
                       kernel_widths_10, 
                       true_coefficients = c(5, -4, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x1 is 5.")

fig10_3 <- plot_kernels(km_nc_2_1, 
                       kernel_widths_10, 
                       true_coefficients = c(0, -4, 3),
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x1 is 0.")

fig10_4 <- plot_kernels(km_nc_2_2, 
                       kernel_widths_10, 
                       true_coefficients = c(0, -4, 3), 
                       ymin = -10, ymax = 10,
                       title = "True local coefficient for x1 is 0.")

png("images/04-09-10.png", width = 2000, height = 1700)
grid.arrange(panel1, panel2, panel3, panel4, nrow = 2)
dev.off()

### Figure 10 (comp.: only for presentation)
true_data <- simulate_data(2500, 
                           3,
                           piece_wise_intervals = list(
                             list(lower = -10, upper = 5), NULL,
                             NULL), 
                           seed = 1, 
                           mu = c(5, 5, 5), 
                           Sigma = matrix(
                             c(0.6, 0, 0, 0, 0.8, 0, 0, 0, 0.6),
                             ncol = 3, nrow = 3, byrow = TRUE), 
                           true_coefficients = c(5, 0, 0), 
                           intercept = 2.5,
                           shock = 0)

png("images/04-09-10-comp.png", width = 1000, height = 848)
ggplot(true_data, aes(y = y, x = x1)) +
  geom_line(size = 2.5) +
  theme(text = element_text(size = 35)) + xlab("x1") + ylab("y")
dev.off()

### Figure 11 (all possible 3 plots)
x1_frame <- as.data.frame(cbind(kernel_widths_10,
                                km_2_1[[1]][[2]], 
                                km_2_1[[2]][[2]], 
                                km_2_1[[3]][[2]]))

x2_frame <- as.data.frame(cbind(kernel_widths_10,
                                km_2_1[[1]][[3]], 
                                km_2_1[[2]][[3]], 
                                km_2_1[[3]][[3]]))

x3_frame <- as.data.frame(cbind(kernel_widths_10,
                                km_2_1[[1]][[4]], 
                                km_2_1[[2]][[4]], 
                                km_2_1[[3]][[4]]))

colnames(x1_frame) <- c("Kernel", "Mean", "lower", "upper")
colnames(x2_frame) <- c("Kernel", "Mean", "lower", "upper")
colnames(x3_frame) <- c("Kernel", "Mean", "lower", "upper")

x1_plot <- x1_frame
x1_plot[x1_plot > 7] <- 7
x1_plot[x1_plot < -7] <- -7
png("04-09-11-1.png", width = 1000, height = 848)
ggplot(x1_plot, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = x1_plot, size = 3) +
  geom_ribbon(data = x1_plot, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) 0) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") + 
  xlab("Kernel width")
dev.off()

x2_plot <- x2_frame
x2_plot[x2_plot > 3] <- 3
x2_plot[x2_plot < -11] <- -11
png("04-09-11-2.png", width = 1000, height = 848)
ggplot(x2_plot, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = x2_plot, size = 3) +
  geom_ribbon(data = x2_plot, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) -4) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") + 
  xlab("Kernel width")
dev.off()

x3_plot <- x3_frame
x3_plot[x3_plot > 7] <- 10
x3_plot[x3_plot < -4] <- -4
png("04-09-11-3.png", width = 1000, height = 848)
ggplot(x3_plot, aes(y = Mean, x = Kernel)) +
  geom_point(size = 3) +
  geom_line(data = x3_plot, size = 3) +
  geom_ribbon(data = x3_plot, aes(ymin = lower, ymax = upper), 
              alpha = 0.3) + geom_path(size = 1.5, stat = 'function', 
                                       fun = function(x) 3) +
  theme(text = element_text(size = 35)) + ylab("Coefficient") +
  xlab("Kernel width")
dev.off()
