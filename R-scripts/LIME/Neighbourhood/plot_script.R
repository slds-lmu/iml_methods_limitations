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
kernel_widths <- readRDS("R-results/LIME/Neighbourhood/kw_univariate.RDS")
pred_frame <- readRDS("R-results/LIME/Neighbourhood/univariate_predframe.RDS")
test_1 <- readRDS("R-results/LIME/Neighbourhood/univariate_test_1.RDS")
test_2 <- readRDS("R-results/LIME/Neighbourhood/univariate_test_2.RDS")
result_1 <- readRDS("R-results/LIME/Neighbourhood/univariate_1.RDS")
result_2 <- readRDS("R-results/LIME/Neighbourhood/univariate_2.RDS")

fig6_1 <- ggplot(data = pred_frame, aes(x1, y_hat)) +
  geom_point(size = 5) +
  stat_function(fun = function(x) result_1[3, 1] + result_1[3, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[3]))) + 
  stat_function(fun = function(x) result_1[4, 1] + result_1[4, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[4]))) + 
  stat_function(fun = function(x) result_1[7, 1] + result_1[7, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[7]))) + 
  stat_function(fun = function(x) result_1[8, 1] + result_1[8, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[8]))) + 
  stat_function(fun = function(x) result_1[12, 1] + result_1[12, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[12]))) +
  stat_function(fun = function(x) result_1[22, 1] + result_1[22, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[22]))) +
  stat_function(fun = function(x) result_1[42, 1] + result_1[42, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[42])))
fig6_1 <- fig6_1 + scale_colour_manual("Kernel width", 
                                       values = c("red", "orange", "yellow", 
                                                  "darkgreen", "lightblue", "blue", 
                                                  "purple")) +
  geom_point(data = test_1, colour = "green", size = 8) +
  theme(text = element_text(size = 35)) +
  ylim(-1, 9) + labs(title = "True slope is negative.") + 
  theme(plot.title = element_text(hjust = 0.5)) + ylab("Predicted Value")

### This creates the second panel of plot 6: Different kernel widths for an
### observation where positive slope is appropriate.

fig6_2 <- ggplot(data = pred_frame, aes(x1, y_hat)) +
  geom_point(size = 5) +
  stat_function(fun = function(x) result_2[3, 1] + result_2[3, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[3]))) + 
  stat_function(fun = function(x) result_2[4, 1] + result_2[4, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[4]))) + 
  stat_function(fun = function(x) result_2[7, 1] + result_2[7, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[7]))) + 
  stat_function(fun = function(x) result_2[8, 1] + result_2[8, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[8]))) + 
  stat_function(fun = function(x) result_2[12, 1] + result_2[12, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[12]))) +
  stat_function(fun = function(x) result_2[22, 1] + result_2[22, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[22]))) +
  stat_function(fun = function(x) result_2[42, 1] + result_2[42, 2] * x, 
                size = 3, aes(colour = as.character(kernel_widths[42])))
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
