### Figure 15
gower_model <- analyse_multivariate_kernel_width(1,
                                                 obs_2[, 2:4], 
                                                 explainer,
                                                 n_features = 3, 
                                                 n_permutations = 6500, 
                                                 dist_fun = "gower",
                                                 seed = 1,
                                                 ci = TRUE,
                                                 feature_select = "auto",
                                                 iterations = 48)[[1]]

frame <- data.frame(coefficient = km_2[[1]][, 3],
                    gower = rep(gower_model[[3]], length(kernel_widths)),
                    true = rep(6, length(kernel_widths)))
melted_frame <- data.frame(kernel = rep(kernel_widths, 3), melt(frame))

png("04-09-15.png", width = 1000, height = 848)
ggplot(data = melted_frame, aes(x = kernel, 
                                y = value, group = variable)) +
  geom_line(aes(color = variable), size = 1.75) + 
  scale_color_discrete(name = "Local coefficent",
                       breaks = c("coefficient", "gower", "true"),
                       labels = c("Euclidean distance", 
                                  "Gower distance", 
                                  "True local coefficient")) +
  geom_point(data = melted_frame[1:34, ], aes(color = variable), size = 3) +
  theme(text = element_text(size = 35)) + xlab("Coefficient") + 
  ylab("Kernel width")
dev.off()