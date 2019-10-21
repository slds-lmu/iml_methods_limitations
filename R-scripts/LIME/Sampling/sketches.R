filename = paste0("images/convex_samples_1.png")
png(filename, width = 700L, height = 500L)
plot1 = plot_lime(sample_seed = 1)
plot1
dev.off()

filename = paste0("images/convex_samples_2.png")
png(filename, width = 700L, height = 500L)
plot2 = plot_lime(sample_seed = 2, ylab = "")
plot2
dev.off()

filename = paste0("images/convex_samples_1vs2.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()

#################

filename = paste0("images/nonconvex_samples_1.png")
png(filename, width = 700L, height = 500L)
plot1 = plot_lime(sample_seed = 1, model_smoothness = 50)
plot1
dev.off()

filename = paste0("images/nonconvex_samples_2.png")
png(filename, width = 700L, height = 500L)
plot2 = plot_lime(sample_seed = 2, model_smoothness = 50, ylab = "")
plot2
dev.off()

filename = paste0("images/nonconvex_samples_1vs2.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()


#################

filename = paste0("images/nonconvex_samples_size100_1.png")
png(filename, width = 700L, height = 500L)
plot1 = plot_lime(sample_seed = 1, model_smoothness = 50, sample_size = 100)
plot1
dev.off()

filename = paste0("images/nonconvex_samples_size100_2.png")
png(filename, width = 700L, height = 500L)
plot2 = plot_lime(sample_seed = 2, model_smoothness = 50, ylab = "", sample_size = 100)
plot2
dev.off()

filename = paste0("images/nonconvex_samples_size100_1vs2.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()


#################

filename = paste0("images/nonconvex_samples_width81000_1.png")
png(filename, width = 700L, height = 500L)
plot1 = plot_lime(sample_seed = 1, model_smoothness = 50, kernel_width = 81000)
plot1
dev.off()

filename = paste0("images/nonconvex_samples_width81000_2.png")
png(filename, width = 700L, height = 500L)
plot2 = plot_lime(sample_seed = 2, model_smoothness = 50, ylab = "", kernel_width = 81000)
plot2
dev.off()

filename = paste0("images/nonconvex_samples_width81000_1vs2.png")
png(filename, width = 1400L, height = 500L)
gridExtra::grid.arrange(plot1, plot2, nrow = 1)
dev.off()
