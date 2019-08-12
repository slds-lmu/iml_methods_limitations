source("R-scripts/packages.R")
set.seed(1)
example <- data.frame(x = rnorm(1000, 0, 1))
p1 <- ggplot(example, aes(x)) + stat_density() + 
  theme(text = element_text(size = 35))
p2 <- ggplot(example, aes(x)) + stat_density(bw = 0.01) +
  theme(text = element_text(size = 35))
fig1 <- grid.arrange(p1, p2, nrow = 1)
saveRDS(fig1, file = "R-results/LIME/Neighbourhood/fig1.RDS")
