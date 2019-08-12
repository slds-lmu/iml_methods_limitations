### Figure 1
panel_1 <- readRDS("R-results/LIME/Neighbourhood/fig1_1.RDS")
panel_2 <- readRDS("R-results/LIME/Neighbourhood/fig1_2.RDS")
png("images/04-09-01.png", width = 1300, height = 848)
grid.arrange(panel_1, panel_2, nrow = 1)
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