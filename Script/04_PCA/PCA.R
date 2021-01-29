## Subject: PCA
## Project: Fly Choice Diet
## Part: 4
## Author: Yang Lyu
## Date created: 02/14/2018
## Date modified: 05/31/2020

# Environment Settings ----------------------------------------------------
library(ggbiplot)
library(egg) # multiplot with ggplot2

# Data Input --------------------------------------------------------------
## Read data
head.data <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Head.RDS")
body.data <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Body.RDS")

# Data Clean --------------------------------------------------------------
## Collect treatment information
head.sample <- as.data.frame(matrix(unlist(strsplit(colnames(head.data), "_")), 
                                    ncol = 4, byrow = T))
body.sample <- as.data.frame(matrix(unlist(strsplit(colnames(body.data), "_")), 
                                    ncol = 4, byrow = T))

colnames(head.sample) = c("Genotype", "Diet", "Tissue", "Replicate")
colnames(body.sample) = c("Genotype", "Diet", "Tissue", "Replicate")

head.sample$group <- paste(head.sample$Genotype, head.sample$Diet, sep = "_")
body.sample$group <- paste(body.sample$Genotype, body.sample$Diet, sep = "_")

head.sample$group <- factor(head.sample$group,
                            levels = c("w1118_FD", "w1118_FD2CD", "w1118_CD", "w1118_CD2FD",
                                       "5-HT2A_FD", "5-HT2A_FD2CD", "5-HT2A_CD", "5-HT2A_CD2FD"))

body.sample$group <- factor(body.sample$group,
                            levels = c("w1118_FD", "w1118_FD2CD", "w1118_CD", "w1118_CD2FD",
                                       "5-HT2A_FD","5-HT2A_FD2CD", "5-HT2A_CD", "5-HT2A_CD2FD"))

head.data.tr <- as.data.frame(t(head.data))
body.data.tr <- as.data.frame(t(body.data))

head.data.tr$group <- head.sample$group
body.data.tr$group <- body.sample$group

head.group <- as.factor(head.data.tr$group)
body.group <- as.factor(body.data.tr$group)

# PCA ---------------------------------------------------------------------
head.pca <- prcomp(as.matrix(head.data.tr[ , -ncol(head.data.tr)]), center = T, scale. = F)
body.pca <- prcomp(as.matrix(body.data.tr[ , -ncol(body.data.tr)]), center = T, scale. = F)

head.pc.var <- summary(head.pca)$importance[2,]
body.pc.var <- summary(body.pca)$importance[2,]

barplot(height = head.pc.var, names.arg = names(head.pc.var), 
        ylim = c(0, 0.6), cex.names = 0.5, las = 2, xlab = "PC", ylab = "Proportion of Variance")

barplot(height = body.pc.var, names.arg = names(body.pc.var), 
        ylim = c(0, 0.6), cex.names = 0.5, las = 2, xlab = "PC", ylab = "Proportion of Variance")

p1 <- ggbiplot(head.pca, obs.scale = 1, var.scale = 1, groups = head.group, ellipse = TRUE, choices = c(2, 3), alpha = 0, var.axes = F) +
    theme(legend.direction = 'vertical', legend.position = "right") + 
    scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black")) +
    scale_fill_manual(values = c("black", "grey70", "orange", "gold", "royalblue", "skyblue", "red", "pink")) + 
    geom_point(size = 3, shape = 21, aes(fill = groups, color = groups))

p2 <- ggbiplot(head.pca, obs.scale = 1, var.scale = 1, groups = head.group, ellipse = TRUE, choices = c(2, 6), alpha = 0, var.axes = F) +
    theme(legend.direction = 'vertical', legend.position = "right") + 
    scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black")) +
    scale_fill_manual(values = c("black", "grey70", "orange", "gold", "royalblue", "skyblue", "red", "pink")) + 
    geom_point(size = 3, shape = 21, aes(fill = groups, color = groups))

p3 <- ggbiplot(body.pca, obs.scale = 1, var.scale = 1, groups = body.group, ellipse = TRUE, choices = c(3, 2), alpha = 0, var.axes = F) +
    theme(legend.direction = 'vertical', legend.position = "right") + 
    scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black")) +
    scale_fill_manual(values = c("black", "grey70", "orange", "gold", "royalblue", "skyblue", "red", "pink")) + 
    geom_point(size = 3, shape = 21, aes(fill = groups, color = groups))

p4 <- ggbiplot(body.pca, obs.scale = 1, var.scale = 1, groups = body.group, ellipse = TRUE, choices = c(3, 6), alpha = 0, var.axes = F) +
    theme(legend.direction = 'vertical', legend.position = "right") + 
    scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black")) +
    scale_fill_manual(values = c("black", "grey70", "orange", "gold", "royalblue", "skyblue", "red", "pink")) + 
    geom_point(size = 3, shape = 21, aes(fill = groups, color = groups))

grid.arrange(grobs = lapply(
    list(p1, p2, p3, p4),
    set_panel_size,
    width = unit(2.5, "in"),
    height = unit(2, "in")
))

# PCA matrix
df <- data.frame()
p0 <- ggplot(df) + 
  geom_point() + xlim(0, 10) + ylim(0, 100) +
  theme_minimal() +
  theme(legend.position = "none")

# Heads
Q <- list()
for(i in 1:5) {
  for(j in (i+1):6) {
    Q[[length(Q) + 1]] <- 
      ggbiplot(head.pca, obs.scale = 1, var.scale = 1, groups = head.group, ellipse = TRUE, choices = c(i, j), alpha = 0, var.axes = F) +
      theme(legend.direction = 'vertical', legend.position = "right") + 
      scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black")) +
      scale_fill_manual(values = c("black", "grey70", "orange", "gold", "royalblue", "skyblue", "red", "pink")) + 
      geom_point(size = 3, shape = 21, aes(fill = groups, color = groups)) +
      theme_minimal() +
      theme(legend.position = "none")
  }
}

grid.arrange(p0, Q[[1]], Q[[2]], Q[[3]], Q[[4]],  Q[[5]], 
             p0, p0,     Q[[6]], Q[[7]], Q[[8]],  Q[[9]], 
             p0, p0,     p0,     Q[[10]],Q[[11]], Q[[12]],
             p0, p0,     p0,     p0,     Q[[13]], Q[[14]],
             p0, p0,     p0,     p0,     p0,      Q[[15]], 
             ncol = 6)

# Bodies
Q <- list()
for(i in 1:5) {
  for(j in (i+1):6) {
    Q[[length(Q) + 1]] <- 
      ggbiplot(body.pca, obs.scale = 1, var.scale = 1, groups = body.group, ellipse = TRUE, choices = c(i, j), alpha = 0, var.axes = F) +
      theme(legend.direction = 'vertical', legend.position = "right") + 
      scale_color_manual(values = c("black", "black", "black", "black", "black", "black", "black", "black")) +
      scale_fill_manual(values = c("black", "grey70", "orange", "gold", "royalblue", "skyblue", "red", "pink")) + 
      geom_point(size = 3, shape = 21, aes(fill = groups, color = groups)) +
      theme_minimal() +
      theme(legend.position = "none")
  }
}

grid.arrange(p0, Q[[1]], Q[[2]], Q[[3]], Q[[4]],  Q[[5]], 
             p0, p0,     Q[[6]], Q[[7]], Q[[8]],  Q[[9]], 
             p0, p0,     p0,     Q[[10]],Q[[11]], Q[[12]],
             p0, p0,     p0,     p0,     Q[[13]], Q[[14]],
             p0, p0,     p0,     p0,     p0,      Q[[15]], 
             ncol = 6)

