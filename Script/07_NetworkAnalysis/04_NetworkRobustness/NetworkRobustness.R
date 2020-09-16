## Subject: Network Analysis - Network Robustness
## Project: Fly Choice Diet
## Part: 7-4
## Author: Yang Lyu
## Date created: 10/01/2019
## Date modified: 08/19/2020

# Environment Settings ----------------------------------------------------
library(igraph)
library(ggplot2)
library(easyGgplot2)
source("Script/07_NetworkAnalysis/01_NetworkConstruction/multiplot.R")

# Data Input --------------------------------------------------------------
## Read data
load("Data/ProcessedData/NetworkObject-FlyChoiceDiet.Rdata")

# Network Fail ------------------------------------------------------------
## Network fail, 10000 simulations
fail.head.wt.fd <- matrix(NA, nrow = 1000, ncol = 30)
fail.head.wt.cd <- matrix(NA, nrow = 1000, ncol = 30)
fail.head.ht.fd <- matrix(NA, nrow = 1000, ncol = 30)
fail.head.ht.cd <- matrix(NA, nrow = 1000, ncol = 30)
fail.body.wt.fd <- matrix(NA, nrow = 1000, ncol = 40)
fail.body.wt.cd <- matrix(NA, nrow = 1000, ncol = 40)
fail.body.ht.fd <- matrix(NA, nrow = 1000, ncol = 40)
fail.body.ht.cd <- matrix(NA, nrow = 1000, ncol = 40)

for (i in 1:1000) {
    delete.head.wt.fd.gc <- sample(V(head.wt.fd.gc), 30, replace = F)
    delete.head.wt.cd.gc <- sample(V(head.wt.cd.gc), 30, replace = F)
    delete.head.ht.fd.gc <- sample(V(head.ht.fd.gc), 30, replace = F)
    delete.head.ht.cd.gc <- sample(V(head.ht.cd.gc), 30, replace = F)
    delete.body.wt.fd.gc <- sample(V(body.wt.fd.gc), 40, replace = F)
    delete.body.wt.cd.gc <- sample(V(body.wt.cd.gc), 40, replace = F)
    delete.body.ht.fd.gc <- sample(V(body.ht.fd.gc), 40, replace = F)
    delete.body.ht.cd.gc <- sample(V(body.ht.cd.gc), 40, replace = F)
    for (j in 1:30) {
        fail.head.wt.fd.gc <- delete_vertices(head.wt.fd.gc, delete.head.wt.fd.gc[1:j])
        fail.head.wt.cd.gc <- delete_vertices(head.wt.cd.gc, delete.head.wt.cd.gc[1:j])
        fail.head.ht.fd.gc <- delete_vertices(head.ht.fd.gc, delete.head.ht.fd.gc[1:j])
        fail.head.ht.cd.gc <- delete_vertices(head.ht.cd.gc, delete.head.ht.cd.gc[1:j])
        fail.head.wt.fd[i, j]  <- average.path.length(fail.head.wt.fd.gc)
        fail.head.wt.cd[i, j]  <- average.path.length(fail.head.wt.cd.gc)
        fail.head.ht.fd[i, j]  <- average.path.length(fail.head.ht.fd.gc)
        fail.head.ht.cd[i, j]  <- average.path.length(fail.head.ht.cd.gc)
    }
    for (j in 1:40) {
        fail.body.wt.fd.gc <- delete_vertices(body.wt.fd.gc, delete.body.wt.fd.gc[1:j])
        fail.body.wt.cd.gc <- delete_vertices(body.wt.cd.gc, delete.body.wt.cd.gc[1:j])
        fail.body.ht.fd.gc <- delete_vertices(body.ht.fd.gc, delete.body.ht.fd.gc[1:j])
        fail.body.ht.cd.gc <- delete_vertices(body.ht.cd.gc, delete.body.ht.cd.gc[1:j])
        
        fail.body.wt.fd[i, j] <- average.path.length(fail.body.wt.fd.gc)
        fail.body.wt.cd[i, j] <- average.path.length(fail.body.wt.cd.gc)
        fail.body.ht.fd[i, j] <- average.path.length(fail.body.ht.fd.gc)
        fail.body.ht.cd[i, j] <- average.path.length(fail.body.ht.cd.gc)
    }
}

delta.fail.head.wt.fd <- 
  (fail.head.wt.fd - average.path.length(head.wt.fd.gc)) / 
   average.path.length(head.wt.fd.gc) * 100
delta.fail.head.wt.cd <- 
  (fail.head.wt.cd - average.path.length(head.wt.cd.gc)) / 
   average.path.length(head.wt.cd.gc) * 100
delta.fail.head.ht.fd <- 
  (fail.head.ht.fd - average.path.length(head.ht.fd.gc)) / 
   average.path.length(head.ht.fd.gc) * 100
delta.fail.head.ht.cd <- 
  (fail.head.ht.cd - average.path.length(head.ht.cd.gc)) / 
  average.path.length(head.ht.cd.gc) * 100

delta.fail.body.wt.fd <- 
  (fail.body.wt.fd - average.path.length(body.wt.fd.gc)) / 
   average.path.length(body.wt.fd.gc) * 100
delta.fail.body.wt.cd <- 
  (fail.body.wt.cd - average.path.length(body.wt.cd.gc)) / 
   average.path.length(body.wt.cd.gc) * 100
delta.fail.body.ht.fd <- 
  (fail.body.ht.fd - average.path.length(body.ht.fd.gc))  / 
   average.path.length(body.ht.fd.gc) * 100
delta.fail.body.ht.cd <- 
  (fail.body.ht.cd - average.path.length(body.ht.cd.gc)) / 
   average.path.length(body.ht.cd.gc) * 100

delta.fail.head.df <- 
  data.frame(remove = rep(1:30, 4000),
             delta = c(as.vector(t(delta.fail.head.wt.fd)), 
                       as.vector(t(delta.fail.head.wt.cd)),
                       as.vector(t(delta.fail.head.ht.fd)), 
                       as.vector(t(delta.fail.head.ht.cd))),
             group = rep(c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"), 
                         each = 30000))

delta.fail.body.df <- 
  data.frame(remove = rep(1:40, 4000), 
             delta = c(as.vector(t(delta.fail.body.wt.fd)), 
                       as.vector(t(delta.fail.body.wt.cd)), 
                       as.vector(t(delta.fail.body.ht.fd)), 
                       as.vector(t(delta.fail.body.ht.cd))), 
             group = rep(c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"), 
                         each = 40000))

data.summary <- function(data, varname, groupnames) {
    require(plyr)
    summary.func <- function(x, col){
        c(mean = mean(x[[col]], na.rm = TRUE),
          sd = sd(x[[col]], na.rm = TRUE))
    }
    data.sum <-ddply(data, groupnames, .fun = summary.func,
                    varname)
    data.sum <- rename(data.sum, c("mean" = varname))
    return(data.sum)
}

delta.fail.head.sum <- data.summary(delta.fail.head.df, varname = "delta",
                                    groupnames = c("group", "remove"))
delta.fail.body.sum <- data.summary(delta.fail.body.df, varname = "delta",
                                    groupnames = c("group", "remove"))

x <- matrix(unlist(strsplit(delta.fail.head.sum$group, "/")), 
            ncol = 2, byrow = T)
y <- matrix(unlist(strsplit(delta.fail.body.sum$group, "/")), 
            ncol = 2, byrow = T)

delta.fail.head.sum$genotype <- factor(x[ ,1], levels = c("w1118", "5-HT2A"))
delta.fail.body.sum$genotype <- factor(y[ ,1], levels = c("w1118", "5-HT2A"))

delta.fail.head.sum$diet <- factor(x[ ,2], levels = c("FD", "CD"))
delta.fail.body.sum$diet <- factor(y[ ,2], levels = c("FD", "CD"))

delta.fail.head.sum$group <- 
  factor(delta.fail.head.sum$group, 
         levels = c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"))
delta.fail.body.sum$group <- 
  factor(delta.fail.body.sum$group, 
         levels = c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"))

p1 <- ggplot(delta.fail.head.sum, 
       aes(x = remove, y = delta, group = group, 
           shape = diet, linetype = diet, color = genotype)) + 
  geom_errorbar(aes(ymin = delta - sd, ymax = delta + sd), width = .1, 
                position = position_dodge(0.1)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(xlim = c(0, 20), ylim = c(-50, 50)) + 
  labs(title = "Network Fail in Head", 
       x = "# of Nodes Removed", 
       y = expression(paste(Delta, " Average Shortest Distance (%)"))) + 
  theme_classic() + 
  scale_color_manual(values = c('#555555','#5399FF'))

p2 <- ggplot(delta.fail.body.sum, 
             aes(x = remove, y = delta, group = group, 
                 shape = diet, linetype = diet, color = genotype)) + 
  geom_errorbar(aes(ymin = delta - sd, ymax = delta + sd), width = .1, 
                position = position_dodge(1)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(xlim = c(0, 30), ylim = c(-50, 50)) + 
  labs(title = "Network Fail in Body", 
       x = "# of Nodes Removed", 
       y = expression(paste(Delta, " Average Shortest Distance (%)"))) + 
  theme_classic() + 
  scale_color_manual(values = c('#555555', '#5399FF'))

# Network Attack ----------------------------------------------------------
## Remove nodes
attack.degree.head.wt.fd <- numeric()
attack.degree.head.wt.cd <- numeric()
attack.degree.head.ht.fd <- numeric()
attack.degree.head.ht.cd <- numeric()
attack.degree.body.wt.fd <- numeric()
attack.degree.body.wt.cd <- numeric()
attack.degree.body.ht.fd <- numeric()
attack.degree.body.ht.cd <- numeric()

attack.eigen.head.wt.fd <- numeric()
attack.eigen.head.wt.cd <- numeric()
attack.eigen.head.ht.fd <- numeric()
attack.eigen.head.ht.cd <- numeric()
attack.eigen.body.wt.fd <- numeric()
attack.eigen.body.wt.cd <- numeric()
attack.eigen.body.ht.fd <- numeric()
attack.eigen.body.ht.cd <- numeric()

head.wt.fd.degree.names <- 
  names(degree(head.wt.fd.gc)[order(degree(head.wt.fd.gc), decreasing = T)])
head.wt.cd.degree.names <- 
  names(degree(head.wt.cd.gc)[order(degree(head.wt.cd.gc), decreasing = T)])
head.ht.fd.degree.names <- 
  names(degree(head.ht.fd.gc)[order(degree(head.ht.fd.gc), decreasing = T)])
head.ht.cd.degree.names <- 
  names(degree(head.ht.cd.gc)[order(degree(head.ht.cd.gc), decreasing = T)])
body.wt.fd.degree.names <- 
  names(degree(body.wt.fd.gc)[order(degree(body.wt.fd.gc), decreasing = T)])
body.wt.cd.degree.names <- 
  names(degree(body.wt.cd.gc)[order(degree(body.wt.cd.gc), decreasing = T)])
body.ht.fd.degree.names <- 
  names(degree(body.ht.fd.gc)[order(degree(body.ht.fd.gc), decreasing = T)])
body.ht.cd.degree.names <- 
  names(degree(body.ht.cd.gc)[order(degree(body.ht.cd.gc), decreasing = T)])

head.wt.fd.eigen.names <- 
  names(evcent(head.wt.fd.gc)$vector[order(evcent(head.wt.fd.gc)$vector, decreasing = T)])
head.wt.cd.eigen.names <- 
  names(evcent(head.wt.cd.gc)$vector[order(evcent(head.wt.cd.gc)$vector, decreasing = T)])
head.ht.fd.eigen.names <- 
  names(evcent(head.ht.fd.gc)$vector[order(evcent(head.ht.fd.gc)$vector, decreasing = T)])
head.ht.cd.eigen.names <- 
  names(evcent(head.ht.cd.gc)$vector[order(evcent(head.ht.cd.gc)$vector, decreasing = T)])
body.wt.fd.eigen.names <- 
  names(evcent(body.wt.fd.gc)$vector[order(evcent(body.wt.fd.gc)$vector, decreasing = T)])
body.wt.cd.eigen.names <- 
  names(evcent(body.wt.cd.gc)$vector[order(evcent(body.wt.cd.gc)$vector, decreasing = T)])
body.ht.fd.eigen.names <- 
  names(evcent(body.ht.fd.gc)$vector[order(evcent(body.ht.fd.gc)$vector, decreasing = T)])
body.ht.cd.eigen.names <- 
  names(evcent(body.ht.cd.gc)$vector[order(evcent(body.ht.cd.gc)$vector, decreasing = T)])

for (i in 1:20) {
    attack.degree.head.wt.fd.gc <- 
      delete_vertices(head.wt.fd.gc, head.wt.fd.degree.names[1:i])
    attack.degree.head.wt.cd.gc <- 
      delete_vertices(head.wt.cd.gc, head.wt.cd.degree.names[1:i])
    attack.degree.head.ht.fd.gc <- 
      delete_vertices(head.ht.fd.gc, head.ht.fd.degree.names[1:i])
    attack.degree.head.ht.cd.gc <- 
      delete_vertices(head.ht.cd.gc, head.ht.cd.degree.names[1:i])
    
    attack.eigen.head.wt.fd.gc <- 
      delete_vertices(head.wt.fd.gc, head.wt.fd.eigen.names[1:i])
    attack.eigen.head.wt.cd.gc <- 
      delete_vertices(head.wt.cd.gc, head.wt.cd.eigen.names[1:i])
    attack.eigen.head.ht.fd.gc <- 
      delete_vertices(head.ht.fd.gc, head.ht.fd.eigen.names[1:i])
    attack.eigen.head.ht.cd.gc <- 
      delete_vertices(head.ht.cd.gc, head.ht.cd.eigen.names[1:i])
    
    attack.degree.head.wt.fd[i] <- 
      average.path.length(attack.degree.head.wt.fd.gc)
    attack.degree.head.wt.cd[i] <- 
      average.path.length(attack.degree.head.wt.cd.gc)
    attack.degree.head.ht.fd[i] <- 
      average.path.length(attack.degree.head.ht.fd.gc)
    attack.degree.head.ht.cd[i] <- 
      average.path.length(attack.degree.head.ht.cd.gc)
    
    attack.eigen.head.wt.fd[i] <- 
      average.path.length(attack.eigen.head.wt.fd.gc)
    attack.eigen.head.wt.cd[i] <- 
      average.path.length(attack.eigen.head.wt.cd.gc)
    attack.eigen.head.ht.fd[i] <- 
      average.path.length(attack.eigen.head.ht.fd.gc)
    attack.eigen.head.ht.cd[i] <- 
      average.path.length(attack.eigen.head.ht.cd.gc)
}

for (i in 1:30) {
    attack.degree.body.wt.fd.gc <- 
      delete_vertices(body.wt.fd.gc, body.wt.fd.degree.names[1:i])
    attack.degree.body.wt.cd.gc <- 
      delete_vertices(body.wt.cd.gc, body.wt.cd.degree.names[1:i])
    attack.degree.body.ht.fd.gc <- 
      delete_vertices(body.ht.fd.gc, body.ht.fd.degree.names[1:i])
    attack.degree.body.ht.cd.gc <- 
      delete_vertices(body.ht.cd.gc, body.ht.cd.degree.names[1:i])
    
    attack.eigen.body.wt.fd.gc <- 
      delete_vertices(body.wt.fd.gc, body.wt.fd.eigen.names[1:i])
    attack.eigen.body.wt.cd.gc <- 
      delete_vertices(body.wt.cd.gc, body.wt.cd.eigen.names[1:i])
    attack.eigen.body.ht.fd.gc <- 
      delete_vertices(body.ht.fd.gc, body.ht.fd.eigen.names[1:i])
    attack.eigen.body.ht.cd.gc <- 
      delete_vertices(body.ht.cd.gc, body.ht.cd.eigen.names[1:i])

    attack.degree.body.wt.fd[i] <- 
      average.path.length(attack.degree.body.wt.fd.gc)
    attack.degree.body.wt.cd[i] <- 
      average.path.length(attack.degree.body.wt.cd.gc)
    attack.degree.body.ht.fd[i] <- 
      average.path.length(attack.degree.body.ht.fd.gc)
    attack.degree.body.ht.cd[i] <- 
      average.path.length(attack.degree.body.ht.cd.gc)

    attack.eigen.body.wt.fd[i] <- 
      average.path.length(attack.eigen.body.wt.fd.gc)
    attack.eigen.body.wt.cd[i] <- 
      average.path.length(attack.eigen.body.wt.cd.gc)
    attack.eigen.body.ht.fd[i] <- 
      average.path.length(attack.eigen.body.ht.fd.gc)
    attack.eigen.body.ht.cd[i] <- 
      average.path.length(attack.eigen.body.ht.cd.gc)
}

attack.head.df <- 
  data.frame(remove = rep(1:20, 4),
             degree = c((attack.degree.head.wt.fd - average.path.length(head.wt.fd.gc)) / 
                          average.path.length(head.wt.fd.gc) * 100, 
                        (attack.degree.head.wt.cd - average.path.length(head.wt.cd.gc)) / 
                          average.path.length(head.wt.cd.gc) * 100,
                        (attack.degree.head.ht.fd - average.path.length(head.ht.fd.gc)) / 
                          average.path.length(head.ht.fd.gc) * 100,
                        (attack.degree.head.ht.cd - average.path.length(head.ht.cd.gc)) / 
                          average.path.length(head.ht.cd.gc) * 100),
              eigen = c((attack.eigen.head.wt.fd - average.path.length(head.wt.fd.gc)) / 
                          average.path.length(head.wt.fd.gc) * 100, 
                        (attack.eigen.head.wt.cd - average.path.length(head.wt.cd.gc)) / 
                          average.path.length(head.wt.cd.gc) * 100,
                        (attack.eigen.head.ht.fd - average.path.length(head.ht.fd.gc)) / 
                          average.path.length(head.ht.fd.gc) * 100,
                        (attack.eigen.head.ht.cd - average.path.length(head.ht.cd.gc)) / 
                          average.path.length(head.ht.cd.gc) * 100),
              group = rep(c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"), 
                          each = 20))

attack.body.df <- 
  data.frame(remove = rep(1:30, 4), 
             degree = c((attack.degree.body.wt.fd - average.path.length(body.wt.fd.gc)) / 
                          average.path.length(body.wt.fd.gc) * 100, 
                        (attack.degree.body.wt.cd - average.path.length(body.wt.cd.gc)) / 
                          average.path.length(body.wt.cd.gc) * 100,
                        (attack.degree.body.ht.fd - average.path.length(body.ht.fd.gc)) / 
                          average.path.length(body.ht.fd.gc) * 100,
                        (attack.degree.body.ht.cd - average.path.length(body.ht.cd.gc)) / 
                          average.path.length(body.ht.cd.gc) * 100),
              eigen = c((attack.eigen.body.wt.fd - average.path.length(body.wt.fd.gc)) / 
                          average.path.length(body.wt.fd.gc) * 100, 
                        (attack.eigen.body.wt.cd - average.path.length(body.wt.cd.gc)) / 
                          average.path.length(body.wt.cd.gc) * 100,
                        (attack.eigen.body.ht.fd - average.path.length(body.ht.fd.gc)) / 
                          average.path.length(body.ht.fd.gc) * 100,
                        (attack.eigen.body.ht.cd - average.path.length(body.ht.cd.gc)) / 
                          average.path.length(body.ht.cd.gc) * 100),
              group = rep(c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"), 
                          each = 30))

x <- matrix(unlist(strsplit(attack.head.df$group, "/")), ncol = 2, byrow = T)
y <- matrix(unlist(strsplit(attack.body.df$group, "/")), ncol = 2, byrow = T)

attack.head.df$genotype <- factor(x[ ,1], levels = c("w1118", "5-HT2A"))
attack.body.df$genotype <- factor(y[ ,1], levels = c("w1118", "5-HT2A"))

attack.head.df$diet <- factor(x[ ,2], levels = c("FD", "CD"))
attack.body.df$diet <- factor(y[ ,2], levels = c("FD", "CD"))

attack.head.df$group <- factor(attack.head.df$group,
                               levels = c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"))
attack.body.df$group <- factor(attack.body.df$group,
                               levels = c("w1118/FD", "w1118/CD", "5-HT2A/FD", "5-HT2A/CD"))

p3 <- ggplot(attack.head.df, 
             aes(x = remove, y = degree, group = group, 
                 shape = diet, linetype = diet, color = genotype)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 50)) + 
  labs(title = "Network Attack by Degrees in Head", 
       x = "# of Nodes Removed", 
       y = expression(paste(Delta, " Avreage Shortest Distance (%)"))) + 
  theme_classic() + 
  scale_color_manual(values = c('#555555', '#5399FF'))

p4 <- ggplot(attack.body.df, 
             aes(x = remove, y = degree, group = group, 
                 shape = diet, linetype = diet, color = genotype)) + 
  geom_line() +
  geom_point() +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 50)) +
  labs(title = "Network Attack by Degrees in Body", 
       x = "# of Nodes Removed", 
       y = expression(paste(Delta, " Average Shortest Distance (%)"))) +
  theme_classic() +
  scale_color_manual(values = c('#555555', '#5399FF'))

p5 <- ggplot(attack.head.df, 
             aes(x = remove, y = eigen, group = group, 
                 shape = diet, linetype = diet, color = genotype)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(xlim = c(0, 20), ylim = c(0, 50)) + 
  labs(title = "Network Attack by Eigenvalues in Head", 
       x = "# of Nodes Removed", 
       y = expression(paste(Delta, " Average Shortest Distance (%)"))) + 
  theme_classic() + 
  scale_color_manual(values = c('#555555', '#5399FF'))

p6 <- ggplot(attack.body.df, 
             aes(x = remove, y = eigen, group = group, 
                 shape = diet, linetype = diet, color = genotype)) + 
  geom_line() + 
  geom_point() + 
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 50)) + 
  labs(title = "Network Attack by Eigenvalues in Body",  
       x = "# of Nodes Removed", 
       y = expression(paste(Delta, " Average Shortest Distance (%)"))) + 
  theme_classic() + 
  scale_color_manual(values = c('#555555', '#5399FF'))

head.wt.cd.degree.names[1]  ### 5-Aminopentanoic acid
head.wt.cd.degree.names[14] ### GMP
head.wt.cd.degree.names[15] ### Isovalerate
head.wt.cd.degree.names[16] ### Choline

head.wt.cd.eigen.names[4]   ### 5-Aminopentanoic acid
head.wt.cd.eigen.names[9]   ### Carnitine
head.wt.cd.eigen.names[15]  ### Choline
head.wt.cd.eigen.names[16]  ### Glutarate

multiplot(p1, p2, p3, p4, p5, p6, 
          layout = matrix(c(1, 2, 3, 4, 5, 6), 
                          nrow = 3, byrow=TRUE))
