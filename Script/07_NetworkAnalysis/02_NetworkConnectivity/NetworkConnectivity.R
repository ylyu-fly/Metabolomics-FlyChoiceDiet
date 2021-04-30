## Subject: Network Analysis - Network Connectivity
## Project: Fly Choice Diet
## Part: 7-2
## Author: Yang Lyu
## Date created: 10/01/2019
## Date modified: 04/18/2021

# Environment Settings ----------------------------------------------------
library(igraph)
library(Hmisc)
library(ggplot2)
source("Script/07_NetworkAnalysis/01_NetworkConstruction/rcorr.adjust.R")
source("Script/07_NetworkAnalysis/01_NetworkConstruction/multiplot.R")

# Data Input --------------------------------------------------------------
## Read data
load("Data/ProcessedData/NetworkObject-FlyChoiceDiet.Rdata")
rawdata.head <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Head.RDS")
rawdata.body <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Body.RDS")
data.head <- t(rawdata.head)
data.body <- t(rawdata.body)

# Edge Number Analysis ----------------------------------------------------
head.wt.fd.edge    <- ecount(head.wt.fd.g)
head.wt.cd.edge    <- ecount(head.wt.cd.g)
head.ht.fd.edge    <- ecount(head.ht.fd.g)
head.ht.cd.edge    <- ecount(head.ht.cd.g)
head.wt.fd2cd.edge <- ecount(head.wt.fd2cd.g)
head.wt.cd2fd.edge <- ecount(head.wt.cd2fd.g)
head.ht.fd2cd.edge <- ecount(head.ht.fd2cd.g)
head.ht.cd2fd.edge <- ecount(head.ht.cd2fd.g)

body.wt.fd.edge    <- ecount(body.wt.fd.g)
body.wt.cd.edge    <- ecount(body.wt.cd.g)
body.ht.fd.edge    <- ecount(body.ht.fd.g)
body.ht.cd.edge    <- ecount(body.ht.cd.g)
body.wt.fd2cd.edge <- ecount(body.wt.fd2cd.g)
body.wt.cd2fd.edge <- ecount(body.wt.cd2fd.g)
body.ht.fd2cd.edge <- ecount(body.ht.fd2cd.g)
body.ht.cd2fd.edge <- ecount(body.ht.cd2fd.g)

head.wt.edge.delta <- head.wt.cd.edge - head.wt.fd.edge
head.ht.edge.delta <- head.ht.cd.edge - head.ht.fd.edge
body.wt.edge.delta <- body.wt.cd.edge - body.wt.fd.edge
body.ht.edge.delta <- body.ht.cd.edge - body.ht.fd.edge

head.fd.edge.delta <- head.ht.fd.edge - head.wt.fd.edge
head.cd.edge.delta <- head.ht.cd.edge - head.wt.cd.edge
body.fd.edge.delta <- body.ht.fd.edge - body.wt.fd.edge
body.cd.edge.delta <- body.ht.cd.edge - body.wt.cd.edge

## Permutation
perm.head.wt.fd.edge.n    <- numeric()
perm.head.wt.cd.edge.n    <- numeric()
perm.head.ht.fd.edge.n    <- numeric()
perm.head.ht.cd.edge.n    <- numeric()
perm.head.wt.fd2cd.edge.n <- numeric()
perm.head.wt.cd2fd.edge.n <- numeric()
perm.head.ht.fd2cd.edge.n <- numeric()
perm.head.ht.cd2fd.edge.n <- numeric()

perm.body.wt.fd.edge.n    <- numeric()
perm.body.wt.cd.edge.n    <- numeric()
perm.body.ht.fd.edge.n    <- numeric()
perm.body.ht.cd.edge.n    <- numeric()
perm.body.wt.fd2cd.edge.n <- numeric()
perm.body.wt.cd2fd.edge.n <- numeric()
perm.body.ht.fd2cd.edge.n <- numeric()
perm.body.ht.cd2fd.edge.n <- numeric()

perm.head.wt.edge.delta <- numeric()
perm.head.ht.edge.delta <- numeric()
perm.body.wt.edge.delta <- numeric()
perm.body.ht.edge.delta <- numeric()

perm.head.fd.edge.delta <- numeric()
perm.head.cd.edge.delta <- numeric()
perm.body.fd.edge.delta <- numeric()
perm.body.cd.edge.delta <- numeric()

for(i in 1:10000) {
  perm.head <- sample(1:nrow(data.head))
  perm.body <- sample(1:nrow(data.body))
  
  perm.head.labels <- rownames(data.head)[perm.head]
  perm.body.labels <- rownames(data.body)[perm.body]
  
  perm.head.wt.fd     <- data.head[grep("w1118_FD_Head",     perm.head.labels), ]
  perm.head.wt.cd     <- data.head[grep("w1118_CD_Head",     perm.head.labels), ]
  perm.head.ht.fd     <- data.head[grep("5-HT2A_FD_Head",    perm.head.labels), ]
  perm.head.ht.cd     <- data.head[grep("5-HT2A_CD_Head",    perm.head.labels), ]
  perm.head.wt.fd2cd  <- data.head[grep("w1118_FD2CD_Head",  perm.head.labels), ]
  perm.head.wt.cd2fd  <- data.head[grep("w1118_CD2FD_Head",  perm.head.labels), ]
  perm.head.ht.fd2cd  <- data.head[grep("5-HT2A_FD2CD_Head", perm.head.labels), ]
  perm.head.ht.cd2fd  <- data.head[grep("5-HT2A_CD2FD_Head", perm.head.labels), ]
  
  perm.body.wt.fd     <- data.body[grep("w1118_FD_Body",     perm.body.labels), ]
  perm.body.wt.cd     <- data.body[grep("w1118_CD_Body",     perm.body.labels), ]
  perm.body.ht.fd     <- data.body[grep("5-HT2A_FD_Body",    perm.body.labels), ]
  perm.body.ht.cd     <- data.body[grep("5-HT2A_CD_Body",    perm.body.labels), ]
  perm.body.wt.fd2cd  <- data.body[grep("w1118_FD2CD_Body",  perm.body.labels), ]
  perm.body.wt.cd2fd  <- data.body[grep("w1118_CD2FD_Body",  perm.body.labels), ]
  perm.body.ht.fd2cd  <- data.body[grep("5-HT2A_FD2CD_Body", perm.body.labels), ]
  perm.body.ht.cd2fd  <- data.body[grep("5-HT2A_CD2FD_Body", perm.body.labels), ]
  
  perm.head.wt.fd.test <- rcorr.adjust(perm.head.wt.fd, type = "spearman")
  perm.head.wt.cd.test <- rcorr.adjust(perm.head.wt.cd, type = "spearman")
  perm.head.ht.fd.test <- rcorr.adjust(perm.head.ht.fd, type = "spearman")
  perm.head.ht.cd.test <- rcorr.adjust(perm.head.ht.cd, type = "spearman")
  perm.head.wt.fd2cd.test <- rcorr.adjust(perm.head.wt.fd2cd, type = "spearman")
  perm.head.wt.cd2fd.test <- rcorr.adjust(perm.head.wt.cd2fd, type = "spearman")
  perm.head.ht.fd2cd.test <- rcorr.adjust(perm.head.ht.fd2cd, type = "spearman")
  perm.head.ht.cd2fd.test <- rcorr.adjust(perm.head.ht.cd2fd, type = "spearman")
  
  perm.body.wt.fd.test <- rcorr.adjust(perm.body.wt.fd, type = "spearman")
  perm.body.wt.cd.test <- rcorr.adjust(perm.body.wt.cd, type = "spearman")
  perm.body.ht.fd.test <- rcorr.adjust(perm.body.ht.fd, type = "spearman")
  perm.body.ht.cd.test <- rcorr.adjust(perm.body.ht.cd, type = "spearman")
  perm.body.wt.fd2cd.test <- rcorr.adjust(perm.body.wt.fd2cd, type = "spearman")
  perm.body.wt.cd2fd.test <- rcorr.adjust(perm.body.wt.cd2fd, type = "spearman")
  perm.body.ht.fd2cd.test <- rcorr.adjust(perm.body.ht.fd2cd, type = "spearman")
  perm.body.ht.cd2fd.test <- rcorr.adjust(perm.body.ht.cd2fd, type = "spearman")
  
  perm.head.wt.fd.padj <- perm.head.wt.fd.test$P
  perm.head.wt.cd.padj <- perm.head.wt.cd.test$P
  perm.head.ht.fd.padj <- perm.head.ht.fd.test$P
  perm.head.ht.cd.padj <- perm.head.ht.cd.test$P
  perm.head.wt.fd2cd.padj <- perm.head.wt.fd2cd.test$P
  perm.head.wt.cd2fd.padj <- perm.head.wt.cd2fd.test$P
  perm.head.ht.fd2cd.padj <- perm.head.ht.fd2cd.test$P
  perm.head.ht.cd2fd.padj <- perm.head.ht.cd2fd.test$P
  
  perm.body.wt.fd.padj <- perm.body.wt.fd.test$P
  perm.body.wt.cd.padj <- perm.body.wt.cd.test$P
  perm.body.ht.fd.padj <- perm.body.ht.fd.test$P
  perm.body.ht.cd.padj <- perm.body.ht.cd.test$P
  perm.body.wt.fd2cd.padj <- perm.body.wt.fd2cd.test$P
  perm.body.wt.cd2fd.padj <- perm.body.wt.cd2fd.test$P
  perm.body.ht.fd2cd.padj <- perm.body.ht.fd2cd.test$P
  perm.body.ht.cd2fd.padj <- perm.body.ht.cd2fd.test$P
  
  perm.head.wt.fd.edge.n[i]    <- (nrow(which(perm.head.wt.fd.padj <= 0.1, arr.ind = T)) 
                                   - dim(perm.head.wt.fd.padj)[1] ) / 2
  
  perm.head.wt.cd.edge.n[i]    <- (nrow(which(perm.head.wt.cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.wt.cd.padj)[1] ) / 2
  
  perm.head.ht.fd.edge.n[i]    <- (nrow(which(perm.head.ht.fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.ht.fd.padj)[1] ) / 2
  
  perm.head.ht.cd.edge.n[i]    <- (nrow(which(perm.head.ht.cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.ht.cd.padj)[1] ) / 2
  
  perm.head.wt.fd2cd.edge.n[i] <- (nrow(which(perm.head.wt.fd2cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.wt.fd2cd.padj)[1] ) / 2
  
  perm.head.wt.cd2fd.edge.n[i] <- (nrow(which(perm.head.wt.cd2fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.wt.cd2fd.padj)[1] ) / 2
  
  perm.head.ht.fd2cd.edge.n[i] <- (nrow(which(perm.head.ht.fd2cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.ht.fd2cd.padj)[1] ) / 2
  
  perm.head.ht.cd2fd.edge.n[i] <- (nrow(which(perm.head.ht.cd2fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.head.ht.cd2fd.padj)[1] ) / 2
  
  perm.body.wt.fd.edge.n[i]    <- (nrow(which(perm.body.wt.fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.wt.fd.padj)[1] ) / 2
  
  perm.body.wt.cd.edge.n[i]    <- (nrow(which(perm.body.wt.cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.wt.cd.padj)[1] ) / 2
                                   
  perm.body.ht.fd.edge.n[i]    <- (nrow(which(perm.body.ht.fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.ht.fd.padj)[1] ) / 2
                                   
  perm.body.ht.cd.edge.n[i]    <- (nrow(which(perm.body.ht.cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.ht.cd.padj)[1] ) / 2
                                   
  perm.body.wt.fd2cd.edge.n[i] <- (nrow(which(perm.body.wt.fd2cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.wt.fd2cd.padj)[1] ) / 2
                                   
  perm.body.wt.cd2fd.edge.n[i] <- (nrow(which(perm.body.wt.cd2fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.wt.cd2fd.padj)[1] ) / 2
                                   
  perm.body.ht.fd2cd.edge.n[i] <- (nrow(which(perm.body.ht.fd2cd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.ht.fd2cd.padj)[1] ) / 2
                                   
  perm.body.ht.cd2fd.edge.n[i] <- (nrow(which(perm.body.ht.cd2fd.padj <= 0.1, arr.ind = T))
                                   - dim(perm.body.ht.cd2fd.padj)[1] ) / 2
  
  perm.head.wt.edge.delta[i] <- perm.head.wt.cd.edge.n[i] - perm.head.wt.fd.edge.n[i]
  perm.head.ht.edge.delta[i] <- perm.head.ht.cd.edge.n[i] - perm.head.ht.fd.edge.n[i]
  perm.body.wt.edge.delta[i] <- perm.body.wt.cd.edge.n[i] - perm.body.wt.fd.edge.n[i]
  perm.body.ht.edge.delta[i] <- perm.body.ht.cd.edge.n[i] - perm.body.ht.fd.edge.n[i]
  
  perm.head.fd.edge.delta[i] <- perm.head.ht.fd.edge.n[i] - perm.head.wt.fd.edge.n[i]
  perm.head.cd.edge.delta[i] <- perm.head.ht.cd.edge.n[i] - perm.head.wt.cd.edge.n[i]
  perm.body.fd.edge.delta[i] <- perm.body.ht.fd.edge.n[i] - perm.body.wt.fd.edge.n[i]
  perm.body.cd.edge.delta[i] <- perm.body.ht.cd.edge.n[i] - perm.body.wt.cd.edge.n[i]
}

### Find percentile
mean(head.wt.fd.edge >= perm.head.wt.fd.edge.n)
mean(head.wt.cd.edge >= perm.head.wt.cd.edge.n)

mean(body.wt.fd.edge >= perm.body.wt.fd.edge.n)
mean(body.wt.cd.edge >= perm.body.wt.cd.edge.n)

par(mfrow=c(2, 1), mar = c(6, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.5)
plot(jitter(rep(1:6, each = 10000)), 
     c(perm.head.wt.fd.edge.n, perm.head.wt.cd.edge.n,
       perm.head.wt.fd2cd.edge.n, perm.head.wt.cd2fd.edge.n,
       perm.head.ht.fd.edge.n, perm.head.ht.cd.edge.n),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 8), ylim = c(0, 2000), las = 1, axes = F, xaxs="i", yaxs="i",
     xlab = "", ylab = "Number of Edges", main = "Heads")

axis(1, at = seq(1, 6, by = 1), las = 2,
     labels = c("w1118/FD", "w1118/CD", "w1118/FD2CD", "w1118/CD2FD", "5-HT2A/FD", "5-HT2A/CD"))
axis(2, at = seq(0, 2000, by = 500), las = 1)

segments(0.7, median(perm.head.wt.fd.edge.n), 
         1.3, median(perm.head.wt.fd.edge.n), 
         col = "black", lwd = 4)

segments(0.7, head.wt.fd.edge, 
         1.3, head.wt.fd.edge, 
         col = "red", lwd = 4)

segments(1.7, median(perm.head.wt.cd.edge.n), 
         2.3, median(perm.head.wt.cd.edge.n), 
         col = "black", lwd = 4)

segments(1.7, head.wt.cd.edge, 
         2.3, head.wt.cd.edge, 
         col = "red", lwd = 4)

segments(2.7, median(perm.head.wt.fd2cd.edge.n), 
         3.3, median(perm.head.wt.fd2cd.edge.n), 
         col = "black", lwd = 4)

segments(2.7, head.wt.fd2cd.edge, 
         3.3, head.wt.fd2cd.edge, 
         col = "red", lwd = 4)

segments(3.7, median(perm.head.wt.cd2fd.edge.n), 
         4.3, median(perm.head.wt.cd2fd.edge.n), 
         col = "black", lwd = 4)

segments(3.7, head.wt.cd2fd.edge, 
         4.3, head.wt.cd2fd.edge, 
         col = "red", lwd = 4)

segments(4.7, median(perm.head.ht.fd.edge.n), 
         5.3, median(perm.head.ht.fd.edge.n), 
         col = "black", lwd = 4)

segments(4.7, head.ht.fd.edge, 
         5.3, head.ht.fd.edge, 
         col = "red", lwd = 4)

segments(5.7, median(perm.head.ht.cd.edge.n), 
         6.3, median(perm.head.ht.cd.edge.n), 
         col = "black", lwd = 4)

segments(5.7, head.ht.cd.edge, 
         6.3, head.ht.cd.edge, 
         col = "red", lwd = 4)

legend("topleft", lwd = c(4, 4), col = c("black", "red"), 
       legend = c("Median (Permutation)", "Observed"), bty = "n", cex = 0.8)

plot(jitter(rep(1:6, each = 10000)), 
     c(perm.body.wt.fd.edge.n, perm.body.wt.cd.edge.n,
       perm.body.wt.fd2cd.edge.n, perm.body.wt.cd2fd.edge.n,
       perm.body.ht.fd.edge.n, perm.body.ht.cd.edge.n),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 8), ylim = c(0, 4000), las = 1, axes = F, xaxs="i", yaxs="i",
     xlab = "", ylab = "Number of Edges", main = "Bodies")
axis(1, at = seq(1, 6, by = 1), las = 2,
     labels = c("w1118/FD", "w1118/CD", 
                "w1118/FD2CD", "w1118/CD2FD", 
                "5-HT2A/FD", "5-HT2A/CD"))
axis(2, at = seq(0, 4000, by = 1000), las = 1)

segments(0.7, median(perm.body.wt.fd.edge.n), 
         1.3, median(perm.body.wt.fd.edge.n), 
         col = "black", lwd = 4)

segments(0.7, body.wt.fd.edge, 
         1.3, body.wt.fd.edge, 
         col = "red", lwd = 4)

segments(1.7, median(perm.body.wt.cd.edge.n), 
         2.3, median(perm.body.wt.cd.edge.n), 
         col = "black", lwd = 4)

segments(1.7, body.wt.cd.edge, 
         2.3, body.wt.cd.edge, 
         col = "red", lwd = 4)

segments(2.7, median(perm.body.wt.fd2cd.edge.n), 
         3.3, median(perm.body.wt.fd2cd.edge.n), 
         col = "black", lwd = 4)

segments(2.7, body.wt.fd2cd.edge, 
         3.3, body.wt.fd2cd.edge, 
         col = "red", lwd = 4)

segments(3.7, median(perm.body.wt.cd2fd.edge.n), 
         4.3, median(perm.body.wt.cd2fd.edge.n), 
         col = "black", lwd = 4)

segments(3.7, body.wt.cd2fd.edge, 
         4.3, body.wt.cd2fd.edge, 
         col = "red", lwd = 4)

segments(4.7, median(perm.body.ht.fd.edge.n), 
         5.3, median(perm.body.ht.fd.edge.n), 
         col = "black", lwd = 4)

segments(4.7, body.ht.fd.edge, 
         5.3, body.ht.fd.edge, 
         col = "red", lwd = 4)

segments(5.7, median(perm.body.ht.cd.edge.n), 
         6.3, median(perm.body.ht.cd.edge.n), 
         col = "black", lwd = 4)

segments(5.7, body.ht.cd.edge, 
         6.3, body.ht.cd.edge, 
         col = "red", lwd = 4)

legend("topleft", lwd = c(4, 4), col = c("black", "red"), 
       legend = c("Median (Permutation)", "Observed"), bty = "n", cex = 0.8)

### Plot dietary effects
par(mfrow=c(1, 2), mar = c(6, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.5)
plot(jitter(rep(1:2, each = 10000)), 
     c(perm.head.wt.edge.delta, perm.head.ht.edge.delta),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 2.8), ylim = c(-1500, 1500), 
     las = 1, axes = F, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "Change of edge number by dietary choice", main = "Heads")

axis(1, at = seq(1, 2, by = 1), las = 2, labels = c("w1118", "5-HT2A"))
axis(2, las = 1)

segments(0.7, median(perm.head.wt.edge.delta), 
         1.3, median(perm.head.wt.edge.delta), 
         col = "black", lwd = 4)

segments(0.7, head.wt.edge.delta, 
         1.3, head.wt.edge.delta, 
         col = "red", lwd = 4)

segments(1.7, median(perm.head.ht.edge.delta), 
         2.3, median(perm.head.ht.edge.delta), 
         col = "black", lwd = 4)

segments(1.7, head.ht.edge.delta, 
         2.3, head.ht.edge.delta, 
         col = "red", lwd = 4)

1-sum(head.wt.edge.delta <= perm.head.wt.edge.delta)/10000
1-sum(head.ht.edge.delta <= perm.head.ht.edge.delta)/10000

text (1, 1400, "P=0.0011", col = "red")
text (2, 1400, "P=0.1547")

plot(jitter(rep(1:2, each = 10000)), 
     c(perm.body.wt.edge.delta, perm.body.ht.edge.delta),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 2.8), ylim = c(-3000, 3000), 
     las = 1, axes = F, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "Change of edge number by dietary choice", main = "Bodies")

axis(1, at = seq(1, 2, by = 1), las = 2,
     labels = c("w1118", "5-HT2A"))
axis(2, las = 1)

segments(0.7, median(perm.body.wt.edge.delta), 
         1.3, median(perm.body.wt.edge.delta), 
         col = "black", lwd = 4)

segments(0.7, body.wt.edge.delta, 
         1.3, body.wt.edge.delta, 
         col = "red", lwd = 4)

segments(1.7, median(perm.body.ht.edge.delta), 
         2.3, median(perm.body.ht.edge.delta), 
         col = "black", lwd = 4)

segments(1.7, body.ht.edge.delta, 
         2.3, body.ht.edge.delta, 
         col = "red", lwd = 4)

1-sum(body.wt.edge.delta <= perm.body.wt.edge.delta)/10000
1-sum(body.ht.edge.delta <= perm.body.ht.edge.delta)/10000

text (1, 2700, "P=0.1025")
text (2, 2700, "P=0.2758")

## Plot genotype effects
par(mfrow=c(1, 2), mar = c(4, 4, 4, 0) + 0.5, mgp = c(2, 0, 0) + 0.5)
plot(jitter(rep(1:2, each = 10000)), 
     c(perm.head.fd.edge.delta, perm.head.cd.edge.delta),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 2.8), ylim = c(-2000, 2000), 
     las = 1, axes = F, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "Change of edge number\nby 5-HT2A", main = "Heads")

axis(1, at = seq(1, 2, by = 1), las = 2, labels = c("FD", "CD"))
axis(2, las = 1)

segments(0.7, median(perm.head.fd.edge.delta), 
         1.3, median(perm.head.fd.edge.delta), 
         col = "black", lwd = 4)

segments(0.7, head.fd.edge.delta, 
         1.3, head.fd.edge.delta, 
         col = "red", lwd = 4)

segments(1.7, median(perm.head.cd.edge.delta), 
         2.3, median(perm.head.cd.edge.delta), 
         col = "black", lwd = 4)

segments(1.7, head.cd.edge.delta, 
         2.3, head.cd.edge.delta, 
         col = "red", lwd = 4)

1-sum(head.fd.edge.delta >= perm.head.fd.edge.delta)/10000
1-sum(head.cd.edge.delta >= perm.head.cd.edge.delta)/10000

text (1, 1600, "P=0.30")
text (2, 1600, "P=0.0022", col = "red")

plot(jitter(rep(1:2, each = 10000)), 
     c(perm.body.fd.edge.delta, perm.body.cd.edge.delta),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 2.8), ylim = c(-3000, 3000), 
     las = 1, axes = F, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "Change of edge number\nby 5-HT2A", main = "Bodies")

axis(1, at = seq(1, 2, by = 1), las = 2, labels = c("FD", "CD"))
axis(2, las = 1)

segments(0.7, median(perm.body.fd.edge.delta), 
         1.3, median(perm.body.fd.edge.delta), 
         col = "black", lwd = 4)

segments(0.7, body.fd.edge.delta, 
         1.3, body.fd.edge.delta, 
         col = "red", lwd = 4)

segments(1.7, median(perm.body.cd.edge.delta), 
         2.3, median(perm.body.cd.edge.delta), 
         col = "black", lwd = 4)

segments(1.7, body.cd.edge.delta, 
         2.3, body.cd.edge.delta, 
         col = "red", lwd = 4)

1-sum(body.fd.edge.delta >= perm.body.fd.edge.delta)/10000
1-sum(body.cd.edge.delta >= perm.body.cd.edge.delta)/10000

text (1, 2400, "P=0.40")
text (2, 2400, "P=0.22")

# Connectivity Analysis ---------------------------------------------------
head.wt.fd.gc.degree <- as.numeric(names(table(degree(head.wt.fd.gc))))
head.wt.cd.gc.degree <- as.numeric(names(table(degree(head.wt.cd.gc))))
head.wt.fd2cd.gc.degree <- as.numeric(names(table(degree(head.wt.fd2cd.gc))))
head.wt.cd2fd.gc.degree <- as.numeric(names(table(degree(head.wt.cd2fd.gc))))
head.ht.fd.gc.degree <- as.numeric(names(table(degree(head.ht.fd.gc))))
head.ht.cd.gc.degree <- as.numeric(names(table(degree(head.ht.cd.gc))))

body.wt.fd.gc.degree <- as.numeric(names(table(degree(body.wt.fd.gc))))
body.wt.cd.gc.degree <- as.numeric(names(table(degree(body.wt.cd.gc))))
body.wt.fd2cd.gc.degree <- as.numeric(names(table(degree(body.wt.fd2cd.gc))))
body.wt.cd2fd.gc.degree <- as.numeric(names(table(degree(body.wt.cd2fd.gc))))
body.ht.fd.gc.degree <- as.numeric(names(table(degree(body.ht.fd.gc))))
body.ht.cd.gc.degree <- as.numeric(names(table(degree(body.ht.cd.gc))))

head.wt.fd.gc.freq <- as.numeric(table(degree(head.wt.fd.gc)) /sum(table(degree(head.wt.fd.gc))))
head.wt.cd.gc.freq <- as.numeric(table(degree(head.wt.cd.gc)) /sum(table(degree(head.wt.cd.gc))))
head.wt.fd2cd.gc.freq <- as.numeric(table(degree(head.wt.fd2cd.gc)) /sum(table(degree(head.wt.fd2cd.gc))))
head.wt.cd2fd.gc.freq <- as.numeric(table(degree(head.wt.cd2fd.gc)) /sum(table(degree(head.wt.cd2fd.gc))))
head.ht.fd.gc.freq <- as.numeric(table(degree(head.ht.fd.gc)) /sum(table(degree(head.ht.fd.gc))))
head.ht.cd.gc.freq <- as.numeric(table(degree(head.ht.cd.gc)) /sum(table(degree(head.ht.cd.gc))))

body.wt.fd.gc.freq <- as.numeric(table(degree(body.wt.fd.gc)) /sum(table(degree(body.wt.fd.gc))))
body.wt.cd.gc.freq <- as.numeric(table(degree(body.wt.cd.gc)) /sum(table(degree(body.wt.cd.gc))))
body.wt.fd2cd.gc.freq <- as.numeric(table(degree(body.wt.fd2cd.gc)) /sum(table(degree(body.wt.fd2cd.gc))))
body.wt.cd2fd.gc.freq <- as.numeric(table(degree(body.wt.cd2fd.gc)) /sum(table(degree(body.wt.cd2fd.gc))))
body.ht.fd.gc.freq <- as.numeric(table(degree(body.ht.fd.gc)) /sum(table(degree(body.ht.fd.gc))))
body.ht.cd.gc.freq <- as.numeric(table(degree(body.ht.cd.gc)) /sum(table(degree(body.ht.cd.gc))))

sum(head.wt.fd.gc.freq[head.wt.fd.gc.degree >= vcount(head.wt.fd.gc) * 0.3])
sum(head.wt.cd.gc.freq[head.wt.cd.gc.degree >= vcount(head.wt.cd.gc) * 0.3])
sum(head.wt.fd2cd.gc.freq[head.wt.fd2cd.gc.degree >= vcount(head.wt.fd2cd.gc) * 0.3])
sum(head.wt.cd2fd.gc.freq[head.wt.cd2fd.gc.degree >= vcount(head.wt.cd2fd.gc) * 0.3])
sum(head.ht.fd.gc.freq[head.ht.fd.gc.degree >= vcount(head.ht.fd.gc) * 0.3])
sum(head.ht.cd.gc.freq[head.ht.cd.gc.degree >= vcount(head.ht.cd.gc) * 0.3])

sum(body.wt.fd.gc.freq[body.wt.fd.gc.degree >= vcount(body.wt.fd.gc) * 0.3])
sum(body.wt.cd.gc.freq[body.wt.cd.gc.degree >= vcount(body.wt.cd.gc) * 0.3])
sum(body.wt.fd2cd.gc.freq[body.wt.fd2cd.gc.degree >= vcount(body.wt.fd2cd.gc) * 0.3])
sum(body.wt.cd2fd.gc.freq[body.wt.cd2fd.gc.degree >= vcount(body.wt.cd2fd.gc) * 0.3])
sum(body.ht.fd.gc.freq[body.ht.fd.gc.degree >= vcount(body.ht.fd.gc) * 0.3])
sum(body.ht.cd.gc.freq[body.ht.cd.gc.degree >= vcount(body.ht.cd.gc) * 0.3])

## Plot heads data
par(mfrow=c(3, 2), mar = c(3, 3, 1, 0) + 0.5, mgp = c(1.5, 0, 0) + 0.5)
plot(log2(head.wt.fd.gc.degree), head.wt.fd.gc.freq, col="grey27",
     xlim = c(0, 6), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Heads / w1118 / FD")

points(log2(head.wt.fd.gc.degree[head.wt.fd.gc.degree >= vcount(head.wt.fd.gc) * 0.3]), 
       head.wt.fd.gc.freq[head.wt.fd.gc.degree >= vcount(head.wt.fd.gc) * 0.3], col="red")

plot(log2(head.wt.cd.gc.degree), head.wt.cd.gc.freq, col="grey27",
     xlim = c(0, 6), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Heads / w1118 / CD")

points(log2(head.wt.cd.gc.degree[head.wt.cd.gc.degree >= vcount(head.wt.cd.gc) * 0.3]), 
       head.wt.cd.gc.freq[head.wt.cd.gc.degree >= vcount(head.wt.cd.gc) * 0.3], col="red")

plot(log2(head.wt.fd2cd.gc.degree), head.wt.fd2cd.gc.freq, col="grey27",
     xlim = c(0, 6), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Heads / w1118 / FD2CD")

points(log2(head.wt.fd2cd.gc.degree[head.wt.fd2cd.gc.degree >= vcount(head.wt.fd2cd.gc) * 0.3]), 
       head.wt.fd2cd.gc.freq[head.wt.fd2cd.gc.degree >= vcount(head.wt.fd2cd.gc) * 0.3], col="red")

plot(log2(head.wt.cd2fd.gc.degree), head.wt.cd2fd.gc.freq, col="grey27",
     xlim = c(0, 6), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Heads / w1118 / CD2FD")

points(log2(head.wt.cd2fd.gc.degree[head.wt.cd2fd.gc.degree >= vcount(head.wt.cd2fd.gc) * 0.3]), 
       head.wt.cd2fd.gc.freq[head.wt.cd2fd.gc.degree >= vcount(head.wt.cd2fd.gc) * 0.3], col="red")

plot(log2(head.ht.fd.gc.degree), head.ht.fd.gc.freq, col="grey27",
     xlim = c(0, 6), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Heads / 5-HT2A / FD")

points(log2(head.ht.fd.gc.degree[head.ht.fd.gc.degree >= vcount(head.ht.fd.gc) * 0.3]), 
       head.ht.fd.gc.freq[head.ht.fd.gc.degree >= vcount(head.ht.fd.gc) * 0.3], col="red")

plot(log2(head.ht.cd.gc.degree), head.ht.cd.gc.freq, col="grey27",
     xlim = c(0, 6), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Heads / 5-HT2A / CD")

points(log2(head.ht.cd.gc.degree[head.ht.cd.gc.degree >= vcount(head.ht.cd.gc) * 0.3]), 
       head.ht.cd.gc.freq[head.ht.cd.gc.degree >= vcount(head.ht.cd.gc) * 0.3], col="red")

## Plot bodies data
par(mfrow=c(3, 2), mar = c(3, 3, 1, 0) + 0.5, mgp = c(1.5, 0, 0) + 0.5)
plot(log2(body.wt.fd.gc.degree), body.wt.fd.gc.freq, col="grey27",
     xlim = c(0, 7), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Bodies / w1118 / FD")

points(log2(body.wt.fd.gc.degree[body.wt.fd.gc.degree >= vcount(body.wt.fd.gc) * 0.3]), 
       body.wt.fd.gc.freq[body.wt.fd.gc.degree >= vcount(body.wt.fd.gc) * 0.3], col="red")

plot(log2(body.wt.cd.gc.degree), body.wt.cd.gc.freq, col="grey27",
     xlim = c(0, 7), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Bodies / w1118 / CD")

points(log2(body.wt.cd.gc.degree[body.wt.cd.gc.degree >= vcount(body.wt.cd.gc) * 0.3]), 
       body.wt.cd.gc.freq[body.wt.cd.gc.degree >= vcount(body.wt.cd.gc) * 0.3], col="red")

plot(log2(body.wt.fd2cd.gc.degree), body.wt.fd2cd.gc.freq, col="grey27",
     xlim = c(0, 7), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Bodies / w1118 / FD2CD")

points(log2(body.wt.fd2cd.gc.degree[body.wt.fd2cd.gc.degree >= vcount(body.wt.fd2cd.gc) * 0.3]), 
       body.wt.fd2cd.gc.freq[body.wt.fd2cd.gc.degree >= vcount(body.wt.fd2cd.gc) * 0.3], col="red")

plot(log2(body.wt.cd2fd.gc.degree), body.wt.cd2fd.gc.freq, col="grey27",
     xlim = c(0, 7), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Bodies / w1118 / CD2FD")

points(log2(body.wt.cd2fd.gc.degree[body.wt.cd2fd.gc.degree >= vcount(body.wt.cd2fd.gc) * 0.3]), 
       body.wt.cd2fd.gc.freq[body.wt.cd2fd.gc.degree >= vcount(body.wt.cd2fd.gc) * 0.3], col="red")

plot(log2(body.ht.fd.gc.degree), body.ht.fd.gc.freq, col="grey27",
     xlim = c(0, 7), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Bodies / 5-HT2A / FD")

points(log2(body.ht.fd.gc.degree[body.ht.fd.gc.degree >= vcount(body.ht.fd.gc) * 0.3]), 
       body.ht.fd.gc.freq[body.ht.fd.gc.degree >= vcount(body.ht.fd.gc) * 0.3], col="red")

plot(log2(body.ht.cd.gc.degree), body.ht.cd.gc.freq, col="grey27",
     xlim = c(0, 7), ylim = c(0, 0.15), las = 1,
     xlab = c("Log2-Degree"), ylab = c("Proportion"), 
     main = "Bodies / 5-HT2A / CD")

points(log2(body.ht.cd.gc.degree[body.ht.cd.gc.degree >= vcount(body.ht.cd.gc) * 0.3]), 
       body.ht.cd.gc.freq[body.ht.cd.gc.degree >= vcount(body.ht.cd.gc) * 0.3], col="red")

# Average of Shortest Path Analysis ---------------------------------------
## Calculate diameter
diameter(head.wt.fd.gc) ### 10
diameter(head.wt.cd.gc) ### 13
diameter(head.wt.fd2cd.gc) ### 8
diameter(head.wt.cd2fd.gc) ### 11
diameter(head.ht.fd.gc) ### 8
diameter(head.ht.cd.gc) ### 7

diameter(body.wt.fd.gc) ### 7
diameter(body.wt.cd.gc) ### 8
diameter(body.wt.fd2cd.gc) ### 9
diameter(body.wt.cd2fd.gc) ### 7
diameter(body.ht.fd.gc) ### 5
diameter(body.ht.cd.gc) ### 7

## Calculate average distance
average.path.length(head.wt.fd.gc) ### 2.88
average.path.length(head.wt.cd.gc) ### 4.37
average.path.length(head.wt.fd2cd.gc) ### 3.00
average.path.length(head.wt.cd2fd.gc) ### 2.85
average.path.length(head.ht.fd.gc) ### 2.33
average.path.length(head.ht.cd.gc) ### 2.26
average.path.length(body.wt.fd.gc) ### 2.25
average.path.length(body.wt.cd.gc) ### 2.42
average.path.length(body.wt.fd2cd.gc) ### 2.55
average.path.length(body.wt.cd2fd.gc) ### 2.08
average.path.length(body.ht.fd.gc) ### 1.86
average.path.length(body.ht.cd.gc) ### 2.18

## Calculate distances (all the shortest paths in the network)
dist.head.core.wt.fd <- distances(head.wt.fd.gc)
dist.head.core.wt.cd <- distances(head.wt.cd.gc)
dist.head.core.wt.fd2cd <- distances(head.wt.fd2cd.gc)
dist.head.core.wt.cd2fd <- distances(head.wt.cd2fd.gc)
dist.head.core.ht.fd <- distances(head.ht.fd.gc)
dist.head.core.ht.cd <- distances(head.ht.cd.gc)
dist.body.core.wt.fd <- distances(body.wt.fd.gc)
dist.body.core.wt.cd <- distances(body.wt.cd.gc)
dist.body.core.wt.fd2cd <- distances(body.wt.fd2cd.gc)
dist.body.core.wt.cd2fd <- distances(body.wt.cd2fd.gc)
dist.body.core.ht.fd <- distances(body.ht.fd.gc)
dist.body.core.ht.cd <- distances(body.ht.cd.gc)

dist.head.core.wt.fd[upper.tri(dist.head.core.wt.fd, diag = T)] <- NA
dist.head.core.wt.cd[upper.tri(dist.head.core.wt.cd, diag = T)] <- NA
dist.head.core.wt.fd2cd[upper.tri(dist.head.core.wt.fd2cd, diag = T)] <- NA
dist.head.core.wt.cd2fd[upper.tri(dist.head.core.wt.cd2fd, diag = T)] <- NA
dist.head.core.ht.fd[upper.tri(dist.head.core.ht.fd, diag = T)] <- NA
dist.head.core.ht.cd[upper.tri(dist.head.core.ht.cd, diag = T)] <- NA
dist.body.core.wt.fd[upper.tri(dist.body.core.wt.fd, diag = T)] <- NA
dist.body.core.wt.cd[upper.tri(dist.body.core.wt.cd, diag = T)] <- NA
dist.body.core.wt.fd2cd[upper.tri(dist.body.core.wt.fd2cd, diag = T)] <- NA
dist.body.core.wt.cd2fd[upper.tri(dist.body.core.wt.cd2fd, diag = T)] <- NA
dist.body.core.ht.fd[upper.tri(dist.body.core.ht.fd, diag = T)] <- NA
dist.body.core.ht.cd[upper.tri(dist.body.core.ht.cd, diag = T)] <- NA

dist.head.core.wt.fd <- as.vector(dist.head.core.wt.fd)
dist.head.core.wt.cd <- as.vector(dist.head.core.wt.cd)
dist.head.core.wt.fd2cd <- as.vector(dist.head.core.wt.fd2cd)
dist.head.core.wt.cd2fd <- as.vector(dist.head.core.wt.cd2fd)
dist.head.core.ht.fd <- as.vector(dist.head.core.ht.fd)
dist.head.core.ht.cd <- as.vector(dist.head.core.ht.cd)
dist.body.core.wt.fd <- as.vector(dist.body.core.wt.fd)
dist.body.core.wt.cd <- as.vector(dist.body.core.wt.cd)
dist.body.core.wt.fd2cd <- as.vector(dist.body.core.wt.fd2cd)
dist.body.core.wt.cd2fd <- as.vector(dist.body.core.wt.cd2fd)
dist.body.core.ht.fd <- as.vector(dist.body.core.ht.fd)
dist.body.core.ht.cd <- as.vector(dist.body.core.ht.cd)

dist.head.core.wt.fd <- dist.head.core.wt.fd[!is.na(dist.head.core.wt.fd)]
dist.head.core.wt.cd <- dist.head.core.wt.cd[!is.na(dist.head.core.wt.cd)]
dist.head.core.wt.fd2cd <- dist.head.core.wt.fd2cd[!is.na(dist.head.core.wt.fd2cd)]
dist.head.core.wt.cd2fd <- dist.head.core.wt.cd2fd[!is.na(dist.head.core.wt.cd2fd)]
dist.head.core.ht.fd <- dist.head.core.ht.fd[!is.na(dist.head.core.ht.fd)]
dist.head.core.ht.cd <- dist.head.core.ht.cd[!is.na(dist.head.core.ht.cd)]
dist.body.core.wt.fd <- dist.body.core.wt.fd[!is.na(dist.body.core.wt.fd)]
dist.body.core.wt.cd <- dist.body.core.wt.cd[!is.na(dist.body.core.wt.cd)]
dist.body.core.wt.fd2cd <- dist.body.core.wt.fd2cd[!is.na(dist.body.core.wt.fd2cd)]
dist.body.core.wt.cd2fd <- dist.body.core.wt.cd2fd[!is.na(dist.body.core.wt.cd2fd)]
dist.body.core.ht.fd <- dist.body.core.ht.fd[!is.na(dist.body.core.ht.fd)]
dist.body.core.ht.cd <- dist.body.core.ht.cd[!is.na(dist.body.core.ht.cd)]

## Calculate and plot the average distances in networks
dist.head.core.wt.fd <- dist.head.core.wt.fd[is.finite(dist.head.core.wt.fd)]
dist.head.core.wt.cd <- dist.head.core.wt.cd[is.finite(dist.head.core.wt.cd)]
dist.head.core.wt.fd2cd <- dist.head.core.wt.fd2cd[is.finite(dist.head.core.wt.fd2cd)]
dist.head.core.wt.cd2fd <- dist.head.core.wt.cd2fd[is.finite(dist.head.core.wt.cd2fd)]
dist.head.core.ht.fd <- dist.head.core.ht.fd[is.finite(dist.head.core.ht.fd)]
dist.head.core.ht.cd <- dist.head.core.ht.cd[is.finite(dist.head.core.ht.cd)]
dist.body.core.wt.fd <- dist.body.core.wt.fd[is.finite(dist.body.core.wt.fd)] 
dist.body.core.wt.cd <- dist.body.core.wt.cd[is.finite(dist.body.core.wt.cd)]
dist.body.core.wt.fd2cd <- dist.body.core.wt.fd2cd[is.finite(dist.body.core.wt.fd2cd)] 
dist.body.core.wt.cd2fd <- dist.body.core.wt.cd2fd[is.finite(dist.body.core.wt.cd2fd)]
dist.body.core.ht.fd <- dist.body.core.ht.fd[is.finite(dist.body.core.ht.fd)] 
dist.body.core.ht.cd <- dist.body.core.ht.cd[is.finite(dist.body.core.ht.cd)]

df.core.distance <- c(dist.head.core.wt.fd, dist.head.core.wt.cd, 
                      dist.head.core.wt.fd2cd, dist.head.core.wt.cd2fd, 
                      dist.head.core.ht.fd, dist.head.core.ht.cd,
                      dist.body.core.wt.fd, dist.body.core.wt.cd, 
                      dist.body.core.wt.fd2cd, dist.body.core.wt.cd2fd, 
                      dist.body.core.ht.fd, dist.body.core.ht.cd)

df.core.group <- rep(c("Head/w1118/FD", "Head/w1118/CD", "Head/w1118/FD2CD", "Head/w1118/CD2FD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                       "Body/w1118/FD", "Body/w1118/CD", "Body/w1118/FD2CD", "Body/w1118/CD2FD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"),
                     c(length(dist.head.core.wt.fd), length(dist.head.core.wt.cd), 
                       length(dist.head.core.wt.fd2cd), length(dist.head.core.wt.cd2fd),
                       length(dist.head.core.ht.fd), length(dist.head.core.ht.cd),
                       length(dist.body.core.wt.fd), length(dist.body.core.wt.cd), 
                       length(dist.body.core.wt.fd2cd), length(dist.body.core.wt.cd2fd), 
                       length(dist.body.core.ht.fd), length(dist.body.core.ht.cd)))

df.core.group <- factor(df.core.group, 
                        levels = c("Head/w1118/FD", "Head/w1118/CD", "Head/w1118/FD2CD", "Head/w1118/CD2FD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                                   "Body/w1118/FD", "Body/w1118/CD", "Body/w1118/FD2CD", "Body/w1118/CD2FD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"))

xc <- matrix(unlist(strsplit(as.character(df.core.group), "/")), ncol = 3, byrow = T)

df.core <- data.frame(Group = df.core.group, Distance = df.core.distance, 
                      Genotype = xc[, 2], Tissue = xc[, 1], Diet = xc[, 3])

df.core$Distance2 <- log2(df.core$Distance)

df.core.head <- df.core[df.core$Tissue == "Head", ]
df.core.body <- df.core[df.core$Tissue == "Body", ]

dist.p1 <- ggplot(df.core.head, aes(x = Group, y = Distance2)) + 
  geom_violin(trim = FALSE, fill = "gray") + 
  labs(title = "Heads", x = "Group", y = "Log2(Shortest Distance)") +
  stat_summary(fun.data = mean_sdl, 
               geom = "pointrange", color = "red")+
  theme_classic()

dist.p2 <- ggplot(df.core.body, aes(x = Group, y = Distance2)) + 
  geom_violin(trim = FALSE, fill = "gray") + 
  labs(title="Bodies", x = "Group", y = "Log2(Shortest Distance)") +
  stat_summary(fun.data = mean_sdl, 
               geom = "pointrange", color = "red") +
  theme_classic()

multiplot(dist.p2, dist.p1,  
          layout = matrix(c(2, 1), nrow = 1, byrow = T))

## One-way ANOVA
res.aov.head.core <- aov(Distance ~ Diet, data = df.core.head[df.core.head$Genotype == "w1118", ])
res.aov.body.core <- aov(Distance ~ Diet, data = df.core.body[df.core.body$Genotype == "w1118", ])

summary(res.aov.head.core)
summary(res.aov.body.core)

## Two-way ANOVA
res.aov.head.core <- aov(Distance ~ Diet + Genotype + Diet:Genotype, data = df.core.head)
res.aov.body.core <- aov(Distance ~ Diet + Genotype + Diet:Genotype, data = df.core.body)

summary(res.aov.head.core)
summary(res.aov.body.core)