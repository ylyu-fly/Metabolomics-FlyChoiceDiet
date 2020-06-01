## Subject: Network Analysis - Network Connectivity
## Project: Fly Choice Diet
## Part: 7-2
## Author: Yang Lyu
## Date created: 10/01/2019
## Date modified: 05/31/2020

# Environment Settings ----------------------------------------------------
library(igraph)
library(ggplot2)
library(reshape2)
source("Script/07_NetworkAnalysis/02_NetworkConnectivity/multiplot.R")

# Data Input --------------------------------------------------------------
## Read data
load("Data/ProcessedData/NetworkObject-FlyChoiceDiet.Rdata")
rawdata.head <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Head.RDS")
rawdata.body <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Body.RDS")
data.head <- t(rawdata.head)
data.body <- t(rawdata.body)


# Edge Number Analysis ----------------------------------------------------
## Get Edge Number
head.wt.cor.df <- data.frame(Cor = c(head.wt.fd.cor.lwr[lower.tri(head.wt.fd.cor.lwr)],
                                     head.wt.cd.cor.lwr[lower.tri(head.wt.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                        each = length(head.wt.fd.cor.lwr[lower.tri(head.wt.fd.cor.lwr)]))))

head.wt.tr.cor.df <- data.frame(Cor = c(head.wt.fd2cd.cor.lwr[lower.tri(head.wt.fd2cd.cor.lwr)],
                                        head.wt.cd2fd.cor.lwr[lower.tri(head.wt.cd2fd.cor.lwr)]), 
                                Diet = factor(rep(c("FD2CD", "CD2FD"), 
                                               each = length(head.wt.fd2cd.cor.lwr[lower.tri(head.wt.fd2cd.cor.lwr)]))))

head.ht.cor.df <- data.frame(Cor = c(head.ht.fd.cor.lwr[lower.tri(head.ht.fd.cor.lwr)],
                                     head.ht.cd.cor.lwr[lower.tri(head.ht.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(head.ht.fd.cor.lwr[lower.tri(head.ht.fd.cor.lwr)]))))

body.wt.cor.df <- data.frame(Cor = c(body.wt.fd.cor.lwr[lower.tri(body.wt.fd.cor.lwr)],
                                     body.wt.cd.cor.lwr[lower.tri(body.wt.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(body.wt.fd.cor.lwr[lower.tri(body.wt.fd.cor.lwr)]))))

body.wt.tr.cor.df <- data.frame(Cor = c(body.wt.fd2cd.cor.lwr[lower.tri(body.wt.fd2cd.cor.lwr)],
                                        body.wt.cd2fd.cor.lwr[lower.tri(body.wt.cd2fd.cor.lwr)]), 
                                Diet = factor(rep(c("FD2CD", "CD2FD"), 
                                                  each = length(body.wt.fd2cd.cor.lwr[lower.tri(body.wt.fd2cd.cor.lwr)]))))

body.ht.cor.df <- data.frame(Cor = c(body.ht.fd.cor.lwr[lower.tri(body.ht.fd.cor.lwr)],
                                     body.ht.cd.cor.lwr[lower.tri(body.ht.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(body.ht.fd.cor.lwr[lower.tri(body.ht.fd.cor.lwr)]))))

## Plot the distribution of correlation coefficient
cor.p1 <- ggplot(head.wt.cor.df, aes(x = Cor, color = Diet, fill = Diet)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.11) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red", "blue", "white")) +
  labs(title = "Head/w1118",x = "Correlation Coefficient", y = "Count") +
  theme_classic()

cor.p2 <- ggplot(head.wt.tr.cor.df, aes(x = Cor, color = Diet, fill = Diet)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.11) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red", "blue", "white")) +
  labs(title = "Head/w1118", x = "Correlation Coefficient", y = "Count") +
  theme_classic()

cor.p3 <- ggplot(head.ht.cor.df, aes(x = Cor, color = Diet, fill = Diet)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.11) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red", "blue", "white")) +
  labs(title="Head/5-HT2A",x = "Correlation Coefficient", y = "Count") +
  theme_classic()

cor.p4 <- ggplot(body.wt.cor.df, aes(x = Cor, color = Diet, fill = Diet)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.11) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red", "blue", "white")) +
  labs(title="Body/w1118", x = "Correlation Coefficient", y = "Count") +
  theme_classic()

cor.p5 <- ggplot(body.wt.tr.cor.df, aes(x = Cor, color = Diet, fill = Diet)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.11) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red", "blue", "white")) +
  labs(title = "Body/w1118", x = "Correlation Coefficient", y = "Count") +
  theme_classic()

cor.p6 <- ggplot(body.ht.cor.df, aes(x = Cor, color = Diet, fill = Diet)) +
  geom_histogram(position = "identity", alpha = 0.3, binwidth = 0.11) +
  scale_color_manual(values = c("black", "black", "black")) +
  scale_fill_manual(values = c("red", "blue", "white")) +
  labs(title = "Body/5-HT2A", x = "Correlation Coefficient", y = "Count") +
  theme_classic()

multiplot(cor.p1, cor.p4, cor.p2, cor.p5, cor.p3, cor.p6, cols=3)

head.wt.cor.df$Type     <- "Weakly or not correlated"
head.wt.tr.cor.df$Type  <- "Weakly or not correlated"
head.ht.cor.df$Type     <- "Weakly or not correlated"

head.wt.cor.df[abs(head.wt.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"
head.wt.tr.cor.df[abs(head.wt.tr.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"
head.ht.cor.df[abs(head.ht.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"

head.wt.type.df <-merge(melt(table(head.wt.cor.df[head.wt.cor.df$Diet == "FD", ]$Type)),
                        melt(table(head.wt.cor.df[head.wt.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)
head.wt.tr.type.df <-merge(melt(table(head.wt.tr.cor.df[head.wt.tr.cor.df$Diet == "FD2CD", ]$Type)),
                           melt(table(head.wt.tr.cor.df[head.wt.tr.cor.df$Diet == "CD2FD", ]$Type)), by = "Var1", all = T)
head.ht.type.df <-merge(melt(table(head.ht.cor.df[head.ht.cor.df$Diet == "FD", ]$Type)),
                        melt(table(head.ht.cor.df[head.ht.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)

colnames(head.wt.type.df) <- c("Type", "FD", "CD")
colnames(head.wt.tr.type.df) <- c("Type", "FD2CD", "CD2FD")
colnames(head.ht.type.df) <- c("Type", "FD", "CD")

head.wt.type.df <- melt(head.wt.type.df, id = c("Type"))
head.wt.tr.type.df <- melt(head.wt.tr.type.df, id = c("Type"))
head.ht.type.df <- melt(head.ht.type.df, id = c("Type"))

colnames(head.wt.type.df) <- c("Type", "Diet", "Count")
colnames(head.wt.tr.type.df) <- c("Type", "Diet", "Count")
colnames(head.ht.type.df) <- c("Type", "Diet", "Count")

fisher.test(matrix(c(732, 4521, 402, 4851), nrow = 2)) # p < 2.2e-16
fisher.test(matrix(c(514, 4739, 519, 4734), nrow = 2)) # p = 0.8957
fisher.test(matrix(c(802, 4451, 604, 4649), nrow = 2)) # p = 1.577e-08

cor.p7 <- ggplot(data = head.wt.type.df, aes(x = Type, y = Count, fill = Diet)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.4, alpha = 0.3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0)) +
  scale_fill_manual(values = c("blue", 'red'))

cor.p8 <- ggplot(data = head.wt.tr.type.df, aes(x = Type, y = Count, fill = Diet)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.4, alpha = 0.3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0)) +
  scale_fill_manual(values = c("blue", 'red'))

cor.p9 <- ggplot(data = head.ht.type.df, aes(x = Type, y = Count, fill = Diet)) +
  geom_bar(stat = "identity", color = "black", position = "dodge", width = 0.4, alpha = 0.3) +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.0)) +
  scale_fill_manual(values = c("blue", 'red'))

multiplot(cor.p7, cor.p8, cor.p9, cols=3)

head.wt.fd.edge    <- which(abs(head.wt.fd.cor.lwr) >= 0.8, arr.ind = T)
head.wt.cd.edge    <- which(abs(head.wt.cd.cor.lwr) >= 0.8, arr.ind = T)
head.ht.fd.edge    <- which(abs(head.ht.fd.cor.lwr) >= 0.8, arr.ind = T)
head.ht.cd.edge    <- which(abs(head.ht.cd.cor.lwr) >= 0.8, arr.ind = T)
head.wt.fd2cd.edge <- which(abs(head.wt.fd2cd.cor.lwr) >= 0.8, arr.ind = T)
head.wt.cd2fd.edge <- which(abs(head.wt.cd2fd.cor.lwr) >= 0.8, arr.ind = T)
head.ht.fd2cd.edge <- which(abs(head.ht.fd2cd.cor.lwr) >= 0.8, arr.ind = T)
head.ht.cd2fd.edge <- which(abs(head.ht.cd2fd.cor.lwr) >= 0.8, arr.ind = T)

body.wt.fd.edge    <- which(abs(body.wt.fd.cor.lwr) >= 0.8, arr.ind = T)
body.wt.cd.edge    <- which(abs(body.wt.cd.cor.lwr) >= 0.8, arr.ind = T)
body.ht.fd.edge    <- which(abs(body.ht.fd.cor.lwr) >= 0.8, arr.ind = T)
body.ht.cd.edge    <- which(abs(body.ht.cd.cor.lwr) >= 0.8, arr.ind = T)
body.wt.fd2cd.edge <- which(abs(body.wt.fd2cd.cor.lwr) >= 0.8, arr.ind = T)
body.wt.cd2fd.edge <- which(abs(body.wt.cd2fd.cor.lwr) >= 0.8, arr.ind = T)
body.ht.fd2cd.edge <- which(abs(body.ht.fd2cd.cor.lwr) >= 0.8, arr.ind = T)
body.ht.cd2fd.edge <- which(abs(body.ht.cd2fd.cor.lwr) >= 0.8, arr.ind = T)

head.wt.edge.delta <- nrow(head.wt.cd.edge) - nrow(head.wt.fd.edge)
head.ht.edge.delta <- nrow(head.ht.cd.edge) - nrow(head.ht.fd.edge)
body.wt.edge.delta <- nrow(body.wt.cd.edge) - nrow(body.wt.fd.edge)
body.ht.edge.delta <- nrow(body.ht.cd.edge) - nrow(body.ht.fd.edge)

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
  
  perm.head.wt.fd.cor    <- cor(perm.head.wt.fd,    method = "spearman")
  perm.head.wt.cd.cor    <- cor(perm.head.wt.cd,    method = "spearman")
  perm.head.ht.fd.cor    <- cor(perm.head.ht.fd,    method = "spearman")
  perm.head.ht.cd.cor    <- cor(perm.head.ht.cd,    method = "spearman")
  perm.head.wt.fd2cd.cor <- cor(perm.head.wt.fd2cd, method = "spearman")
  perm.head.wt.cd2fd.cor <- cor(perm.head.wt.cd2fd, method = "spearman")
  perm.head.ht.fd2cd.cor <- cor(perm.head.ht.fd2cd, method = "spearman")
  perm.head.ht.cd2fd.cor <- cor(perm.head.ht.cd2fd, method = "spearman")
  
  perm.body.wt.fd.cor    <- cor(perm.body.wt.fd,    method = "spearman")
  perm.body.wt.cd.cor    <- cor(perm.body.wt.cd,    method = "spearman")
  perm.body.ht.fd.cor    <- cor(perm.body.ht.fd,    method = "spearman")
  perm.body.ht.cd.cor    <- cor(perm.body.ht.cd,    method = "spearman")
  perm.body.wt.fd2cd.cor <- cor(perm.body.wt.fd2cd, method = "spearman")
  perm.body.wt.cd2fd.cor <- cor(perm.body.wt.cd2fd, method = "spearman")
  perm.body.ht.fd2cd.cor <- cor(perm.body.ht.fd2cd, method = "spearman")
  perm.body.ht.cd2fd.cor <- cor(perm.body.ht.cd2fd, method = "spearman")
  
  perm.head.wt.fd.cor.lwr    <- perm.head.wt.fd.cor*lower.tri(perm.head.wt.fd.cor)
  perm.head.wt.cd.cor.lwr    <- perm.head.wt.cd.cor*lower.tri(perm.head.wt.cd.cor)
  perm.head.ht.fd.cor.lwr    <- perm.head.ht.fd.cor*lower.tri(perm.head.ht.fd.cor)
  perm.head.ht.cd.cor.lwr    <- perm.head.ht.cd.cor*lower.tri(perm.head.ht.cd.cor)
  perm.head.wt.fd2cd.cor.lwr <- perm.head.wt.fd2cd.cor*lower.tri(perm.head.wt.fd2cd.cor)
  perm.head.wt.cd2fd.cor.lwr <- perm.head.wt.cd2fd.cor*lower.tri(perm.head.wt.cd2fd.cor)
  perm.head.ht.fd2cd.cor.lwr <- perm.head.ht.fd2cd.cor*lower.tri(perm.head.ht.fd2cd.cor)
  perm.head.ht.cd2fd.cor.lwr <- perm.head.ht.cd2fd.cor*lower.tri(perm.head.ht.cd2fd.cor)
  
  perm.body.wt.fd.cor.lwr    <- perm.body.wt.fd.cor*lower.tri(perm.body.wt.fd.cor)
  perm.body.wt.cd.cor.lwr    <- perm.body.wt.cd.cor*lower.tri(perm.body.wt.cd.cor)
  perm.body.ht.fd.cor.lwr    <- perm.body.ht.fd.cor*lower.tri(perm.body.ht.fd.cor)
  perm.body.ht.cd.cor.lwr    <- perm.body.ht.cd.cor*lower.tri(perm.body.ht.cd.cor)
  perm.body.wt.fd2cd.cor.lwr <- perm.body.wt.fd2cd.cor*lower.tri(perm.body.wt.fd2cd.cor)
  perm.body.wt.cd2fd.cor.lwr <- perm.body.wt.cd2fd.cor*lower.tri(perm.body.wt.cd2fd.cor)
  perm.body.ht.fd2cd.cor.lwr <- perm.body.ht.fd2cd.cor*lower.tri(perm.body.ht.fd2cd.cor)
  perm.body.ht.cd2fd.cor.lwr <- perm.body.ht.cd2fd.cor*lower.tri(perm.body.ht.cd2fd.cor)
  
  perm.head.wt.fd.edge.n[i]    <- nrow(which(abs(perm.head.wt.fd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.wt.cd.edge.n[i]    <- nrow(which(abs(perm.head.wt.cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.ht.fd.edge.n[i]    <- nrow(which(abs(perm.head.ht.fd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.ht.cd.edge.n[i]    <- nrow(which(abs(perm.head.ht.cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.wt.fd2cd.edge.n[i] <- nrow(which(abs(perm.head.wt.fd2cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.wt.cd2fd.edge.n[i] <- nrow(which(abs(perm.head.wt.cd2fd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.ht.fd2cd.edge.n[i] <- nrow(which(abs(perm.head.ht.fd2cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.head.ht.cd2fd.edge.n[i] <- nrow(which(abs(perm.head.ht.cd2fd.cor.lwr) >= 0.8, arr.ind = T))
  
  perm.body.wt.fd.edge.n[i]    <- nrow(which(abs(perm.body.wt.fd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.wt.cd.edge.n[i]    <- nrow(which(abs(perm.body.wt.cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.ht.fd.edge.n[i]    <- nrow(which(abs(perm.body.ht.fd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.ht.cd.edge.n[i]    <- nrow(which(abs(perm.body.ht.cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.wt.fd2cd.edge.n[i] <- nrow(which(abs(perm.body.wt.fd2cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.wt.cd2fd.edge.n[i] <- nrow(which(abs(perm.body.wt.cd2fd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.ht.fd2cd.edge.n[i] <- nrow(which(abs(perm.body.ht.fd2cd.cor.lwr) >= 0.8, arr.ind = T))
  perm.body.ht.cd2fd.edge.n[i] <- nrow(which(abs(perm.body.ht.cd2fd.cor.lwr) >= 0.8, arr.ind = T))
  
  perm.head.wt.edge.delta[i] <- perm.head.wt.cd.edge.n[i] - perm.head.wt.fd.edge.n[i]
  perm.head.ht.edge.delta[i] <- perm.head.ht.cd.edge.n[i] - perm.head.ht.fd.edge.n[i]
  perm.body.wt.edge.delta[i] <- perm.body.wt.cd.edge.n[i] - perm.body.wt.fd.edge.n[i]
  perm.body.ht.edge.delta[i] <- perm.body.ht.cd.edge.n[i] - perm.body.ht.fd.edge.n[i]
}

### Find percentile
mean(nrow(head.wt.fd.edge) >= perm.head.wt.fd.edge.n)

par(mfrow=c(2, 1), mar = c(6, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.5)
plot(jitter(rep(1:6, each = 10000)), 
     c(perm.head.wt.fd.edge.n, perm.head.wt.cd.edge.n,
       perm.head.wt.fd2cd.edge.n, perm.head.wt.cd2fd.edge.n,
       perm.head.ht.fd.edge.n, perm.head.ht.cd.edge.n),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 8), ylim = c(0, 1200), las = 1, axes = F, xaxs="i", yaxs="i",
     xlab = "", ylab = "Number of Edges", main = "Head")
axis(1, at = seq(1, 6, by = 1), las = 2,
     labels = c("w1118/FD", "w1118/CD", "w1118/FD2CD", "w1118/CD2FD", "5-HT2A/FD", "5-HT2A/CD"))
axis(2, at = seq(0, 1200, by = 200), las = 1)
segments(0.7, median(perm.head.wt.fd.edge.n), 1.3, median(perm.head.wt.fd.edge.n), 
         col = "black", lwd = 4)
segments(0.7, nrow(head.wt.fd.edge), 1.3, nrow(head.wt.fd.edge), 
         col = "red", lwd = 4)
segments(1.7, median(perm.head.wt.cd.edge.n), 2.3, median(perm.head.wt.cd.edge.n), 
         col = "black", lwd = 4)
segments(1.7, nrow(head.wt.cd.edge), 2.3, nrow(head.wt.cd.edge), 
         col = "red", lwd = 4)
segments(2.7, median(perm.head.wt.fd2cd.edge.n), 3.3, median(perm.head.wt.fd2cd.edge.n), 
         col = "black", lwd = 4)
segments(2.7, nrow(head.wt.fd2cd.edge), 3.3, nrow(head.wt.fd2cd.edge), 
         col = "red", lwd = 4)
segments(3.7, median(perm.head.wt.cd2fd.edge.n), 4.3, median(perm.head.wt.cd2fd.edge.n), 
         col = "black", lwd = 4)
segments(3.7, nrow(head.wt.cd2fd.edge), 4.3, nrow(head.wt.cd2fd.edge), 
         col = "red", lwd = 4)
segments(4.7, median(perm.head.ht.fd.edge.n), 5.3, median(perm.head.ht.fd.edge.n), 
         col = "black", lwd = 4)
segments(4.7, nrow(head.ht.fd.edge), 5.3, nrow(head.ht.fd.edge), 
         col = "red", lwd = 4)
segments(5.7, median(perm.head.ht.cd.edge.n), 6.3, median(perm.head.ht.cd.edge.n), 
         col = "black", lwd = 4)
segments(5.7, nrow(head.ht.cd.edge), 6.3, nrow(head.ht.cd.edge), 
         col = "red", lwd = 4)
legend("topleft", lwd = c(4, 4), col = c("black", "red"), 
       legend = c("Median (Permutation)", "Observed"), bty = "n", cex = 0.8)

plot(jitter(rep(1:6, each = 10000)), 
     c(perm.body.wt.fd.edge.n, perm.body.wt.cd.edge.n,
       perm.body.wt.fd2cd.edge.n, perm.body.wt.cd2fd.edge.n,
       perm.body.ht.fd.edge.n, perm.body.ht.cd.edge.n),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 8), ylim = c(0, 2400), las = 1, axes = F, xaxs="i", yaxs="i",
     xlab = "", ylab = "Number of Edges", main = "Body")
axis(1, at = seq(1, 6, by = 1), las = 2,
     labels = c("w1118/FD", "w1118/CD", 
                "w1118/FD2CD", "w1118/CD2FD", 
                "5-HT2A/FD", "5-HT2A/CD"))
axis(2, at = seq(0, 2400, by = 400), las = 1)

segments(0.7, median(perm.body.wt.fd.edge.n), 
         1.3, median(perm.body.wt.fd.edge.n), 
         col = "black", lwd = 4)

segments(0.7, nrow(body.wt.fd.edge), 
         1.3, nrow(body.wt.fd.edge), 
         col = "red", lwd = 4)

segments(1.7, median(perm.body.wt.cd.edge.n), 
         2.3, median(perm.body.wt.cd.edge.n), 
         col = "black", lwd = 4)

segments(1.7, nrow(body.wt.cd.edge), 
         2.3, nrow(body.wt.cd.edge), 
         col = "red", lwd = 4)

segments(2.7, median(perm.body.wt.fd2cd.edge.n), 
         3.3, median(perm.body.wt.fd2cd.edge.n), 
         col = "black", lwd = 4)

segments(2.7, nrow(body.wt.fd2cd.edge), 
         3.3, nrow(body.wt.fd2cd.edge), 
         col = "red", lwd = 4)

segments(3.7, median(perm.body.wt.cd2fd.edge.n), 
         4.3, median(perm.body.wt.cd2fd.edge.n), 
         col = "black", lwd = 4)

segments(3.7, nrow(body.wt.cd2fd.edge), 
         4.3, nrow(body.wt.cd2fd.edge), 
         col = "red", lwd = 4)

segments(4.7, median(perm.body.ht.fd.edge.n), 
         5.3, median(perm.body.ht.fd.edge.n), 
         col = "black", lwd = 4)

segments(4.7, nrow(body.ht.fd.edge), 
         5.3, nrow(body.ht.fd.edge), 
         col = "red", lwd = 4)

segments(5.7, median(perm.body.ht.cd.edge.n), 
         6.3, median(perm.body.ht.cd.edge.n), 
         col = "black", lwd = 4)

segments(5.7, nrow(body.ht.cd.edge), 
         6.3, nrow(body.ht.cd.edge), 
         col = "red", lwd = 4)

legend("topleft", lwd = c(4, 4), col = c("black", "red"), 
       legend = c("Median (Permutation)", "Observed"), bty = "n", cex = 0.8)

par(mfrow=c(1, 2), mar = c(6, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.5)
plot(jitter(rep(1:2, each = 10000)), 
     c(perm.head.wt.edge.delta, perm.head.ht.edge.delta),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 2.8), ylim = c(-1000, 1000), 
     las = 1, axes = F, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "Change of edge number by dietary choice", main = "Head")

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

text (1, 900, "P=0.0011", col = "red")
text (2, 900, "P=0.1547")

plot(jitter(rep(1:2, each = 10000)), 
     c(perm.body.wt.edge.delta, perm.body.ht.edge.delta),
     pch = 3, col = c("grey47","grey57", "grey67", "grey77", "grey87", "grey97"),
     xlim = c(0.2, 2.8), ylim = c(-2000, 2000), 
     las = 1, axes = F, xaxs = "i", yaxs = "i",
     xlab = "", ylab = "Change of edge number by dietary choice", main = "Body")

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

text (1, 1800, "P=0.1025")
text (2, 1800, "P=0.2758")

# Connectivity Analysis ---------------------------------------------------
## Calculate diameter
diameter(head.wt.fd.g)
diameter(head.wt.cd.g)
diameter(head.ht.fd.g)
diameter(head.ht.cd.g)
diameter(body.wt.fd.g)
diameter(body.wt.cd.g)
diameter(body.ht.fd.g)
diameter(body.ht.cd.g)

## Calculate average distance
average.path.length(head.wt.fd.g)
average.path.length(head.wt.cd.g)
average.path.length(head.ht.fd.g)
average.path.length(head.ht.cd.g)
average.path.length(body.wt.fd.g)
average.path.length(body.wt.cd.g)
average.path.length(body.ht.fd.g)
average.path.length(body.ht.cd.g)

## Calculate distances (all the shortest paths in the network)
dist.head.wt.fd <- distances(head.wt.fd.g)
dist.head.wt.cd <- distances(head.wt.cd.g)
dist.head.ht.fd <- distances(head.ht.fd.g)
dist.head.ht.cd <- distances(head.ht.cd.g)
dist.body.wt.fd <- distances(body.wt.fd.g)
dist.body.wt.cd <- distances(body.wt.cd.g)
dist.body.ht.fd <- distances(body.ht.fd.g)
dist.body.ht.cd <- distances(body.ht.cd.g)

dist.head.core.wt.fd <- distances(head.wt.fd.gc)
dist.head.core.wt.cd <- distances(head.wt.cd.gc)
dist.head.core.ht.fd <- distances(head.ht.fd.gc)
dist.head.core.ht.cd <- distances(head.ht.cd.gc)
dist.body.core.wt.fd <- distances(body.wt.fd.gc)
dist.body.core.wt.cd <- distances(body.wt.cd.gc)
dist.body.core.ht.fd <- distances(body.ht.fd.gc)
dist.body.core.ht.cd <- distances(body.ht.cd.gc)

dist.head.wt.fd[upper.tri(dist.head.wt.fd, diag = T)] <- NA
dist.head.wt.cd[upper.tri(dist.head.wt.cd, diag = T)] <- NA
dist.head.ht.fd[upper.tri(dist.head.ht.fd, diag = T)] <- NA
dist.head.ht.cd[upper.tri(dist.head.ht.cd, diag = T)] <- NA
dist.body.wt.fd[upper.tri(dist.body.wt.fd, diag = T)] <- NA
dist.body.wt.cd[upper.tri(dist.body.wt.cd, diag = T)] <- NA
dist.body.ht.fd[upper.tri(dist.body.ht.fd, diag = T)] <- NA
dist.body.ht.cd[upper.tri(dist.body.ht.cd, diag = T)] <- NA

dist.head.core.wt.fd[upper.tri(dist.head.core.wt.fd, diag = T)] <- NA
dist.head.core.wt.cd[upper.tri(dist.head.core.wt.cd, diag = T)] <- NA
dist.head.core.ht.fd[upper.tri(dist.head.core.ht.fd, diag = T)] <- NA
dist.head.core.ht.cd[upper.tri(dist.head.core.ht.cd, diag = T)] <- NA
dist.body.core.wt.fd[upper.tri(dist.body.core.wt.fd, diag = T)] <- NA
dist.body.core.wt.cd[upper.tri(dist.body.core.wt.cd, diag = T)] <- NA
dist.body.core.ht.fd[upper.tri(dist.body.core.ht.fd, diag = T)] <- NA
dist.body.core.ht.cd[upper.tri(dist.body.core.ht.cd, diag = T)] <- NA

dist.head.wt.fd <- as.vector(dist.head.wt.fd)
dist.head.wt.cd <- as.vector(dist.head.wt.cd)
dist.head.ht.fd <- as.vector(dist.head.ht.fd)
dist.head.ht.cd <- as.vector(dist.head.ht.cd)
dist.body.wt.fd <- as.vector(dist.body.wt.fd)
dist.body.wt.cd <- as.vector(dist.body.wt.cd)
dist.body.ht.fd <- as.vector(dist.body.ht.fd)
dist.body.ht.cd <- as.vector(dist.body.ht.cd)

dist.head.core.wt.fd <- as.vector(dist.head.core.wt.fd)
dist.head.core.wt.cd <- as.vector(dist.head.core.wt.cd)
dist.head.core.ht.fd <- as.vector(dist.head.core.ht.fd)
dist.head.core.ht.cd <- as.vector(dist.head.core.ht.cd)
dist.body.core.wt.fd <- as.vector(dist.body.core.wt.fd)
dist.body.core.wt.cd <- as.vector(dist.body.core.wt.cd)
dist.body.core.ht.fd <- as.vector(dist.body.core.ht.fd)
dist.body.core.ht.cd <- as.vector(dist.body.core.ht.cd)

dist.head.wt.fd <- dist.head.wt.fd[!is.na(dist.head.wt.fd)]
dist.head.wt.cd <- dist.head.wt.cd[!is.na(dist.head.wt.cd)]
dist.head.ht.fd <- dist.head.ht.fd[!is.na(dist.head.ht.fd)]
dist.head.ht.cd <- dist.head.ht.cd[!is.na(dist.head.ht.cd)]
dist.body.wt.fd <- dist.body.wt.fd[!is.na(dist.body.wt.fd)]
dist.body.wt.cd <- dist.body.wt.cd[!is.na(dist.body.wt.cd)]
dist.body.ht.fd <- dist.body.ht.fd[!is.na(dist.body.ht.fd)]
dist.body.ht.cd <- dist.body.ht.cd[!is.na(dist.body.ht.cd)]

dist.head.core.wt.fd <- dist.head.core.wt.fd[!is.na(dist.head.wt.fd)]
dist.head.core.wt.cd <- dist.head.core.wt.cd[!is.na(dist.head.wt.cd)]
dist.head.core.ht.fd <- dist.head.core.ht.fd[!is.na(dist.head.ht.fd)]
dist.head.core.ht.cd <- dist.head.core.ht.cd[!is.na(dist.head.ht.cd)]
dist.body.core.wt.fd <- dist.body.core.wt.fd[!is.na(dist.body.wt.fd)]
dist.body.core.wt.cd <- dist.body.core.wt.cd[!is.na(dist.body.wt.cd)]
dist.body.core.ht.fd <- dist.body.core.ht.fd[!is.na(dist.body.ht.fd)]
dist.body.core.ht.cd <- dist.body.core.ht.cd[!is.na(dist.body.ht.cd)]

## Calucalte and plot the number of connected and disconnected vertex pairs
df.group <- rep(c("Head/w1118/FD", "Head/w1118/CD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                  "Body/w1118/FD", "Body/w1118/CD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"), each = 2)
df.group <- factor(df.group, 
                   levels = c("Head/w1118/FD", "Head/w1118/CD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                              "Body/w1118/FD", "Body/w1118/CD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"))

df.type  <- rep(c("Disconnected", "Connected"), time = 8)
df.type <-factor(df.type, levels = c("Disconnected", "Connected"))

df.count <- c(as.numeric(table(is.finite(dist.head.wt.fd))),
              as.numeric(table(is.finite(dist.head.wt.cd))),
              as.numeric(table(is.finite(dist.head.ht.fd))),
              as.numeric(table(is.finite(dist.head.ht.cd))),
              as.numeric(table(is.finite(dist.body.wt.fd))),
              as.numeric(table(is.finite(dist.body.wt.cd))),
              as.numeric(table(is.finite(dist.body.ht.fd))),
              as.numeric(table(is.finite(dist.body.ht.cd))))

df.prop  <- c(as.numeric(table(is.finite(dist.head.wt.fd)))/5253,
              as.numeric(table(is.finite(dist.head.wt.cd)))/5253,
              as.numeric(table(is.finite(dist.head.ht.fd)))/5253,
              as.numeric(table(is.finite(dist.head.ht.cd)))/5253,
              as.numeric(table(is.finite(dist.body.wt.fd)))/7750,
              as.numeric(table(is.finite(dist.body.wt.cd)))/7750,
              as.numeric(table(is.finite(dist.body.ht.fd)))/7750,
              as.numeric(table(is.finite(dist.body.ht.cd)))/7750)

df <- data.frame(Group = df.group, Type = df.type, 
                 Count = df.count, Proportion = df.prop)

p <- ggplot(data = df, aes(x = Group, y = Proportion, fill = Type)) +
    geom_bar(stat = "identity", color = "black") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
p + scale_fill_manual(values = c('#999999', '#E69F00'))

## Calculate and plot the average distances in networks
dist.head.wt.fd <- dist.head.wt.fd[is.finite(dist.head.wt.fd)]
dist.head.wt.cd <- dist.head.wt.cd[is.finite(dist.head.wt.cd)]
dist.head.ht.fd <- dist.head.ht.fd[is.finite(dist.head.ht.fd)]
dist.head.ht.cd <- dist.head.ht.cd[is.finite(dist.head.ht.cd)]
dist.body.wt.fd <- dist.body.wt.fd[is.finite(dist.body.wt.fd)] 
dist.body.wt.cd <- dist.body.wt.cd[is.finite(dist.body.wt.cd)]
dist.body.ht.fd <- dist.body.ht.fd[is.finite(dist.body.ht.fd)] 
dist.body.ht.cd <- dist.body.ht.cd[is.finite(dist.body.ht.cd)]

dist.head.core.wt.fd <- dist.head.wt.fd[is.finite(dist.head.core.wt.fd)]
dist.head.core.wt.cd <- dist.head.wt.cd[is.finite(dist.head.core.wt.cd)]
dist.head.core.ht.fd <- dist.head.ht.fd[is.finite(dist.head.core.ht.fd)]
dist.head.core.ht.cd <- dist.head.ht.cd[is.finite(dist.head.core.ht.cd)]
dist.body.core.wt.fd <- dist.body.wt.fd[is.finite(dist.body.core.wt.fd)] 
dist.body.core.wt.cd <- dist.body.wt.cd[is.finite(dist.body.core.wt.cd)]
dist.body.core.ht.fd <- dist.body.ht.fd[is.finite(dist.body.core.ht.fd)] 
dist.body.core.ht.cd <- dist.body.ht.cd[is.finite(dist.body.core.ht.cd)]

df.distance <- c(dist.head.wt.fd, dist.head.wt.cd, 
                 dist.head.ht.fd, dist.head.ht.cd,
                 dist.body.wt.fd, dist.body.wt.cd, 
                 dist.body.ht.fd, dist.body.ht.cd)

df.core.distance <- c(dist.head.core.wt.fd, dist.head.core.wt.cd, 
                      dist.head.core.ht.fd, dist.head.core.ht.cd,
                      dist.body.core.wt.fd, dist.body.core.wt.cd, 
                      dist.body.core.ht.fd, dist.body.core.ht.cd)

df.group <- rep(c("Head/w1118/FD", "Head/w1118/CD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                  "Body/w1118/FD", "Body/w1118/CD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"),
                c(length(dist.head.wt.fd), length(dist.head.wt.cd), 
                  length(dist.head.ht.fd), length(dist.head.ht.cd),
                  length(dist.body.wt.fd), length(dist.body.wt.cd), 
                  length(dist.body.ht.fd), length(dist.body.ht.cd)))

df.core.group <- rep(c("Head/w1118/FD", "Head/w1118/CD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                       "Body/w1118/FD", "Body/w1118/CD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"),
                     c(length(dist.head.core.wt.fd), length(dist.head.core.wt.cd), 
                       length(dist.head.core.ht.fd), length(dist.head.core.ht.cd),
                       length(dist.body.core.wt.fd), length(dist.body.core.wt.cd), 
                       length(dist.body.core.ht.fd), length(dist.body.core.ht.cd)))

df.group <- factor(df.group, 
                   levels = c("Head/w1118/FD", "Head/w1118/CD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                              "Body/w1118/FD", "Body/w1118/CD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"))

df.core.group <- factor(df.core.group, 
                        levels = c("Head/w1118/FD", "Head/w1118/CD", "Head/5-HT2A/FD", "Head/5-HT2A/CD",
                                   "Body/w1118/FD", "Body/w1118/CD", "Body/5-HT2A/FD", "Body/5-HT2A/CD"))

x  <- matrix(unlist(strsplit(as.character(df.group), "/")), ncol = 3, byrow = T)
xc <- matrix(unlist(strsplit(as.character(df.core.group), "/")), ncol = 3, byrow = T)

df <- data.frame(Group = df.group, Distance = df.distance, 
                 Genotype = x[, 2], Tissue = x[, 1], Diet = x[, 3])

df.core <- data.frame(Group = df.core.group, Distance = df.core.distance, 
                      Genotype = xc[, 2], Tissue = xc[, 1], Diet = xc[, 3])

df$Distance2 <- log2(df$Distance)
df.core$Distance2 <- log2(df.core$Distance)

df.head <- df[df$Tissue == "Head", ]
df.body <- df[df$Tissue == "Body", ]
df.core.head <- df.core[df.core$Tissue == "Head", ]
df.core.body <- df.core[df.core$Tissue == "Body", ]

dist.p1 <- ggplot(df.head, aes(x = Group, y = Distance2)) + 
  geom_violin(trim = FALSE, fill = "gray") + 
  labs(title="Whole Network/Heads",x = "Group", y = "Log2(Distance)") + 
  stat_summary(fun.data = mean_sdl, 
               geom = "pointrange", color = "red") +
  theme_classic()

dist.p2 <- ggplot(df.body, aes(x = Group, y = Distance2)) + 
  geom_violin(trim = FALSE, fill = "gray") + 
  labs(title = "Whole Network/Bodies", x = "Group", y = "Log2(Distance)") + 
  stat_summary(fun.data = mean_sdl, 
               geom = "pointrange", color = "red") + 
  theme_classic()

dist.p3 <- ggplot(df.core.head, aes(x = Group, y = Distance2)) + 
  geom_violin(trim = FALSE, fill = "gray") + 
  labs(title = "Core/Heads", x = "Group", y = "Log2(Distance)") +
  stat_summary(fun.data = mean_sdl, 
               geom = "pointrange", color = "red")+
  theme_classic()

dist.p4 <- ggplot(df.core.body, aes(x = Group, y = Distance2)) + 
  geom_violin(trim = FALSE, fill = "gray") + 
  labs(title="Core/Bodies", x = "Group", y = "Log2(Distance)") +
  stat_summary(fun.data = mean_sdl, 
               geom = "pointrange", color = "red") +
  theme_classic()

multiplot(dist.p1, dist.p2, dist.p3, dist.p4, 
          layout = matrix(c(1, 2, 3, 4), nrow = 2, byrow = T))

wilcox.test(dist.head.ht.fd, dist.head.ht.cd)
wilcox.test(dist.head.wt.fd, dist.head.wt.cd)
wilcox.test(dist.body.wt.fd, dist.body.wt.cd)
wilcox.test(dist.body.ht.fd, dist.body.ht.cd)

res.aov.head <- aov(Distance ~ Diet + Genotype + Diet:Genotype, data = df.head)
res.aov.body <- aov(Distance ~ Diet + Genotype + Diet:Genotype, data = df.body)

res.aov.head.core <- aov(Distance ~ Diet + Genotype + Diet:Genotype, data = df.core.head)
res.aov.body.core <- aov(Distance ~ Diet + Genotype + Diet:Genotype, data = df.core.body)

summary(res.aov.head)
summary(res.aov.body)

summary(res.aov.head.core)
summary(res.aov.body.core)