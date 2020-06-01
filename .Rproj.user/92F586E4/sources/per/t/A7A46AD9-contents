## Subject: Network Analysis - Network Construction
## Project: Fly Choice Diet
## Part: 7-1
## Author: Yang Lyu
## Date created: 09/29/2019
## Date modified: 05/31/2020

# Environment Settings ----------------------------------------------------
library(igraph)
library(gtools)
library(UpSetR)

# Data Input --------------------------------------------------------------
## Read data
rawdata.head <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Head.RDS")
rawdata.body <- readRDS("Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Body.RDS")

# Data Clean --------------------------------------------------------------
## Transpose matrix
data.head <- t(rawdata.head)
data.body <- t(rawdata.body)

## Subset treatment
head.wt.fd <- data.head[grep("w1118_FD_Head",  rownames(data.head)), ]
head.wt.cd <- data.head[grep("w1118_CD_Head",  rownames(data.head)), ]
head.ht.fd <- data.head[grep("5-HT2A_FD_Head", rownames(data.head)), ]
head.ht.cd <- data.head[grep("5-HT2A_CD_Head", rownames(data.head)), ]
head.wt.fd2cd <- data.head[grep("w1118_FD2CD_Head",  rownames(data.head)), ]
head.wt.cd2fd <- data.head[grep("w1118_CD2FD_Head",  rownames(data.head)), ]
head.ht.fd2cd <- data.head[grep("5-HT2A_FD2CD_Head", rownames(data.head)), ]
head.ht.cd2fd <- data.head[grep("5-HT2A_CD2FD_Head", rownames(data.head)), ]

body.wt.fd <- data.body[grep("w1118_FD_Body",  rownames(data.body)), ]
body.wt.cd <- data.body[grep("w1118_CD_Body",  rownames(data.body)), ]
body.ht.fd <- data.body[grep("5-HT2A_FD_Body", rownames(data.body)), ]
body.ht.cd <- data.body[grep("5-HT2A_CD_Body", rownames(data.body)), ]
body.wt.fd2cd <- data.body[grep("w1118_FD2CD_Body",  rownames(data.body)), ]
body.wt.cd2fd <- data.body[grep("w1118_CD2FD_Body",  rownames(data.body)), ]
body.ht.fd2cd <- data.body[grep("5-HT2A_FD2CD_Body", rownames(data.body)), ]
body.ht.cd2fd <- data.body[grep("5-HT2A_CD2FD_Body", rownames(data.body)), ]

# Network Construction ----------------------------------------------------
## Calculate adjacency matrix
head.wt.fd.cor <- cor(head.wt.fd, method = "spearman")
head.wt.cd.cor <- cor(head.wt.cd, method = "spearman")
head.ht.fd.cor <- cor(head.ht.fd, method = "spearman")
head.ht.cd.cor <- cor(head.ht.cd, method = "spearman")
head.wt.fd2cd.cor <- cor(head.wt.fd2cd, method = "spearman")
head.wt.cd2fd.cor <- cor(head.wt.cd2fd, method = "spearman")
head.ht.fd2cd.cor <- cor(head.ht.fd2cd, method = "spearman")
head.ht.cd2fd.cor <- cor(head.ht.cd2fd, method = "spearman")

body.wt.fd.cor <- cor(body.wt.fd, method = "spearman")
body.wt.cd.cor <- cor(body.wt.cd, method = "spearman")
body.ht.fd.cor <- cor(body.ht.fd, method = "spearman")
body.ht.cd.cor <- cor(body.ht.cd, method = "spearman")
body.wt.fd2cd.cor <- cor(body.wt.fd2cd, method = "spearman")
body.wt.cd2fd.cor <- cor(body.wt.cd2fd, method = "spearman")
body.ht.fd2cd.cor <- cor(body.ht.fd2cd, method = "spearman")
body.ht.cd2fd.cor <- cor(body.ht.cd2fd, method = "spearman")

## Remove self-loop
diag(head.wt.fd.cor) <- 0
diag(head.wt.cd.cor) <- 0
diag(head.ht.fd.cor) <- 0
diag(head.ht.cd.cor) <- 0
diag(head.wt.fd2cd.cor) <- 0
diag(head.wt.cd2fd.cor) <- 0
diag(head.ht.fd2cd.cor) <- 0
diag(head.ht.cd2fd.cor) <- 0

diag(body.wt.fd.cor) <- 0
diag(body.wt.cd.cor) <- 0
diag(body.ht.fd.cor) <- 0
diag(body.ht.cd.cor) <- 0
diag(body.wt.fd2cd.cor) <- 0
diag(body.wt.cd2fd.cor) <- 0
diag(body.ht.fd2cd.cor) <- 0
diag(body.ht.cd2fd.cor) <- 0

## Return the lower triangle of the matrices
head.wt.fd.cor.lwr <- head.wt.fd.cor * lower.tri(head.wt.fd.cor)
head.wt.cd.cor.lwr <- head.wt.cd.cor * lower.tri(head.wt.cd.cor)
head.ht.fd.cor.lwr <- head.ht.fd.cor * lower.tri(head.ht.fd.cor)
head.ht.cd.cor.lwr <- head.ht.cd.cor * lower.tri(head.ht.cd.cor)
head.wt.fd2cd.cor.lwr <- head.wt.fd2cd.cor * lower.tri(head.wt.fd2cd.cor)
head.wt.cd2fd.cor.lwr <- head.wt.cd2fd.cor * lower.tri(head.wt.cd2fd.cor)
head.ht.fd2cd.cor.lwr <- head.ht.fd2cd.cor * lower.tri(head.ht.fd2cd.cor)
head.ht.cd2fd.cor.lwr <- head.ht.cd2fd.cor * lower.tri(head.ht.cd2fd.cor)

body.wt.fd.cor.lwr <- body.wt.fd.cor * lower.tri(body.wt.fd.cor)
body.wt.cd.cor.lwr <- body.wt.cd.cor * lower.tri(body.wt.cd.cor)
body.ht.fd.cor.lwr <- body.ht.fd.cor * lower.tri(body.ht.fd.cor)
body.ht.cd.cor.lwr <- body.ht.cd.cor * lower.tri(body.ht.cd.cor)
body.wt.fd2cd.cor.lwr <- body.wt.fd2cd.cor * lower.tri(body.wt.fd2cd.cor)
body.wt.cd2fd.cor.lwr <- body.wt.cd2fd.cor * lower.tri(body.wt.cd2fd.cor)
body.ht.fd2cd.cor.lwr <- body.ht.fd2cd.cor * lower.tri(body.ht.fd2cd.cor)
body.ht.cd2fd.cor.lwr <- body.ht.cd2fd.cor * lower.tri(body.ht.cd2fd.cor)

## Construct adjacency matrix
head.wt.fd.adj <- head.wt.fd.cor
head.wt.cd.adj <- head.wt.cd.cor
head.ht.fd.adj <- head.ht.fd.cor
head.ht.cd.adj <- head.ht.cd.cor
head.wt.fd2cd.adj <- head.wt.fd2cd.cor
head.wt.cd2fd.adj <- head.wt.cd2fd.cor
head.ht.fd2cd.adj <- head.ht.fd2cd.cor
head.ht.cd2fd.adj <- head.ht.cd2fd.cor

body.wt.fd.adj <- body.wt.fd.cor
body.wt.cd.adj <- body.wt.cd.cor
body.ht.fd.adj <- body.ht.fd.cor
body.ht.cd.adj <- body.ht.cd.cor
body.wt.fd2cd.adj <- body.wt.fd2cd.cor
body.wt.cd2fd.adj <- body.wt.cd2fd.cor
body.ht.fd2cd.adj <- body.ht.fd2cd.cor
body.ht.cd2fd.adj <- body.ht.cd2fd.cor

head.wt.fd.adj[head.wt.fd.adj >= 0.8 | head.wt.fd.adj <= -0.8] = 1
head.wt.cd.adj[head.wt.cd.adj >= 0.8 | head.wt.cd.adj <= -0.8] = 1
head.ht.fd.adj[head.ht.fd.adj >= 0.8 | head.ht.fd.adj <= -0.8] = 1
head.ht.cd.adj[head.ht.cd.adj >= 0.8 | head.ht.cd.adj <= -0.8] = 1
head.wt.fd2cd.adj[head.wt.fd2cd.adj >= 0.8 | head.wt.fd2cd.adj <= -0.8] = 1
head.wt.cd2fd.adj[head.wt.cd2fd.adj >= 0.8 | head.wt.cd2fd.adj <= -0.8] = 1
head.ht.fd2cd.adj[head.ht.fd2cd.adj >= 0.8 | head.ht.fd2cd.adj <= -0.8] = 1
head.ht.cd2fd.adj[head.ht.cd2fd.adj >= 0.8 | head.ht.cd2fd.adj <= -0.8] = 1

body.wt.fd.adj[body.wt.fd.adj >= 0.8 | body.wt.fd.adj <= -0.8] = 1
body.wt.cd.adj[body.wt.cd.adj >= 0.8 | body.wt.cd.adj <= -0.8] = 1
body.ht.fd.adj[body.ht.fd.adj >= 0.8 | body.ht.fd.adj <= -0.8] = 1
body.ht.cd.adj[body.ht.cd.adj >= 0.8 | body.ht.cd.adj <= -0.8] = 1
body.wt.fd2cd.adj[body.wt.fd2cd.adj >= 0.8 | body.wt.fd2cd.adj <= -0.8] = 1
body.wt.cd2fd.adj[body.wt.cd2fd.adj >= 0.8 | body.wt.cd2fd.adj <= -0.8] = 1
body.ht.fd2cd.adj[body.ht.fd2cd.adj >= 0.8 | body.ht.fd2cd.adj <= -0.8] = 1
body.ht.cd2fd.adj[body.ht.cd2fd.adj >= 0.8 | body.ht.cd2fd.adj <= -0.8] = 1

head.wt.fd.adj[head.wt.fd.adj != 1] <- 0
head.wt.cd.adj[head.wt.cd.adj != 1] <- 0
head.ht.fd.adj[head.ht.fd.adj != 1] <- 0
head.ht.cd.adj[head.ht.cd.adj != 1] <- 0
head.wt.fd2cd.adj[head.wt.fd2cd.adj != 1] <- 0
head.wt.cd2fd.adj[head.wt.cd2fd.adj != 1] <- 0
head.ht.fd2cd.adj[head.ht.fd2cd.adj != 1] <- 0
head.ht.cd2fd.adj[head.ht.cd2fd.adj != 1] <- 0

body.wt.fd.adj[body.wt.fd.adj != 1] <- 0
body.wt.cd.adj[body.wt.cd.adj != 1] <- 0
body.ht.fd.adj[body.ht.fd.adj != 1] <- 0
body.ht.cd.adj[body.ht.cd.adj != 1] <- 0
body.wt.fd2cd.adj[body.wt.fd2cd.adj != 1] <- 0
body.wt.cd2fd.adj[body.wt.cd2fd.adj != 1] <- 0
body.ht.fd2cd.adj[body.ht.fd2cd.adj != 1] <- 0
body.ht.cd2fd.adj[body.ht.cd2fd.adj != 1] <- 0

head.wt.fd.weighted.adj <- head.wt.fd.cor
head.wt.cd.weighted.adj <- head.wt.cd.cor
head.ht.fd.weighted.adj <- head.ht.fd.cor
head.ht.cd.weighted.adj <- head.ht.cd.cor
head.wt.fd2cd.weighted.adj <- head.wt.fd2cd.cor
head.wt.cd2fd.weighted.adj <- head.wt.cd2fd.cor
head.ht.fd2cd.weighted.adj <- head.ht.fd2cd.cor
head.ht.cd2fd.weighted.adj <- head.ht.cd2fd.cor

body.wt.fd.weighted.adj <- body.wt.fd.cor
body.wt.cd.weighted.adj <- body.wt.cd.cor
body.ht.fd.weighted.adj <- body.ht.fd.cor
body.ht.cd.weighted.adj <- body.ht.cd.cor
body.wt.fd2cd.weighted.adj <- body.wt.fd2cd.cor
body.wt.cd2fd.weighted.adj <- body.wt.cd2fd.cor
body.ht.fd2cd.weighted.adj <- body.ht.fd2cd.cor
body.ht.cd2fd.weighted.adj <- body.ht.cd2fd.cor

head.wt.fd.weighted.adj[head.wt.fd.weighted.adj < 0.8 & head.wt.fd.weighted.adj > -0.8] <- 0
head.wt.cd.weighted.adj[head.wt.cd.weighted.adj < 0.8 & head.wt.cd.weighted.adj > -0.8] <- 0
head.ht.fd.weighted.adj[head.ht.fd.weighted.adj < 0.8 & head.ht.fd.weighted.adj > -0.8] <- 0
head.ht.cd.weighted.adj[head.ht.cd.weighted.adj < 0.8 & head.ht.cd.weighted.adj > -0.8] <- 0
head.wt.fd2cd.weighted.adj[head.wt.fd2cd.weighted.adj < 0.8 & head.wt.fd2cd.weighted.adj > -0.8] <- 0
head.wt.cd2fd.weighted.adj[head.wt.cd2fd.weighted.adj < 0.8 & head.wt.cd2fd.weighted.adj > -0.8] <- 0
head.ht.fd2cd.weighted.adj[head.ht.fd2cd.weighted.adj < 0.8 & head.ht.fd2cd.weighted.adj > -0.8] <- 0
head.ht.cd2fd.weighted.adj[head.ht.cd2fd.weighted.adj < 0.8 & head.ht.cd2fd.weighted.adj > -0.8] <- 0

body.wt.fd.weighted.adj[body.wt.fd.weighted.adj < 0.8 & body.wt.fd.weighted.adj > -0.8] <- 0
body.wt.cd.weighted.adj[body.wt.cd.weighted.adj < 0.8 & body.wt.cd.weighted.adj > -0.8] <- 0
body.ht.fd.weighted.adj[body.ht.fd.weighted.adj < 0.8 & body.ht.fd.weighted.adj > -0.8] <- 0
body.ht.cd.weighted.adj[body.ht.cd.weighted.adj < 0.8 & body.ht.cd.weighted.adj > -0.8] <- 0
body.wt.fd2cd.weighted.adj[body.wt.fd2cd.weighted.adj < 0.8 & body.wt.fd2cd.weighted.adj > -0.8] <- 0
body.wt.cd2fd.weighted.adj[body.wt.cd2fd.weighted.adj < 0.8 & body.wt.cd2fd.weighted.adj > -0.8] <- 0
body.ht.fd2cd.weighted.adj[body.ht.fd2cd.weighted.adj < 0.8 & body.ht.fd2cd.weighted.adj > -0.8] <- 0
body.ht.cd2fd.weighted.adj[body.ht.cd2fd.weighted.adj < 0.8 & body.ht.cd2fd.weighted.adj > -0.8] <- 0

## Create graphs from adjacency matrices
head.wt.fd.g <- graph.adjacency(head.wt.fd.adj, mode = "undirected", weighted = NULL)
head.wt.cd.g <- graph.adjacency(head.wt.cd.adj, mode = "undirected", weighted = NULL)
head.ht.fd.g <- graph.adjacency(head.ht.fd.adj, mode = "undirected", weighted = NULL)
head.ht.cd.g <- graph.adjacency(head.ht.cd.adj, mode = "undirected", weighted = NULL)
head.wt.fd2cd.g <- graph.adjacency(head.wt.fd2cd.adj, mode = "undirected", weighted = NULL)
head.wt.cd2fd.g <- graph.adjacency(head.wt.cd2fd.adj, mode = "undirected", weighted = NULL)
head.ht.fd2cd.g <- graph.adjacency(head.ht.fd2cd.adj, mode = "undirected", weighted = NULL)
head.ht.cd2fd.g <- graph.adjacency(head.ht.cd2fd.adj, mode = "undirected", weighted = NULL)

body.wt.fd.g <- graph.adjacency(body.wt.fd.adj, mode = "undirected", weighted = NULL)
body.wt.cd.g <- graph.adjacency(body.wt.cd.adj, mode = "undirected", weighted = NULL)
body.ht.fd.g <- graph.adjacency(body.ht.fd.adj, mode = "undirected", weighted = NULL)
body.ht.cd.g <- graph.adjacency(body.ht.cd.adj, mode = "undirected", weighted = NULL)
body.wt.fd2cd.g <- graph.adjacency(body.wt.fd2cd.adj, mode = "undirected", weighted = NULL)
body.wt.cd2fd.g <- graph.adjacency(body.wt.cd2fd.adj, mode = "undirected", weighted = NULL)
body.ht.fd2cd.g <- graph.adjacency(body.ht.fd2cd.adj, mode = "undirected", weighted = NULL)
body.ht.cd2fd.g <- graph.adjacency(body.ht.cd2fd.adj, mode = "undirected", weighted = NULL)

head.wt.fd.weighted.g <- graph.adjacency(head.wt.fd.weighted.adj, mode = "undirected", weighted = T)
head.wt.cd.weighted.g <- graph.adjacency(head.wt.cd.weighted.adj, mode = "undirected", weighted = T)
head.ht.fd.weighted.g <- graph.adjacency(head.ht.fd.weighted.adj, mode = "undirected", weighted = T)
head.ht.cd.weighted.g <- graph.adjacency(head.ht.cd.weighted.adj, mode = "undirected", weighted = T)
head.wt.fd2cd.weighted.g <- graph.adjacency(head.wt.fd2cd.weighted.adj, mode = "undirected", weighted = T)
head.wt.cd2fd.weighted.g <- graph.adjacency(head.wt.cd2fd.weighted.adj, mode = "undirected", weighted = T)
head.ht.fd2cd.weighted.g <- graph.adjacency(head.ht.fd2cd.weighted.adj, mode = "undirected", weighted = T)
head.ht.cd2fd.weighted.g <- graph.adjacency(head.ht.cd2fd.weighted.adj, mode = "undirected", weighted = T)

body.wt.fd.weighted.g <- graph.adjacency(body.wt.fd.weighted.adj, mode = "undirected", weighted = T)
body.wt.cd.weighted.g <- graph.adjacency(body.wt.cd.weighted.adj, mode = "undirected", weighted = T)
body.ht.fd.weighted.g <- graph.adjacency(body.ht.fd.weighted.adj, mode = "undirected", weighted = T)
body.ht.cd.weighted.g <- graph.adjacency(body.ht.cd.weighted.adj, mode = "undirected", weighted = T)
body.wt.fd2cd.weighted.g <- graph.adjacency(body.wt.fd2cd.weighted.adj, mode = "undirected", weighted = T)
body.wt.cd2fd.weighted.g <- graph.adjacency(body.wt.cd2fd.weighted.adj, mode = "undirected", weighted = T)
body.ht.fd2cd.weighted.g <- graph.adjacency(body.ht.fd2cd.weighted.adj, mode = "undirected", weighted = T)
body.ht.cd2fd.weighted.g <- graph.adjacency(body.ht.cd2fd.weighted.adj, mode = "undirected", weighted = T)

ecount(head.wt.fd.g)
ecount(head.wt.cd.g)
ecount(head.ht.fd.g)
ecount(head.ht.cd.g)

vcount(head.wt.fd.g)
nrow(combinations(103,2))
fisher.test(matrix(c(732, 402, 567, 567), nrow = 2))
fisher.test(matrix(c(604, 802, 703, 703), nrow = 2))
fisher.test(matrix(c(604, 802, 732, 402), nrow = 2))

ecount(body.wt.fd.g)
ecount(body.wt.cd.g)
ecount(body.ht.fd.g)
ecount(body.ht.cd.g)

vcount(body.wt.fd.g)
nrow(combinations(125,2))
fisher.test(matrix(c(1317, 945, 1131, 1131), nrow = 2))
fisher.test(matrix(c(1608, 1306, 1457, 1457), nrow = 2))
fisher.test(matrix(c(1608, 1306, 1317, 945), nrow = 2))

# Network Visualization ---------------------------------------------------
par(mfrow=c(2, 4), mar = c(1, 1, 1, 1) + 0.1, mgp = c(2, 0, 0) + 0.5)
plot(head.wt.fd.g, layout = layout.fruchterman.reingold, main = "Head / w1118 / FD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)
plot(head.wt.cd.g, layout = layout.fruchterman.reingold, main = "Head / w1118 / CD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)
plot(head.ht.fd.g, layout = layout.fruchterman.reingold, main = "Head / 5-HT2A / FD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)
plot(head.ht.cd.g, layout = layout.fruchterman.reingold, main = "Head / 5-HT2A / CD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)

plot(body.wt.fd.g, layout = layout.fruchterman.reingold, main = "Body / w1118 / FD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)
plot(body.wt.cd.g, layout = layout.fruchterman.reingold, main = "Body / w1118 / CD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)
plot(body.ht.fd.g, layout = layout.fruchterman.reingold, main = "Body / 5-HT2A / FD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)
plot(body.ht.cd.g, layout = layout.fruchterman.reingold, main = "Body / 5-HT2A / CD",
     vertex.color = "tomato", vertex.frame.color = "tomato", vertex.size = 15, 
     vertex.label.family = "Helvetica", vertex.color = "grey27", vertex.label.cex = 0.5)

# Extract Subnetwork ------------------------------------------------------
clusters(head.wt.fd.g)
clusters(head.wt.cd.g)
clusters(head.ht.fd.g)
clusters(head.ht.cd.g)
clusters(body.wt.fd.g)
clusters(body.wt.cd.g)
clusters(body.ht.fd.g)
clusters(body.ht.cd.g)

head.wt.fd.gc <- decompose.graph(head.wt.fd.g)[[order(sapply(decompose.graph(head.wt.fd.g), vcount), decreasing = 1)[1]]]
head.wt.cd.gc <- decompose.graph(head.wt.cd.g)[[order(sapply(decompose.graph(head.wt.cd.g), vcount), decreasing = 1)[1]]]
head.ht.fd.gc <- decompose.graph(head.ht.fd.g)[[order(sapply(decompose.graph(head.ht.fd.g), vcount), decreasing = 1)[1]]]
head.ht.cd.gc <- decompose.graph(head.ht.cd.g)[[order(sapply(decompose.graph(head.ht.cd.g), vcount), decreasing = 1)[1]]]
body.wt.fd.gc <- decompose.graph(body.wt.fd.g)[[order(sapply(decompose.graph(body.wt.fd.g), vcount), decreasing = 1)[1]]]
body.wt.cd.gc <- decompose.graph(body.wt.cd.g)[[order(sapply(decompose.graph(body.wt.cd.g), vcount), decreasing = 1)[1]]]
body.ht.fd.gc <- decompose.graph(body.ht.fd.g)[[order(sapply(decompose.graph(body.ht.fd.g), vcount), decreasing = 1)[1]]]
body.ht.cd.gc <- decompose.graph(body.ht.cd.g)[[order(sapply(decompose.graph(body.ht.cd.g), vcount), decreasing = 1)[1]]]

table(sapply(decompose.graph(head.wt.fd.g), vcount))
table(sapply(decompose.graph(head.wt.cd.g), vcount))
table(sapply(decompose.graph(head.ht.fd.g), vcount))
table(sapply(decompose.graph(head.ht.cd.g), vcount))
table(sapply(decompose.graph(body.wt.fd.g), vcount))
table(sapply(decompose.graph(body.wt.cd.g), vcount))
table(sapply(decompose.graph(body.ht.fd.g), vcount))
table(sapply(decompose.graph(body.ht.cd.g), vcount))

vcount(head.wt.fd.gc)
vcount(head.wt.cd.gc)
vcount(head.ht.fd.gc)
vcount(head.ht.cd.gc)
vcount(body.wt.fd.gc)
vcount(body.wt.cd.gc)
vcount(body.ht.fd.gc)
vcount(body.ht.cd.gc)

ecount(head.wt.fd.gc)/ecount(head.wt.fd.g)
ecount(head.wt.cd.gc)/ecount(head.wt.cd.g)
ecount(head.ht.fd.gc)/ecount(head.ht.fd.g)
ecount(head.ht.cd.gc)/ecount(head.ht.cd.g)

ecount(body.wt.fd.gc)/ecount(body.wt.fd.g)
ecount(body.wt.cd.gc)/ecount(body.wt.cd.g)
ecount(body.ht.fd.gc)/ecount(body.ht.fd.g)
ecount(body.ht.cd.gc)/ecount(body.ht.cd.g)

head.lt <- list(head.wt.fd = V(head.wt.fd.gc)$name,
                head.wt.cd = V(head.wt.cd.gc)$name,
                head.ht.fd = V(head.ht.fd.gc)$name,
                head.ht.cd = V(head.ht.cd.gc)$name)
                
                
body.lt <- list(body.wt.fd = V(body.wt.fd.gc)$name,
                body.wt.cd = V(body.wt.cd.gc)$name,
                body.ht.fd = V(body.ht.fd.gc)$name,
                body.ht.cd = V(body.ht.cd.gc)$name)

upset(fromList(head.lt), 
      sets = c("head.wt.fd", "head.wt.cd", "head.ht.fd", "head.ht.cd"), 
      sets.x.label = "Core Size",
      mainbar.y.label = "Metabolite Intersections",
      order.by = "freq")

upset(fromList(body.lt), 
      sets = c("body.wt.fd", "body.wt.cd", "body.ht.fd", "body.ht.cd"), 
      sets.x.label = "Core Size",
      mainbar.y.label = "Metabolite Intersections",
      order.by = "freq")

# Data Output -------------------------------------------------------------
write(V(head.wt.fd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-head.wt.fd.gc.txt")
write(V(head.wt.cd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-head.wt.cd.gc.txt")
write(V(head.ht.fd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-head.ht.fd.gc.txt")
write(V(head.ht.cd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-head.ht.cd.gc.txt")
write(V(body.wt.fd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-body.wt.fd.gc.txt")
write(V(body.wt.cd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-body.wt.cd.gc.txt")
write(V(body.ht.fd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-body.ht.fd.gc.txt")
write(V(body.ht.cd.gc)$name, "Script/07_NetworkAnalysis/01_NetworkConstruction/List-body.ht.cd.gc.txt")

save(head.wt.fd.cor, head.wt.cd.cor, head.ht.fd.cor, head.ht.cd.cor,
     body.wt.fd.cor, body.wt.cd.cor, body.ht.fd.cor, body.ht.cd.cor,
     head.wt.fd2cd.cor, head.wt.cd2fd.cor, head.ht.fd2cd.cor, head.ht.cd2fd.cor,
     body.wt.fd2cd.cor, body.wt.cd2fd.cor, body.ht.fd2cd.cor, body.ht.cd2fd.cor,
     head.wt.fd.cor.lwr, head.wt.cd.cor.lwr, head.ht.fd.cor.lwr, head.ht.cd.cor.lwr,
     body.wt.fd.cor.lwr, body.wt.cd.cor.lwr, body.ht.fd.cor.lwr, body.ht.cd.cor.lwr,
     head.wt.fd2cd.cor.lwr, head.wt.cd2fd.cor.lwr, head.ht.fd2cd.cor.lwr, head.ht.cd2fd.cor.lwr,
     body.wt.fd2cd.cor.lwr, body.wt.cd2fd.cor.lwr, body.ht.fd2cd.cor.lwr, body.ht.cd2fd.cor.lwr,
     head.wt.fd.adj, head.wt.cd.adj, head.ht.fd.adj, head.ht.cd.adj,
     body.wt.fd.adj, body.wt.cd.adj, body.ht.fd.adj, body.ht.cd.adj,
     head.wt.fd2cd.adj, head.wt.cd2fd.adj, head.ht.fd2cd.adj, head.ht.cd2fd.adj,
     body.wt.fd2cd.adj, body.wt.cd2fd.adj, body.ht.fd2cd.adj, body.ht.cd2fd.adj,
     head.wt.fd.g, head.wt.cd.g, head.ht.fd.g, head.ht.cd.g,
     body.wt.fd.g, body.wt.cd.g, body.ht.fd.g, body.ht.cd.g,
     head.wt.fd2cd.g, head.wt.cd2fd.g, head.ht.fd2cd.g, head.ht.cd2fd.g,
     body.wt.fd2cd.g, body.wt.cd2fd.g, body.ht.fd2cd.g, body.ht.cd2fd.g,
     head.wt.fd.weighted.g, head.wt.cd.weighted.g, head.ht.fd.weighted.g, head.ht.cd.weighted.g,
     body.wt.fd.weighted.g, body.wt.cd.weighted.g, body.ht.fd.weighted.g, body.ht.cd.weighted.g,
     head.wt.fd2cd.weighted.g, head.wt.cd2fd.weighted.g, head.ht.fd2cd.weighted.g, head.ht.cd2fd.weighted.g,
     body.wt.fd2cd.weighted.g, body.wt.cd2fd.weighted.g, body.ht.fd2cd.weighted.g, body.ht.cd2fd.weighted.g,
     head.wt.fd.gc, head.wt.cd.gc, head.ht.fd.gc, head.ht.cd.gc,
     body.wt.fd.gc, body.wt.cd.gc, body.ht.fd.gc, body.ht.cd.gc,
     file = "Data/ProcessedData/NetworkObject-FlyChoiceDiet.Rdata")
