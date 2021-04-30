## Subject: Network Analysis - Network Modularity
## Project: Fly Choice Diet
## Part: 7-3
## Author: Yang Lyu
## Date created: 10/01/2019
## Date modified:04/09/2021

# Environment Settings ----------------------------------------------------
library(igraph)
library(pheatmap)
library(MetaboAnalystR)

# Data Input --------------------------------------------------------------
## Read data
load("Data/ProcessedData/NetworkObject-FlyChoiceDiet.Rdata")

# Modularity Analysis -----------------------------------------------------
## Identify modules
clst.head.wt.fd <- cluster_leading_eigen(head.wt.fd.g)
clst.head.wt.cd <- cluster_leading_eigen(head.wt.cd.g)
clst.head.wt.fd2cd <- cluster_leading_eigen(head.wt.fd2cd.g)
clst.head.wt.cd2fd <- cluster_leading_eigen(head.wt.cd2fd.g)
clst.head.ht.fd <- cluster_leading_eigen(head.ht.fd.g)
clst.head.ht.cd <- cluster_leading_eigen(head.ht.cd.g)

clst.body.wt.fd <- cluster_leading_eigen(body.wt.fd.g)
clst.body.wt.cd <- cluster_leading_eigen(body.wt.cd.g)
clst.body.wt.fd2cd <- cluster_leading_eigen(body.wt.fd2cd.g)
clst.body.wt.cd2fd <- cluster_leading_eigen(body.wt.cd2fd.g)
clst.body.ht.fd <- cluster_leading_eigen(body.ht.fd.g)
clst.body.ht.cd <- cluster_leading_eigen(body.wt.cd.g)

modularity(clst.head.wt.fd)
modularity(clst.head.wt.cd)
modularity(clst.head.wt.fd2cd)
modularity(clst.head.wt.cd2fd)
modularity(clst.head.ht.fd)
modularity(clst.head.ht.cd)

modularity(clst.body.wt.fd)
modularity(clst.body.wt.cd)
modularity(clst.body.wt.fd2cd)
modularity(clst.body.wt.cd2fd)
modularity(clst.body.ht.fd)
modularity(clst.body.ht.cd)

sizes(clst.head.wt.fd)
sizes(clst.head.wt.cd)
sizes(clst.head.wt.fd2cd)
sizes(clst.head.wt.cd2fd)
sizes(clst.head.ht.fd)
sizes(clst.head.ht.cd)

sizes(clst.body.wt.fd)
sizes(clst.body.wt.cd)
sizes(clst.body.wt.fd2cd)
sizes(clst.body.wt.cd2fd)
sizes(clst.body.ht.fd)
sizes(clst.body.ht.cd)

head.wt.fd.membership <- membership(clst.head.wt.fd)
head.wt.cd.membership <- membership(clst.head.wt.cd)
head.wt.fd2cd.membership <- membership(clst.head.wt.fd2cd)
head.wt.cd2fd.membership <- membership(clst.head.wt.cd2fd)
head.ht.fd.membership <- membership(clst.head.ht.fd)
head.ht.cd.membership <- membership(clst.head.ht.cd)

body.wt.fd.membership <- membership(clst.body.wt.fd)
body.wt.cd.membership <- membership(clst.body.wt.cd)
body.wt.fd2cd.membership <- membership(clst.body.wt.fd2cd)
body.wt.cd2fd.membership <- membership(clst.body.wt.cd2fd)
body.ht.fd.membership <- membership(clst.body.ht.fd)
body.ht.cd.membership <- membership(clst.body.ht.cd)

head.wt.fd.order <- names(head.wt.fd.membership[order(head.wt.fd.membership)])
head.wt.cd.order <- names(head.wt.cd.membership[order(head.wt.cd.membership)])
head.wt.fd2cd.order <- names(head.wt.fd.membership[order(head.wt.fd2cd.membership)])
head.wt.cd2fd.order <- names(head.wt.cd.membership[order(head.wt.cd2fd.membership)])
head.ht.fd.order <- names(head.ht.fd.membership[order(head.ht.fd.membership)])
head.ht.cd.order <- names(head.ht.cd.membership[order(head.ht.cd.membership)])

body.wt.fd.order <- names(body.wt.fd.membership[order(body.wt.fd.membership)])
body.wt.cd.order <- names(body.wt.cd.membership[order(body.wt.cd.membership)])
body.wt.fd2cd.order <- names(body.wt.fd.membership[order(body.wt.fd2cd.membership)])
body.wt.cd2fd.order <- names(body.wt.cd.membership[order(body.wt.cd2fd.membership)])
body.ht.fd.order <- names(body.ht.fd.membership[order(body.ht.fd.membership)])
body.ht.cd.order <- names(body.ht.cd.membership[order(body.ht.cd.membership)])

head.wt.fd.module <- data.frame(Module = head.wt.fd.membership[order(head.wt.fd.membership)])
head.wt.cd.module <- data.frame(Module = head.wt.cd.membership[order(head.wt.cd.membership)])
head.wt.fd2cd.module <- data.frame(Module = head.wt.fd2cd.membership[order(head.wt.fd2cd.membership)])
head.wt.cd2fd.module <- data.frame(Module = head.wt.cd2fd.membership[order(head.wt.cd2fd.membership)])
head.ht.fd.module <- data.frame(Module = head.ht.fd.membership[order(head.ht.fd.membership)])
head.ht.cd.module <- data.frame(Module = head.ht.cd.membership[order(head.ht.cd.membership)])

body.wt.fd.module <- data.frame(Module = body.wt.fd.membership[order(body.wt.fd.membership)])
body.wt.cd.module <- data.frame(Module = body.wt.cd.membership[order(body.wt.cd.membership)])
body.wt.fd2cd.module <- data.frame(Module = body.wt.fd2cd.membership[order(body.wt.fd2cd.membership)])
body.wt.cd2fd.module <- data.frame(Module = body.wt.cd2fd.membership[order(body.wt.cd2fd.membership)])
body.ht.fd.module <- data.frame(Module = body.ht.fd.membership[order(body.ht.fd.membership)])
body.ht.cd.module <- data.frame(Module = body.ht.cd.membership[order(body.ht.cd.membership)])

row.names(head.wt.fd.module) <- head.wt.fd.order
row.names(head.wt.cd.module) <- head.wt.cd.order
row.names(head.wt.fd2cd.module) <- head.wt.fd2cd.order
row.names(head.wt.cd2fd.module) <- head.wt.cd2fd.order
row.names(head.ht.fd.module) <- head.ht.fd.order
row.names(head.ht.cd.module) <- head.ht.cd.order

row.names(body.wt.fd.module) <- body.wt.fd.order
row.names(body.wt.cd.module) <- body.wt.cd.order
row.names(body.wt.fd2cd.module) <- body.wt.fd2cd.order
row.names(body.wt.cd2fd.module) <- body.wt.cd2fd.order
row.names(body.ht.fd.module) <- body.ht.fd.order
row.names(body.ht.cd.module) <- body.ht.cd.order

head.wt.fd.module$Module <- factor(head.wt.fd.module$Module)
head.wt.cd.module$Module <- factor(head.wt.cd.module$Module)
head.wt.fd2cd.module$Module <- factor(head.wt.fd2cd.module$Module)
head.wt.cd2fd.module$Module <- factor(head.wt.cd2fd.module$Module)
head.ht.fd.module$Module <- factor(head.ht.fd.module$Module)
head.ht.cd.module$Module <- factor(head.ht.cd.module$Module)

body.wt.fd.module$Module <- factor(body.wt.fd.module$Module)
body.wt.cd.module$Module <- factor(body.wt.cd.module$Module)
body.wt.fd2cd.module$Module <- factor(body.wt.fd2cd.module$Module)
body.wt.cd2fd.module$Module <- factor(body.wt.cd2fd.module$Module)
body.ht.fd.module$Module <- factor(body.ht.fd.module$Module)
body.ht.cd.module$Module <- factor(body.ht.cd.module$Module)

macolor <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                                  "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                                  "#4393C3", "#2166AC", "#053061")))(100)

diag(head.wt.fd.cor) <- 1
diag(head.wt.cd.cor) <- 1
diag(head.wt.fd2cd.cor) <- 1
diag(head.wt.cd2fd.cor) <- 1
diag(head.ht.fd.cor) <- 1
diag(head.ht.cd.cor) <- 1

diag(body.wt.fd.cor) <- 1
diag(body.wt.cd.cor) <- 1
diag(body.wt.fd2cd.cor) <- 1
diag(body.wt.cd2fd.cor) <- 1
diag(body.ht.fd.cor) <- 1
diag(body.ht.cd.cor) <- 1

### Head
pheatmap(head.wt.fd.cor[head.wt.fd.order, head.wt.fd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = head.wt.fd.module, annotation_col = head.wt.fd.module)

pheatmap(head.wt.cd.cor[head.wt.cd.order, head.wt.cd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = head.wt.cd.module, annotation_col = head.wt.cd.module)

pheatmap(head.wt.fd2cd.cor[head.wt.fd2cd.order, head.wt.fd2cd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = head.wt.fd2cd.module, annotation_col = head.wt.fd2cd.module)

pheatmap(head.wt.cd2fd.cor[head.wt.cd2fd.order, head.wt.cd2fd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = head.wt.cd2fd.module, annotation_col = head.wt.cd2fd.module)

pheatmap(head.ht.fd.cor[head.ht.fd.order, head.ht.fd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = head.ht.fd.module, annotation_col = head.ht.fd.module)

pheatmap(head.ht.cd.cor[head.ht.cd.order, head.ht.cd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = head.ht.cd.module, annotation_col = head.ht.cd.module)

### Body
pheatmap(body.wt.fd.cor[body.wt.fd.order, body.wt.fd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = body.wt.fd.module, annotation_col = body.wt.fd.module)

pheatmap(body.wt.cd.cor[body.wt.cd.order, body.wt.cd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = body.wt.cd.module, annotation_col = body.wt.cd.module)

pheatmap(body.wt.fd2cd.cor[body.wt.fd2cd.order, body.wt.fd2cd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = body.wt.fd2cd.module, annotation_col = body.wt.fd2cd.module)

pheatmap(body.wt.cd2fd.cor[body.wt.cd2fd.order, body.wt.cd2fd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = body.wt.cd2fd.module, annotation_col = body.wt.cd2fd.module)

pheatmap(body.ht.fd.cor[body.ht.fd.order, body.ht.fd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = body.ht.fd.module, annotation_col = body.ht.fd.module)

pheatmap(body.ht.cd.cor[body.ht.cd.order, body.ht.cd.order], color = macolor, border_color = NA,
         cluster_rows = FALSE, cluster_cols = FALSE, labels_row = "", labels_col = "",
         annotation_row = body.ht.cd.module, annotation_col = body.ht.cd.module)

## Plot sub module
head.wt.fd.mod.names <- head.wt.fd.order[52:95]

write(head.wt.fd.mod.names, 
      file = "Script/07_NetworkAnalysis/03_NetworkModularity/List-ModNames_Head_wt_fd.txt")

head.wt.fd.mod <- induced.subgraph(head.wt.fd.weighted.g, head.wt.fd.mod.names)
head.wt.cd.mod <- induced.subgraph(head.wt.cd.weighted.g, head.wt.fd.mod.names)
head.ht.fd.mod <- induced.subgraph(head.ht.fd.weighted.g, head.wt.fd.mod.names)
head.ht.cd.mod <- induced.subgraph(head.ht.cd.weighted.g, head.wt.fd.mod.names)

head.wt.difference <- graph.difference(head.wt.fd.mod, head.wt.cd.mod)
head.ht.rescue     <- graph.intersection(head.wt.difference, head.ht.cd.mod)

ecount(head.wt.difference) ### 717
ecount(head.ht.rescue) ### 317

sum(sign(edge.attributes(head.ht.rescue)$weight_1) == sign(edge.attributes(head.ht.rescue)$weight_2)) ### 253

head.wt.fd.mod.edge.col <- rep("dark red", ecount(head.wt.fd.mod))
head.wt.cd.mod.edge.col <- rep("dark red", ecount(head.wt.cd.mod))
head.ht.fd.mod.edge.col <- rep("dark red", ecount(head.ht.fd.mod))
head.ht.cd.mod.edge.col <- rep("dark red", ecount(head.ht.cd.mod))

head.wt.fd.mod.edge.col[edge.attributes(head.wt.fd.mod)$weight < 0] <- "royalblue"
head.wt.cd.mod.edge.col[edge.attributes(head.wt.cd.mod)$weight < 0] <- "royalblue"
head.ht.fd.mod.edge.col[edge.attributes(head.ht.fd.mod)$weight < 0] <- "royalblue"
head.ht.cd.mod.edge.col[edge.attributes(head.ht.cd.mod)$weight < 0] <- "royalblue"

V(head.wt.fd.mod)$label <- NA
V(head.wt.cd.mod)$label <- NA
V(head.ht.fd.mod)$label <- NA
V(head.ht.cd.mod)$label <- NA

V(head.wt.fd.mod)$color <- "grey50"
V(head.wt.cd.mod)$color <- "grey50"
V(head.ht.fd.mod)$color <- "grey50"
V(head.ht.cd.mod)$color <- "grey50"

par(mfrow = c(1, 4), mar = c(1, 1, 3, 1))
plot(head.wt.fd.mod, edge.color = head.wt.fd.mod.edge.col, 
     main = paste("Head/w1118/FD", paste("N(edge)=", ecount(head.wt.fd.mod), sep = ""), sep ="\n"),
     layout = layout_in_circle, order = head.wt.fd.mod.names)

plot(head.wt.cd.mod, edge.color = head.wt.cd.mod.edge.col,
     main = paste("Head/w1118/CD", paste("N(edge)=", ecount(head.wt.cd.mod), sep = ""), sep ="\n"),
     layout = layout_in_circle, order = head.wt.fd.mod.names)

plot(head.ht.fd.mod, edge.color = head.ht.fd.mod.edge.col,
     main = paste("Head/5-HT2A/FD", paste("N(edge)=", ecount(head.ht.fd.mod), sep = ""), sep ="\n"),
     layout = layout_in_circle, order = head.wt.fd.mod.names)

plot(head.ht.cd.mod, edge.color = head.ht.cd.mod.edge.col,
     main = paste("Head/5-HT2A/CD", paste("N(edge)=", ecount(head.ht.cd.mod), sep = ""), sep ="\n"),
     layout = layout_in_circle, order = head.wt.fd.mod.names)

transitivity(head.wt.fd.gc)
transitivity(head.wt.cd.gc)
transitivity(head.ht.fd.gc)
transitivity(head.ht.cd.gc)
transitivity(body.wt.fd.gc)
transitivity(body.wt.cd.gc)
transitivity(body.ht.fd.gc)
transitivity(body.ht.cd.gc)

# Pathway Analysis --------------------------------------------------------
## Initiate Data Objects
head.wt.fd.mod1.mSet <- InitDataObjects("conc", "pathora", FALSE)

## Set up mSetObj with the list of compounds
head.wt.fd.mod1.mSet <- Setup.MapData(head.wt.fd.mod1.mSet, head.wt.fd.mod.names)

## Cross reference list of compounds against libraries 
head.wt.fd.mod1.mSet <- CrossReferencing(head.wt.fd.mod1.mSet, "name")

## Create the mapping results table
head.wt.fd.mod1.mSet <- CreateMappingResultTable(head.wt.fd.mod1.mSet)

## KEGG Enrichment Test
head.wt.fd.mod1.mSet <- SetKEGG.PathLib(head.wt.fd.mod1.mSet, "dme", "current")

head.wt.fd.mod1.mSet <- SetMetabolomeFilter(head.wt.fd.mod1.mSet, F)

head.wt.fd.mod1.mSet <- CalculateOraScore(head.wt.fd.mod1.mSet, "rbc", "hyperg")
