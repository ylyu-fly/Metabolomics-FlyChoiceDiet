## Subject: Network Analysis - Network Construction
## Project: Fly Choice Diet
## Part: 7-1
## Author: Yang Lyu
## Date created: 09/29/2019
## Date modified: 02/13/2021

# Environment Settings ----------------------------------------------------
library(Hmisc)
library(reshape2)
library(igraph)
library(MetaboAnalystR)
source("Script/07_NetworkAnalysis/01_NetworkConstruction/multiplot.R")
source("Script/07_NetworkAnalysis/01_NetworkConstruction/rcorr.adjust.R")

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

# Normality Test ----------------------------------------------------------
## First, test normality for each metabolite
head.wt.fd.shapiro.pval <- numeric()
head.wt.cd.shapiro.pval <- numeric()
head.ht.fd.shapiro.pval <- numeric()
head.ht.cd.shapiro.pval <- numeric()
head.wt.fd2cd.shapiro.pval <- numeric()
head.wt.cd2fd.shapiro.pval <- numeric()
head.ht.fd2cd.shapiro.pval <- numeric()
head.ht.cd2fd.shapiro.pval <- numeric()

body.wt.fd.shapiro.pval <- numeric()
body.wt.cd.shapiro.pval <- numeric()
body.ht.fd.shapiro.pval <- numeric()
body.ht.cd.shapiro.pval <- numeric()
body.wt.fd2cd.shapiro.pval <- numeric()
body.wt.cd2fd.shapiro.pval <- numeric()
body.ht.fd2cd.shapiro.pval <- numeric()
body.ht.cd2fd.shapiro.pval <- numeric()

for (i in 1:dim(head.wt.fd)[2]) {
   head.wt.fd.shapiro.pval[i] <- shapiro.test(head.wt.fd[, i])$p.value
   head.wt.cd.shapiro.pval[i] <- shapiro.test(head.wt.cd[, i])$p.value
   head.ht.fd.shapiro.pval[i] <- shapiro.test(head.ht.fd[, i])$p.value
   head.ht.cd.shapiro.pval[i] <- shapiro.test(head.ht.cd[, i])$p.value
   
   head.wt.fd2cd.shapiro.pval[i] <- shapiro.test(head.wt.fd2cd[, i])$p.value
   head.wt.cd2fd.shapiro.pval[i] <- shapiro.test(head.wt.cd2fd[, i])$p.value
   head.ht.fd2cd.shapiro.pval[i] <- shapiro.test(head.ht.fd2cd[, i])$p.value
   head.ht.cd2fd.shapiro.pval[i] <- shapiro.test(head.ht.cd2fd[, i])$p.value
}

for (i in 1:dim(body.wt.fd)[2]) {
   body.wt.fd.shapiro.pval[i] <- shapiro.test(body.wt.fd[, i])$p.value
   body.wt.cd.shapiro.pval[i] <- shapiro.test(body.wt.cd[, i])$p.value
   body.ht.fd.shapiro.pval[i] <- shapiro.test(body.ht.fd[, i])$p.value
   body.ht.cd.shapiro.pval[i] <- shapiro.test(body.ht.cd[, i])$p.value
   
   body.wt.fd2cd.shapiro.pval[i] <- shapiro.test(body.wt.fd2cd[, i])$p.value
   body.wt.cd2fd.shapiro.pval[i] <- shapiro.test(body.wt.cd2fd[, i])$p.value
   body.ht.fd2cd.shapiro.pval[i] <- shapiro.test(body.ht.fd2cd[, i])$p.value
   body.ht.cd2fd.shapiro.pval[i] <- shapiro.test(body.ht.cd2fd[, i])$p.value
}

head.wt.fd.shapiro.padj <- p.adjust(head.wt.fd.shapiro.pval, method = "fdr")
head.wt.cd.shapiro.padj <- p.adjust(head.wt.cd.shapiro.pval, method = "fdr")
head.ht.fd.shapiro.padj <- p.adjust(head.ht.fd.shapiro.pval, method = "fdr")
head.ht.cd.shapiro.padj <- p.adjust(head.ht.cd.shapiro.pval, method = "fdr")

head.wt.fd2cd.shapiro.padj <- p.adjust(head.wt.fd2cd.shapiro.pval, method = "fdr")
head.wt.cd2fd.shapiro.padj <- p.adjust(head.wt.cd2fd.shapiro.pval, method = "fdr")
head.ht.fd2cd.shapiro.padj <- p.adjust(head.ht.fd2cd.shapiro.pval, method = "fdr")
head.ht.cd2fd.shapiro.padj <- p.adjust(head.ht.cd2fd.shapiro.pval, method = "fdr")

body.wt.fd.shapiro.padj <- p.adjust(body.wt.fd.shapiro.pval, method = "fdr")
body.wt.cd.shapiro.padj <- p.adjust(body.wt.cd.shapiro.pval, method = "fdr")
body.ht.fd.shapiro.padj <- p.adjust(body.ht.fd.shapiro.pval, method = "fdr")
body.ht.cd.shapiro.padj <- p.adjust(body.ht.cd.shapiro.pval, method = "fdr")

body.wt.fd2cd.shapiro.padj <- p.adjust(body.wt.fd2cd.shapiro.pval, method = "fdr")
body.wt.cd2fd.shapiro.padj <- p.adjust(body.wt.cd2fd.shapiro.pval, method = "fdr")
body.ht.fd2cd.shapiro.padj <- p.adjust(body.ht.fd2cd.shapiro.pval, method = "fdr")
body.ht.cd2fd.shapiro.padj <- p.adjust(body.ht.cd2fd.shapiro.pval, method = "fdr")

head.wt.fd.shapiro.padj < 0.05
head.wt.cd.shapiro.padj < 0.05
head.ht.fd.shapiro.padj < 0.05
head.ht.cd.shapiro.padj < 0.05

head.wt.fd2cd.shapiro.padj < 0.05
head.wt.cd2fd.shapiro.padj < 0.05
head.ht.fd2cd.shapiro.padj < 0.05
head.ht.cd2fd.shapiro.padj < 0.05

body.wt.fd.shapiro.padj < 0.05
body.wt.cd.shapiro.padj < 0.05
body.ht.fd.shapiro.padj < 0.05
body.ht.cd.shapiro.padj < 0.05

body.wt.fd2cd.shapiro.padj < 0.05
body.wt.cd2fd.shapiro.padj < 0.05
body.ht.fd2cd.shapiro.padj < 0.05
body.ht.cd2fd.shapiro.padj < 0.05

### There are a few metabolites failed normality test
### We will use spearman rank correlation coefficient

# Correlation Coefficient Analysis ----------------------------------------
## Spearman rank correlation test and FDR correction
head.wt.fd.test <- rcorr.adjust(head.wt.fd, type = "spearman")
head.wt.cd.test <- rcorr.adjust(head.wt.cd, type = "spearman")
head.ht.fd.test <- rcorr.adjust(head.ht.fd, type = "spearman")
head.ht.cd.test <- rcorr.adjust(head.ht.cd, type = "spearman")
head.wt.fd2cd.test <- rcorr.adjust(head.wt.fd2cd, type = "spearman")
head.wt.cd2fd.test <- rcorr.adjust(head.wt.cd2fd, type = "spearman")
head.ht.fd2cd.test <- rcorr.adjust(head.ht.fd2cd, type = "spearman")
head.ht.cd2fd.test <- rcorr.adjust(head.ht.cd2fd, type = "spearman")

body.wt.fd.test <- rcorr.adjust(body.wt.fd, type = "spearman")
body.wt.cd.test <- rcorr.adjust(body.wt.cd, type = "spearman")
body.ht.fd.test <- rcorr.adjust(body.ht.fd, type = "spearman")
body.ht.cd.test <- rcorr.adjust(body.ht.cd, type = "spearman")
body.wt.fd2cd.test <- rcorr.adjust(body.wt.fd2cd, type = "spearman")
body.wt.cd2fd.test <- rcorr.adjust(body.wt.cd2fd, type = "spearman")
body.ht.fd2cd.test <- rcorr.adjust(body.ht.fd2cd, type = "spearman")
body.ht.cd2fd.test <- rcorr.adjust(body.ht.cd2fd, type = "spearman")

head.wt.fd.cor  <- head.wt.fd.test$R$r
head.wt.cd.cor  <- head.wt.cd.test$R$r
head.ht.fd.cor  <- head.ht.fd.test$R$r
head.ht.cd.cor  <- head.ht.cd.test$R$r
head.wt.fd2cd.cor  <- head.wt.fd2cd.test$R$r
head.wt.cd2fd.cor  <- head.wt.cd2fd.test$R$r
head.ht.fd2cd.cor  <- head.ht.fd2cd.test$R$r
head.ht.cd2fd.cor  <- head.ht.cd2fd.test$R$r

body.wt.fd.cor  <- body.wt.fd.test$R$r
body.wt.cd.cor  <- body.wt.cd.test$R$r
body.ht.fd.cor  <- body.ht.fd.test$R$r
body.ht.cd.cor  <- body.ht.cd.test$R$r
body.wt.fd2cd.cor  <- body.wt.fd2cd.test$R$r
body.wt.cd2fd.cor  <- body.wt.cd2fd.test$R$r
body.ht.fd2cd.cor  <- body.ht.fd2cd.test$R$r
body.ht.cd2fd.cor  <- body.ht.cd2fd.test$R$r

head.wt.fd.padj <- head.wt.fd.test$P
head.wt.cd.padj <- head.wt.cd.test$P
head.ht.fd.padj <- head.ht.fd.test$P
head.ht.cd.padj <- head.ht.cd.test$P
head.wt.fd2cd.padj <- head.wt.fd2cd.test$P
head.wt.cd2fd.padj <- head.wt.cd2fd.test$P
head.ht.fd2cd.padj <- head.ht.fd2cd.test$P
head.ht.cd2fd.padj <- head.ht.cd2fd.test$P

body.wt.fd.padj <- body.wt.fd.test$P
body.wt.cd.padj <- body.wt.cd.test$P
body.ht.fd.padj <- body.ht.fd.test$P
body.ht.cd.padj <- body.ht.cd.test$P
body.wt.fd2cd.padj <- body.wt.fd2cd.test$P
body.wt.cd2fd.padj <- body.wt.cd2fd.test$P
body.ht.fd2cd.padj <- body.ht.fd2cd.test$P
body.ht.cd2fd.padj <- body.ht.cd2fd.test$P

## Plot the distribution of correlation coefficient
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

head.wt.cor.df <- data.frame(Cor = c(head.wt.fd.cor.lwr[lower.tri(head.wt.fd.cor.lwr)],
                                     head.wt.cd.cor.lwr[lower.tri(head.wt.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(head.wt.fd.cor.lwr[lower.tri(head.wt.fd.cor.lwr)]))))

head.wt.tr1.cor.df <- data.frame(Cor = c(head.wt.fd2cd.cor.lwr[lower.tri(head.wt.fd2cd.cor.lwr)],
                                         head.wt.cd.cor.lwr[lower.tri(head.wt.cd.cor.lwr)]), 
                                 Diet = factor(rep(c("FD2CD", "CD"), 
                                                  each = length(head.wt.fd2cd.cor.lwr[lower.tri(head.wt.fd2cd.cor.lwr)]))))

head.wt.tr2.cor.df <- data.frame(Cor = c(head.wt.cd2fd.cor.lwr[lower.tri(head.wt.cd2fd.cor.lwr)],
                                         head.wt.fd.cor.lwr[lower.tri(head.wt.fd.cor.lwr)]), 
                                 Diet = factor(rep(c("CD2FD", "FD"), 
                                                   each = length(head.wt.cd2fd.cor.lwr[lower.tri(head.wt.cd2fd.cor.lwr)]))))

head.ht.cor.df <- data.frame(Cor = c(head.ht.fd.cor.lwr[lower.tri(head.ht.fd.cor.lwr)],
                                     head.ht.cd.cor.lwr[lower.tri(head.ht.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(head.ht.fd.cor.lwr[lower.tri(head.ht.fd.cor.lwr)]))))

body.wt.cor.df <- data.frame(Cor = c(body.wt.fd.cor.lwr[lower.tri(body.wt.fd.cor.lwr)],
                                     body.wt.cd.cor.lwr[lower.tri(body.wt.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(body.wt.fd.cor.lwr[lower.tri(body.wt.fd.cor.lwr)]))))

body.wt.tr1.cor.df <- data.frame(Cor = c(body.wt.fd2cd.cor.lwr[lower.tri(body.wt.fd2cd.cor.lwr)],
                                         body.wt.cd.cor.lwr[lower.tri(body.wt.cd.cor.lwr)]), 
                                 Diet = factor(rep(c("FD2CD", "CD"), 
                                                   each = length(body.wt.fd2cd.cor.lwr[lower.tri(body.wt.fd2cd.cor.lwr)]))))

body.wt.tr2.cor.df <- data.frame(Cor = c(body.wt.cd2fd.cor.lwr[lower.tri(body.wt.cd2fd.cor.lwr)],
                                         body.wt.fd.cor.lwr[lower.tri(body.wt.fd.cor.lwr)]), 
                                 Diet = factor(rep(c("CD2FD", "FD"), 
                                                   each = length(body.wt.cd2fd.cor.lwr[lower.tri(body.wt.cd2fd.cor.lwr)]))))

body.ht.cor.df <- data.frame(Cor = c(body.ht.fd.cor.lwr[lower.tri(body.ht.fd.cor.lwr)],
                                     body.ht.cd.cor.lwr[lower.tri(body.ht.cd.cor.lwr)]), 
                             Diet = factor(rep(c("FD", "CD"), 
                                               each = length(body.ht.fd.cor.lwr[lower.tri(body.ht.fd.cor.lwr)]))))

## Plot the distribution of correlation coefficient
cor.p1 <- ggplot2.histogram(data=head.wt.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Head/w1118",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 800)) 


cor.p2 <- ggplot2.histogram(data=head.wt.tr1.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Head/w1118",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 800))

cor.p3 <- ggplot2.histogram(data=head.wt.tr2.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Head/w1118",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 800))

cor.p4 <- ggplot2.histogram(data=head.ht.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Head/5-HT2A",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 800))

cor.p5 <- ggplot2.histogram(data=body.wt.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Body/w1118",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 1200))

cor.p6 <- ggplot2.histogram(data=body.wt.tr1.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Body/w1118",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 1200))

cor.p7 <- ggplot2.histogram(data=body.wt.tr2.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Body/w1118",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 1200))

cor.p8 <- ggplot2.histogram(data=body.ht.cor.df, xName='Cor', groupName='Diet', legendPosition="top", 
                            groupColors=c('deepskyblue', 'gray50'), bins = 10) +
   scale_color_manual(values = c("black", "black", "black")) +
   labs(title = "Body/5-HT2A",x = "Correlation Coefficient", y = "Count") +
   coord_cartesian(xlim = c(-1, 1), ylim = c(0, 1200))

multiplot(cor.p1, cor.p5, cor.p2, cor.p6, cor.p3, cor.p7, cor.p4, cor.p8, cols=4)

head.wt.cor.df$Type      <- "Weakly or not correlated"
head.wt.tr1.cor.df$Type  <- "Weakly or not correlated"
head.wt.tr2.cor.df$Type  <- "Weakly or not correlated"
head.ht.cor.df$Type      <- "Weakly or not correlated"

body.wt.cor.df$Type      <- "Weakly or not correlated"
body.wt.tr1.cor.df$Type  <- "Weakly or not correlated"
body.wt.tr2.cor.df$Type  <- "Weakly or not correlated"
body.ht.cor.df$Type      <- "Weakly or not correlated"

head.wt.cor.df[abs(head.wt.cor.df$Cor) >= 0.8, ]$Type         <- "Strongly correlated"
head.wt.tr1.cor.df[abs(head.wt.tr1.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"
head.wt.tr2.cor.df[abs(head.wt.tr2.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"
head.ht.cor.df[abs(head.ht.cor.df$Cor) >= 0.8, ]$Type         <- "Strongly correlated"

body.wt.cor.df[abs(body.wt.cor.df$Cor) >= 0.8, ]$Type         <- "Strongly correlated"
body.wt.tr1.cor.df[abs(body.wt.tr1.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"
body.wt.tr2.cor.df[abs(body.wt.tr2.cor.df$Cor) >= 0.8, ]$Type <- "Strongly correlated"
body.ht.cor.df[abs(body.ht.cor.df$Cor) >= 0.8, ]$Type         <- "Strongly correlated"

head.wt.type.df <-merge(melt(table(head.wt.cor.df[head.wt.cor.df$Diet == "FD", ]$Type)),
                        melt(table(head.wt.cor.df[head.wt.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)
head.wt.tr1.type.df <-merge(melt(table(head.wt.tr1.cor.df[head.wt.tr1.cor.df$Diet == "FD2CD", ]$Type)),
                           melt(table(head.wt.tr1.cor.df[head.wt.tr1.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)
head.wt.tr2.type.df <-merge(melt(table(head.wt.tr2.cor.df[head.wt.tr2.cor.df$Diet == "CD2FD", ]$Type)),
                           melt(table(head.wt.tr2.cor.df[head.wt.tr2.cor.df$Diet == "FD", ]$Type)), by = "Var1", all = T)
head.ht.type.df <-merge(melt(table(head.ht.cor.df[head.ht.cor.df$Diet == "FD", ]$Type)),
                        melt(table(head.ht.cor.df[head.ht.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)

body.wt.type.df <-merge(melt(table(body.wt.cor.df[body.wt.cor.df$Diet == "FD", ]$Type)),
                        melt(table(body.wt.cor.df[body.wt.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)
body.wt.tr1.type.df <-merge(melt(table(body.wt.tr1.cor.df[body.wt.tr1.cor.df$Diet == "FD2CD", ]$Type)),
                            melt(table(body.wt.tr1.cor.df[body.wt.tr1.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)
body.wt.tr2.type.df <-merge(melt(table(body.wt.tr2.cor.df[body.wt.tr2.cor.df$Diet == "CD2FD", ]$Type)),
                            melt(table(body.wt.tr2.cor.df[body.wt.tr2.cor.df$Diet == "FD", ]$Type)), by = "Var1", all = T)
body.ht.type.df <-merge(melt(table(body.ht.cor.df[body.ht.cor.df$Diet == "FD", ]$Type)),
                        melt(table(body.ht.cor.df[body.ht.cor.df$Diet == "CD", ]$Type)), by = "Var1", all = T)

colnames(head.wt.type.df) <- c("Type", "FD", "CD")
colnames(head.wt.tr1.type.df) <- c("Type", "FD2CD", "CD")
colnames(head.wt.tr2.type.df) <- c("Type", "CD2FD", "FD")
colnames(head.ht.type.df) <- c("Type", "FD", "CD")

colnames(body.wt.type.df) <- c("Type", "FD", "CD")
colnames(body.wt.tr1.type.df) <- c("Type", "FD2CD", "CD")
colnames(body.wt.tr2.type.df) <- c("Type", "CD2FD", "FD")
colnames(body.ht.type.df) <- c("Type", "FD", "CD")

head.wt.type.df <- melt(head.wt.type.df, id = c("Type"))
head.wt.tr1.type.df <- melt(head.wt.tr1.type.df, id = c("Type"))
head.wt.tr2.type.df <- melt(head.wt.tr2.type.df, id = c("Type"))
head.ht.type.df <- melt(head.ht.type.df, id = c("Type"))

body.wt.type.df <- melt(body.wt.type.df, id = c("Type"))
body.wt.tr1.type.df <- melt(body.wt.tr1.type.df, id = c("Type"))
body.wt.tr2.type.df <- melt(body.wt.tr2.type.df, id = c("Type"))
body.ht.type.df <- melt(body.ht.type.df, id = c("Type"))

colnames(head.wt.type.df) <- c("Type", "Diet", "Count")
colnames(head.wt.tr1.type.df) <- c("Type", "Diet", "Count")
colnames(head.wt.tr2.type.df) <- c("Type", "Diet", "Count")
colnames(head.ht.type.df) <- c("Type", "Diet", "Count")

colnames(body.wt.type.df) <- c("Type", "Diet", "Count")
colnames(body.wt.tr1.type.df) <- c("Type", "Diet", "Count")
colnames(body.wt.tr2.type.df) <- c("Type", "Diet", "Count")
colnames(body.ht.type.df) <- c("Type", "Diet", "Count")

head.wt.type.df
head.wt.tr1.type.df
head.wt.tr2.type.df
head.ht.type.df

body.wt.type.df
body.wt.tr1.type.df
body.wt.tr2.type.df
body.ht.type.df

fisher.test(matrix(c(732, 4521, 402, 4851), nrow = 2)) # p < 2.2e-16
fisher.test(matrix(c(514, 4739, 402, 4851), nrow = 2)) # p = 0.0001217
fisher.test(matrix(c(519, 4734, 732, 4521), nrow = 2)) # p = 1.56e-10
fisher.test(matrix(c(802, 4451, 604, 4649), nrow = 2)) # p = 1.577e-08

fisher.test(matrix(c(1317, 6433, 945, 6805), nrow = 2)) # p < 2.2e-16
fisher.test(matrix(c(1003, 6747, 945, 6805), nrow = 2)) # p = 0.1672
fisher.test(matrix(c(1177, 6573, 1317, 6433), nrow = 2)) # p = 0.002373
fisher.test(matrix(c(1608, 6142, 1306, 6444), nrow = 2)) # p = 5.928e-10

## Plot correlation coefficient vs FDR
par(mfrow=c(4, 4), mar = c(4, 4, 1, 1) + 0.1, mgp = c(2, 0, 0) + 0.5)
plot(abs(head.wt.fd.cor[upper.tri(head.wt.fd.cor, diag = FALSE)]), head.wt.fd.padj[upper.tri(head.wt.fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / w1118 / FD")

plot(abs(head.wt.cd.cor[upper.tri(head.wt.cd.cor, diag = FALSE)]), head.wt.cd.padj[upper.tri(head.wt.cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / w1118 / CD")

plot(abs(head.ht.fd.cor[upper.tri(head.ht.fd.cor, diag = FALSE)]), head.ht.fd.padj[upper.tri(head.ht.fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / 5-HT2A / FD")

plot(abs(head.ht.cd.cor[upper.tri(head.ht.cd.cor, diag = FALSE)]), head.ht.cd.padj[upper.tri(head.ht.cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / 5-HT2A / CD")

plot(abs(head.wt.fd2cd.cor[upper.tri(head.wt.fd2cd.cor, diag = FALSE)]), head.wt.fd2cd.padj[upper.tri(head.wt.fd2cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / w1118 / FD2CD")

plot(abs(head.wt.cd2fd.cor[upper.tri(head.wt.cd2fd.cor, diag = FALSE)]), head.wt.cd2fd.padj[upper.tri(head.wt.cd2fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / w1118 / CD2FD")

plot(abs(head.ht.fd2cd.cor[upper.tri(head.ht.fd2cd.cor, diag = FALSE)]), head.ht.fd2cd.padj[upper.tri(head.ht.fd2cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / 5-HT2A / FD2CD")

plot(abs(head.ht.cd2fd.cor[upper.tri(head.ht.cd2fd.cor, diag = FALSE)]), head.ht.cd2fd.padj[upper.tri(head.ht.cd2fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Heads / 5-HT2A / CD2FD")

plot(abs(body.wt.fd.cor[upper.tri(body.wt.fd.cor, diag = FALSE)]), body.wt.fd.padj[upper.tri(body.wt.fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / w1118 / FD")

plot(abs(body.wt.cd.cor[upper.tri(body.wt.cd.cor, diag = FALSE)]), body.wt.cd.padj[upper.tri(body.wt.cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / w1118 / CD")

plot(abs(body.ht.fd.cor[upper.tri(body.ht.fd.cor, diag = FALSE)]), body.ht.fd.padj[upper.tri(body.ht.fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / 5-HT2A / FD")

plot(abs(body.ht.cd.cor[upper.tri(body.ht.cd.cor, diag = FALSE)]), body.ht.cd.padj[upper.tri(body.ht.cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / 5-HT2A / CD")

plot(abs(body.wt.fd2cd.cor[upper.tri(body.wt.fd2cd.cor, diag = FALSE)]), body.wt.fd2cd.padj[upper.tri(body.wt.fd2cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / w1118 / FD2CD")

plot(abs(body.wt.cd2fd.cor[upper.tri(body.wt.cd2fd.cor, diag = FALSE)]), body.wt.cd2fd.padj[upper.tri(body.wt.cd2fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / w1118 / CD2FD")

plot(abs(body.ht.fd2cd.cor[upper.tri(body.ht.fd2cd.cor, diag = FALSE)]), body.ht.fd2cd.padj[upper.tri(body.ht.fd2cd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / 5-HT2A / FD2CD")

plot(abs(body.ht.cd2fd.cor[upper.tri(body.ht.cd2fd.cor, diag = FALSE)]), body.ht.cd2fd.padj[upper.tri(body.ht.cd2fd.padj, diag = FALSE)],
     xlab = "Correlation Coefficient", ylab = "FDR", las = 1, main = "Bodies / 5-HT2A / CD2FD")
### Will try FDR = 0.1 or FDR = 0.05

## Get the rho equivalent of the FDR = 0.1 or FDR = 0.05
head.wt.fd.i <- sum(head.wt.fd.padj[upper.tri(head.wt.fd.padj, diag = FALSE)] <= 0.1)
head.wt.cd.i <- sum(head.wt.cd.padj[upper.tri(head.wt.cd.padj, diag = FALSE)] <= 0.1)
head.ht.fd.i <- sum(head.ht.fd.padj[upper.tri(head.ht.fd.padj, diag = FALSE)] <= 0.1)
head.ht.cd.i <- sum(head.ht.cd.padj[upper.tri(head.ht.cd.padj, diag = FALSE)] <= 0.1)

head.wt.fd2cd.i <- sum(head.wt.fd2cd.padj[upper.tri(head.wt.fd2cd.padj, diag = FALSE)] <= 0.1)
head.wt.cd2fd.i <- sum(head.wt.cd2fd.padj[upper.tri(head.wt.cd2fd.padj, diag = FALSE)] <= 0.1)
head.ht.fd2cd.i <- sum(head.ht.fd2cd.padj[upper.tri(head.ht.fd2cd.padj, diag = FALSE)] <= 0.1)
head.ht.cd2fd.i <- sum(head.ht.cd2fd.padj[upper.tri(head.ht.cd2fd.padj, diag = FALSE)] <= 0.1)

body.wt.fd.i <- sum(body.wt.fd.padj[upper.tri(body.wt.fd.padj, diag = FALSE)] <= 0.1)
body.wt.cd.i <- sum(body.wt.cd.padj[upper.tri(body.wt.cd.padj, diag = FALSE)] <= 0.1)
body.ht.fd.i <- sum(body.ht.fd.padj[upper.tri(body.ht.fd.padj, diag = FALSE)] <= 0.1)
body.ht.cd.i <- sum(body.ht.cd.padj[upper.tri(body.ht.cd.padj, diag = FALSE)] <= 0.1)

body.wt.fd2cd.i <- sum(body.wt.fd2cd.padj[upper.tri(body.wt.fd2cd.padj, diag = FALSE)] <= 0.1)
body.wt.cd2fd.i <- sum(body.wt.cd2fd.padj[upper.tri(body.wt.cd2fd.padj, diag = FALSE)] <= 0.1)
body.ht.fd2cd.i <- sum(body.ht.fd2cd.padj[upper.tri(body.ht.fd2cd.padj, diag = FALSE)] <= 0.1)
body.ht.cd2fd.i <- sum(body.ht.cd2fd.padj[upper.tri(body.ht.cd2fd.padj, diag = FALSE)] <= 0.1)

head.wt.fd.j <- sum(head.wt.fd.padj[upper.tri(head.wt.fd.padj, diag = FALSE)] <= 0.05)
head.wt.cd.j <- sum(head.wt.cd.padj[upper.tri(head.wt.cd.padj, diag = FALSE)] <= 0.05)
head.ht.fd.j <- sum(head.ht.fd.padj[upper.tri(head.ht.fd.padj, diag = FALSE)] <= 0.05)
head.ht.cd.j <- sum(head.ht.cd.padj[upper.tri(head.ht.cd.padj, diag = FALSE)] <= 0.05)

head.wt.fd2cd.j <- sum(head.wt.fd2cd.padj[upper.tri(head.wt.fd2cd.padj, diag = FALSE)] <= 0.05)
head.wt.cd2fd.j <- sum(head.wt.cd2fd.padj[upper.tri(head.wt.cd2fd.padj, diag = FALSE)] <= 0.05)
head.ht.fd2cd.j <- sum(head.ht.fd2cd.padj[upper.tri(head.ht.fd2cd.padj, diag = FALSE)] <= 0.05)
head.ht.cd2fd.j <- sum(head.ht.cd2fd.padj[upper.tri(head.ht.cd2fd.padj, diag = FALSE)] <= 0.05)

body.wt.fd.j <- sum(body.wt.fd.padj[upper.tri(body.wt.fd.padj, diag = FALSE)] <= 0.05)
body.wt.cd.j <- sum(body.wt.cd.padj[upper.tri(body.wt.cd.padj, diag = FALSE)] <= 0.05)
body.ht.fd.j <- sum(body.ht.fd.padj[upper.tri(body.ht.fd.padj, diag = FALSE)] <= 0.05)
body.ht.cd.j <- sum(body.ht.cd.padj[upper.tri(body.ht.cd.padj, diag = FALSE)] <= 0.05)

body.wt.fd2cd.j <- sum(body.wt.fd2cd.padj[upper.tri(body.wt.fd2cd.padj, diag = FALSE)] <= 0.05)
body.wt.cd2fd.j <- sum(body.wt.cd2fd.padj[upper.tri(body.wt.cd2fd.padj, diag = FALSE)] <= 0.05)
body.ht.fd2cd.j <- sum(body.ht.fd2cd.padj[upper.tri(body.ht.fd2cd.padj, diag = FALSE)] <= 0.05)
body.ht.cd2fd.j <- sum(body.ht.cd2fd.padj[upper.tri(body.ht.cd2fd.padj, diag = FALSE)] <= 0.05)

head.wt.fd.cor.abs <- abs(head.wt.fd.cor[upper.tri(head.wt.fd.cor, diag = FALSE)])
head.wt.cd.cor.abs <- abs(head.wt.cd.cor[upper.tri(head.wt.cd.cor, diag = FALSE)])
head.ht.fd.cor.abs <- abs(head.ht.fd.cor[upper.tri(head.ht.fd.cor, diag = FALSE)])
head.ht.cd.cor.abs <- abs(head.ht.cd.cor[upper.tri(head.ht.cd.cor, diag = FALSE)])

head.wt.fd2cd.cor.abs <- abs(head.wt.fd2cd.cor[upper.tri(head.wt.fd2cd.cor, diag = FALSE)])
head.wt.cd2fd.cor.abs <- abs(head.wt.cd2fd.cor[upper.tri(head.wt.cd2fd.cor, diag = FALSE)])
head.ht.fd2cd.cor.abs <- abs(head.ht.fd2cd.cor[upper.tri(head.ht.fd2cd.cor, diag = FALSE)])
head.ht.cd2fd.cor.abs <- abs(head.ht.cd2fd.cor[upper.tri(head.ht.cd2fd.cor, diag = FALSE)])

body.wt.fd.cor.abs <- abs(body.wt.fd.cor[upper.tri(body.wt.fd.cor, diag = FALSE)])
body.wt.cd.cor.abs <- abs(body.wt.cd.cor[upper.tri(body.wt.cd.cor, diag = FALSE)])
body.ht.fd.cor.abs <- abs(body.ht.fd.cor[upper.tri(body.ht.fd.cor, diag = FALSE)])
body.ht.cd.cor.abs <- abs(body.ht.cd.cor[upper.tri(body.ht.cd.cor, diag = FALSE)])

body.wt.fd2cd.cor.abs <- abs(body.wt.fd2cd.cor[upper.tri(body.wt.fd2cd.cor, diag = FALSE)])
body.wt.cd2fd.cor.abs <- abs(body.wt.cd2fd.cor[upper.tri(body.wt.cd2fd.cor, diag = FALSE)])
body.ht.fd2cd.cor.abs <- abs(body.ht.fd2cd.cor[upper.tri(body.ht.fd2cd.cor, diag = FALSE)])
body.ht.cd2fd.cor.abs <- abs(body.ht.cd2fd.cor[upper.tri(body.ht.cd2fd.cor, diag = FALSE)])

head.wt.fd.cor.abs.sort <- head.wt.fd.cor.abs[order(head.wt.fd.cor.abs, decreasing = T)]
head.wt.cd.cor.abs.sort <- head.wt.cd.cor.abs[order(head.wt.cd.cor.abs, decreasing = T)]
head.ht.fd.cor.abs.sort <- head.ht.fd.cor.abs[order(head.ht.fd.cor.abs, decreasing = T)]
head.ht.cd.cor.abs.sort <- head.ht.cd.cor.abs[order(head.ht.cd.cor.abs, decreasing = T)]

head.wt.fd2cd.cor.abs.sort <- head.wt.fd2cd.cor.abs[order(head.wt.fd2cd.cor.abs, decreasing = T)]
head.wt.cd2fd.cor.abs.sort <- head.wt.cd2fd.cor.abs[order(head.wt.cd2fd.cor.abs, decreasing = T)]
head.ht.fd2cd.cor.abs.sort <- head.ht.fd2cd.cor.abs[order(head.ht.fd2cd.cor.abs, decreasing = T)]
head.ht.cd2fd.cor.abs.sort <- head.ht.cd2fd.cor.abs[order(head.ht.cd2fd.cor.abs, decreasing = T)]

body.wt.fd.cor.abs.sort <- body.wt.fd.cor.abs[order(body.wt.fd.cor.abs, decreasing = T)]
body.wt.cd.cor.abs.sort <- body.wt.cd.cor.abs[order(body.wt.cd.cor.abs, decreasing = T)]
body.ht.fd.cor.abs.sort <- body.ht.fd.cor.abs[order(body.ht.fd.cor.abs, decreasing = T)]
body.ht.cd.cor.abs.sort <- body.ht.cd.cor.abs[order(body.ht.cd.cor.abs, decreasing = T)]

body.wt.fd2cd.cor.abs.sort <- body.wt.fd2cd.cor.abs[order(body.wt.fd2cd.cor.abs, decreasing = T)]
body.wt.cd2fd.cor.abs.sort <- body.wt.cd2fd.cor.abs[order(body.wt.cd2fd.cor.abs, decreasing = T)]
body.ht.fd2cd.cor.abs.sort <- body.ht.fd2cd.cor.abs[order(body.ht.fd2cd.cor.abs, decreasing = T)]
body.ht.cd2fd.cor.abs.sort <- body.ht.cd2fd.cor.abs[order(body.ht.cd2fd.cor.abs, decreasing = T)]

head.wt.fd.cor.abs.sort[head.wt.fd.i]
head.wt.cd.cor.abs.sort[head.wt.cd.i]
head.ht.fd.cor.abs.sort[head.ht.fd.i]
head.ht.cd.cor.abs.sort[head.ht.cd.i]

head.wt.fd2cd.cor.abs.sort[head.wt.fd2cd.i]
head.wt.cd2fd.cor.abs.sort[head.wt.cd2fd.i]
head.ht.fd2cd.cor.abs.sort[head.ht.fd2cd.i]
head.ht.cd2fd.cor.abs.sort[head.ht.cd2fd.i]

body.wt.fd.cor.abs.sort[body.wt.fd.i]
body.wt.cd.cor.abs.sort[body.wt.cd.i]
body.ht.fd.cor.abs.sort[body.ht.fd.i]
body.ht.cd.cor.abs.sort[body.ht.cd.i]

body.wt.fd2cd.cor.abs.sort[body.wt.fd2cd.i]
body.wt.cd2fd.cor.abs.sort[body.wt.cd2fd.i]
body.ht.fd2cd.cor.abs.sort[body.ht.fd2cd.i]
body.ht.cd2fd.cor.abs.sort[body.ht.cd2fd.i]

head.wt.fd.cor.abs.sort[head.wt.fd.j]
head.wt.cd.cor.abs.sort[head.wt.cd.j]
head.ht.fd.cor.abs.sort[head.ht.fd.j]
head.ht.cd.cor.abs.sort[head.ht.cd.j]

head.wt.fd2cd.cor.abs.sort[head.wt.fd2cd.j]
head.wt.cd2fd.cor.abs.sort[head.wt.cd2fd.j]
head.ht.fd2cd.cor.abs.sort[head.ht.fd2cd.j]
head.ht.cd2fd.cor.abs.sort[head.ht.cd2fd.j]

body.wt.fd.cor.abs.sort[body.wt.fd.j]
body.wt.cd.cor.abs.sort[body.wt.cd.j]
body.ht.fd.cor.abs.sort[body.ht.fd.j]
body.ht.cd.cor.abs.sort[body.ht.cd.j]

body.wt.fd2cd.cor.abs.sort[body.wt.fd2cd.j]
body.wt.cd2fd.cor.abs.sort[body.wt.cd2fd.j]
body.ht.fd2cd.cor.abs.sort[body.ht.fd2cd.j]
body.ht.cd2fd.cor.abs.sort[body.ht.cd2fd.j]

# Construct Unweighted Networks -------------------------------------------
## Construct adjacency matrix
head.wt.fd.adj.fdr10 <- head.wt.fd.cor
head.wt.cd.adj.fdr10 <- head.wt.cd.cor
head.ht.fd.adj.fdr10 <- head.ht.fd.cor
head.ht.cd.adj.fdr10 <- head.ht.cd.cor
head.wt.fd2cd.adj.fdr10 <- head.wt.fd2cd.cor
head.wt.cd2fd.adj.fdr10 <- head.wt.cd2fd.cor
head.ht.fd2cd.adj.fdr10 <- head.ht.fd2cd.cor
head.ht.cd2fd.adj.fdr10 <- head.ht.cd2fd.cor

body.wt.fd.adj.fdr10 <- body.wt.fd.cor
body.wt.cd.adj.fdr10 <- body.wt.cd.cor
body.ht.fd.adj.fdr10 <- body.ht.fd.cor
body.ht.cd.adj.fdr10 <- body.ht.cd.cor
body.wt.fd2cd.adj.fdr10 <- body.wt.fd2cd.cor
body.wt.cd2fd.adj.fdr10 <- body.wt.cd2fd.cor
body.ht.fd2cd.adj.fdr10 <- body.ht.fd2cd.cor
body.ht.cd2fd.adj.fdr10 <- body.ht.cd2fd.cor

head.wt.fd.adj.fdr5 <- head.wt.fd.cor
head.wt.cd.adj.fdr5 <- head.wt.cd.cor
head.ht.fd.adj.fdr5 <- head.ht.fd.cor
head.ht.cd.adj.fdr5 <- head.ht.cd.cor
head.wt.fd2cd.adj.fdr5 <- head.wt.fd2cd.cor
head.wt.cd2fd.adj.fdr5 <- head.wt.cd2fd.cor
head.ht.fd2cd.adj.fdr5 <- head.ht.fd2cd.cor
head.ht.cd2fd.adj.fdr5 <- head.ht.cd2fd.cor

body.wt.fd.adj.fdr5 <- body.wt.fd.cor
body.wt.cd.adj.fdr5 <- body.wt.cd.cor
body.ht.fd.adj.fdr5 <- body.ht.fd.cor
body.ht.cd.adj.fdr5 <- body.ht.cd.cor
body.wt.fd2cd.adj.fdr5 <- body.wt.fd2cd.cor
body.wt.cd2fd.adj.fdr5 <- body.wt.cd2fd.cor
body.ht.fd2cd.adj.fdr5 <- body.ht.fd2cd.cor
body.ht.cd2fd.adj.fdr5 <- body.ht.cd2fd.cor

head.wt.fd.adj.fdr10[head.wt.fd.padj <= 0.1] = 1
head.wt.cd.adj.fdr10[head.wt.cd.padj <= 0.1] = 1
head.ht.fd.adj.fdr10[head.ht.fd.padj <= 0.1] = 1
head.ht.cd.adj.fdr10[head.ht.cd.padj <= 0.1] = 1
head.wt.fd2cd.adj.fdr10[head.wt.fd2cd.padj <= 0.1] = 1
head.wt.cd2fd.adj.fdr10[head.wt.cd2fd.padj <= 0.1] = 1
head.ht.fd2cd.adj.fdr10[head.ht.fd2cd.padj <= 0.1] = 1
head.ht.cd2fd.adj.fdr10[head.ht.cd2fd.padj <= 0.1] = 1

body.wt.fd.adj.fdr10[body.wt.fd.padj <= 0.1] = 1
body.wt.cd.adj.fdr10[body.wt.cd.padj <= 0.1] = 1
body.ht.fd.adj.fdr10[body.ht.fd.padj <= 0.1] = 1
body.ht.cd.adj.fdr10[body.ht.cd.padj <= 0.1] = 1
body.wt.fd2cd.adj.fdr10[body.wt.fd2cd.padj <= 0.1] = 1
body.wt.cd2fd.adj.fdr10[body.wt.cd2fd.padj <= 0.1] = 1
body.ht.fd2cd.adj.fdr10[body.ht.fd2cd.padj <= 0.1] = 1
body.ht.cd2fd.adj.fdr10[body.ht.cd2fd.padj <= 0.1] = 1

head.wt.fd.adj.fdr5[head.wt.fd.padj <= 0.05] = 1
head.wt.cd.adj.fdr5[head.wt.cd.padj <= 0.05] = 1
head.ht.fd.adj.fdr5[head.ht.fd.padj <= 0.05] = 1
head.ht.cd.adj.fdr5[head.ht.cd.padj <= 0.05] = 1
head.wt.fd2cd.adj.fdr5[head.wt.fd2cd.padj <= 0.05] = 1
head.wt.cd2fd.adj.fdr5[head.wt.cd2fd.padj <= 0.05] = 1
head.ht.fd2cd.adj.fdr5[head.ht.fd2cd.padj <= 0.05] = 1
head.ht.cd2fd.adj.fdr5[head.ht.cd2fd.padj <= 0.05] = 1

body.wt.fd.adj.fdr5[body.wt.fd.padj <= 0.05] = 1
body.wt.cd.adj.fdr5[body.wt.cd.padj <= 0.05] = 1
body.ht.fd.adj.fdr5[body.ht.fd.padj <= 0.05] = 1
body.ht.cd.adj.fdr5[body.ht.cd.padj <= 0.05] = 1
body.wt.fd2cd.adj.fdr5[body.wt.fd2cd.padj <= 0.05] = 1
body.wt.cd2fd.adj.fdr5[body.wt.cd2fd.padj <= 0.05] = 1
body.ht.fd2cd.adj.fdr5[body.ht.fd2cd.padj <= 0.05] = 1
body.ht.cd2fd.adj.fdr5[body.ht.cd2fd.padj <= 0.05] = 1

head.wt.fd.adj.fdr10[head.wt.fd.adj.fdr10 != 1] <- 0
head.wt.cd.adj.fdr10[head.wt.cd.adj.fdr10 != 1] <- 0
head.ht.fd.adj.fdr10[head.ht.fd.adj.fdr10 != 1] <- 0
head.ht.cd.adj.fdr10[head.ht.cd.adj.fdr10 != 1] <- 0
head.wt.fd2cd.adj.fdr10[head.wt.fd2cd.adj.fdr10 != 1] <- 0
head.wt.cd2fd.adj.fdr10[head.wt.cd2fd.adj.fdr10 != 1] <- 0
head.ht.fd2cd.adj.fdr10[head.ht.fd2cd.adj.fdr10 != 1] <- 0
head.ht.cd2fd.adj.fdr10[head.ht.cd2fd.adj.fdr10 != 1] <- 0

body.wt.fd.adj.fdr10[body.wt.fd.adj.fdr10 != 1] <- 0
body.wt.cd.adj.fdr10[body.wt.cd.adj.fdr10 != 1] <- 0
body.ht.fd.adj.fdr10[body.ht.fd.adj.fdr10 != 1] <- 0
body.ht.cd.adj.fdr10[body.ht.cd.adj.fdr10 != 1] <- 0
body.wt.fd2cd.adj.fdr10[body.wt.fd2cd.adj.fdr10 != 1] <- 0
body.wt.cd2fd.adj.fdr10[body.wt.cd2fd.adj.fdr10 != 1] <- 0
body.ht.fd2cd.adj.fdr10[body.ht.fd2cd.adj.fdr10 != 1] <- 0
body.ht.cd2fd.adj.fdr10[body.ht.cd2fd.adj.fdr10 != 1] <- 0

head.wt.fd.adj.fdr5[head.wt.fd.adj.fdr5 != 1] <- 0
head.wt.cd.adj.fdr5[head.wt.cd.adj.fdr5 != 1] <- 0
head.ht.fd.adj.fdr5[head.ht.fd.adj.fdr5 != 1] <- 0
head.ht.cd.adj.fdr5[head.ht.cd.adj.fdr5 != 1] <- 0
head.wt.fd2cd.adj.fdr5[head.wt.fd2cd.adj.fdr5 != 1] <- 0
head.wt.cd2fd.adj.fdr5[head.wt.cd2fd.adj.fdr5 != 1] <- 0
head.ht.fd2cd.adj.fdr5[head.ht.fd2cd.adj.fdr5 != 1] <- 0
head.ht.cd2fd.adj.fdr5[head.ht.cd2fd.adj.fdr5 != 1] <- 0

body.wt.fd.adj.fdr5[body.wt.fd.adj.fdr5 != 1] <- 0
body.wt.cd.adj.fdr5[body.wt.cd.adj.fdr5 != 1] <- 0
body.ht.fd.adj.fdr5[body.ht.fd.adj.fdr5 != 1] <- 0
body.ht.cd.adj.fdr5[body.ht.cd.adj.fdr5 != 1] <- 0
body.wt.fd2cd.adj.fdr5[body.wt.fd2cd.adj.fdr5 != 1] <- 0
body.wt.cd2fd.adj.fdr5[body.wt.cd2fd.adj.fdr5 != 1] <- 0
body.ht.fd2cd.adj.fdr5[body.ht.fd2cd.adj.fdr5 != 1] <- 0
body.ht.cd2fd.adj.fdr5[body.ht.cd2fd.adj.fdr5 != 1] <- 0

## Remove Self-loop
diag(head.wt.fd.adj.fdr10) <- 0
diag(head.wt.cd.adj.fdr10) <- 0
diag(head.ht.fd.adj.fdr10) <- 0
diag(head.ht.cd.adj.fdr10) <- 0
diag(head.wt.fd2cd.adj.fdr10) <- 0
diag(head.wt.cd2fd.adj.fdr10) <- 0
diag(head.ht.fd2cd.adj.fdr10) <- 0
diag(head.ht.cd2fd.adj.fdr10) <- 0

diag(body.wt.fd.adj.fdr10) <- 0
diag(body.wt.cd.adj.fdr10) <- 0
diag(body.ht.fd.adj.fdr10) <- 0
diag(body.ht.cd.adj.fdr10) <- 0
diag(body.wt.fd2cd.adj.fdr10) <- 0
diag(body.wt.cd2fd.adj.fdr10) <- 0
diag(body.ht.fd2cd.adj.fdr10) <- 0
diag(body.ht.cd2fd.adj.fdr10) <- 0

diag(head.wt.fd.adj.fdr5) <- 0
diag(head.wt.cd.adj.fdr5) <- 0
diag(head.ht.fd.adj.fdr5) <- 0
diag(head.ht.cd.adj.fdr5) <- 0
diag(head.wt.fd2cd.adj.fdr5) <- 0
diag(head.wt.cd2fd.adj.fdr5) <- 0
diag(head.ht.fd2cd.adj.fdr5) <- 0
diag(head.ht.cd2fd.adj.fdr5) <- 0

diag(body.wt.fd.adj.fdr5) <- 0
diag(body.wt.cd.adj.fdr5) <- 0
diag(body.ht.fd.adj.fdr5) <- 0
diag(body.ht.cd.adj.fdr5) <- 0
diag(body.wt.fd2cd.adj.fdr5) <- 0
diag(body.wt.cd2fd.adj.fdr5) <- 0
diag(body.ht.fd2cd.adj.fdr5) <- 0
diag(body.ht.cd2fd.adj.fdr5) <- 0

## Create Graphs from adjacency matrices
### Unweighted networks
head.wt.fd.g.fdr10 <- graph.adjacency(head.wt.fd.adj.fdr10, mode = "undirected", weighted = NULL)
head.wt.cd.g.fdr10 <- graph.adjacency(head.wt.cd.adj.fdr10, mode = "undirected", weighted = NULL)
head.ht.fd.g.fdr10 <- graph.adjacency(head.ht.fd.adj.fdr10, mode = "undirected", weighted = NULL)
head.ht.cd.g.fdr10 <- graph.adjacency(head.ht.cd.adj.fdr10, mode = "undirected", weighted = NULL)
head.wt.fd2cd.g.fdr10 <- graph.adjacency(head.wt.fd2cd.adj.fdr10, mode = "undirected", weighted = NULL)
head.wt.cd2fd.g.fdr10 <- graph.adjacency(head.wt.cd2fd.adj.fdr10, mode = "undirected", weighted = NULL)
head.ht.fd2cd.g.fdr10 <- graph.adjacency(head.ht.fd2cd.adj.fdr10, mode = "undirected", weighted = NULL)
head.ht.cd2fd.g.fdr10 <- graph.adjacency(head.ht.cd2fd.adj.fdr10, mode = "undirected", weighted = NULL)

body.wt.fd.g.fdr10 <- graph.adjacency(body.wt.fd.adj.fdr10, mode = "undirected", weighted = NULL)
body.wt.cd.g.fdr10 <- graph.adjacency(body.wt.cd.adj.fdr10, mode = "undirected", weighted = NULL)
body.ht.fd.g.fdr10 <- graph.adjacency(body.ht.fd.adj.fdr10, mode = "undirected", weighted = NULL)
body.ht.cd.g.fdr10 <- graph.adjacency(body.ht.cd.adj.fdr10, mode = "undirected", weighted = NULL)
body.wt.fd2cd.g.fdr10 <- graph.adjacency(body.wt.fd2cd.adj.fdr10, mode = "undirected", weighted = NULL)
body.wt.cd2fd.g.fdr10 <- graph.adjacency(body.wt.cd2fd.adj.fdr10, mode = "undirected", weighted = NULL)
body.ht.fd2cd.g.fdr10 <- graph.adjacency(body.ht.fd2cd.adj.fdr10, mode = "undirected", weighted = NULL)
body.ht.cd2fd.g.fdr10 <- graph.adjacency(body.ht.cd2fd.adj.fdr10, mode = "undirected", weighted = NULL)

head.wt.fd.g.fdr5 <- graph.adjacency(head.wt.fd.adj.fdr5, mode = "undirected", weighted = NULL)
head.wt.cd.g.fdr5 <- graph.adjacency(head.wt.cd.adj.fdr5, mode = "undirected", weighted = NULL)
head.ht.fd.g.fdr5 <- graph.adjacency(head.ht.fd.adj.fdr5, mode = "undirected", weighted = NULL)
head.ht.cd.g.fdr5 <- graph.adjacency(head.ht.cd.adj.fdr5, mode = "undirected", weighted = NULL)
head.wt.fd2cd.g.fdr5 <- graph.adjacency(head.wt.fd2cd.adj.fdr5, mode = "undirected", weighted = NULL)
head.wt.cd2fd.g.fdr5 <- graph.adjacency(head.wt.cd2fd.adj.fdr5, mode = "undirected", weighted = NULL)
head.ht.fd2cd.g.fdr5 <- graph.adjacency(head.ht.fd2cd.adj.fdr5, mode = "undirected", weighted = NULL)
head.ht.cd2fd.g.fdr5 <- graph.adjacency(head.ht.cd2fd.adj.fdr5, mode = "undirected", weighted = NULL)

body.wt.fd.g.fdr5 <- graph.adjacency(body.wt.fd.adj.fdr5, mode = "undirected", weighted = NULL)
body.wt.cd.g.fdr5 <- graph.adjacency(body.wt.cd.adj.fdr5, mode = "undirected", weighted = NULL)
body.ht.fd.g.fdr5 <- graph.adjacency(body.ht.fd.adj.fdr5, mode = "undirected", weighted = NULL)
body.ht.cd.g.fdr5 <- graph.adjacency(body.ht.cd.adj.fdr5, mode = "undirected", weighted = NULL)
body.wt.fd2cd.g.fdr5 <- graph.adjacency(body.wt.fd2cd.adj.fdr5, mode = "undirected", weighted = NULL)
body.wt.cd2fd.g.fdr5 <- graph.adjacency(body.wt.cd2fd.adj.fdr5, mode = "undirected", weighted = NULL)
body.ht.fd2cd.g.fdr5 <- graph.adjacency(body.ht.fd2cd.adj.fdr5, mode = "undirected", weighted = NULL)
body.ht.cd2fd.g.fdr5 <- graph.adjacency(body.ht.cd2fd.adj.fdr5, mode = "undirected", weighted = NULL)

ecount(head.wt.fd.g.fdr10)
ecount(head.wt.cd.g.fdr10)
ecount(head.ht.fd.g.fdr10)
ecount(head.ht.cd.g.fdr10)

ecount(body.wt.fd.g.fdr10)
ecount(body.wt.cd.g.fdr10)
ecount(body.ht.fd.g.fdr10)
ecount(body.ht.cd.g.fdr10)

ecount(head.wt.fd.g.fdr5)
ecount(head.wt.cd.g.fdr5)
ecount(head.ht.fd.g.fdr5)
ecount(head.ht.cd.g.fdr5)

ecount(body.wt.fd.g.fdr5)
ecount(body.wt.cd.g.fdr5)
ecount(body.ht.fd.g.fdr5)
ecount(body.ht.cd.g.fdr5)

# Network Visualization ---------------------------------------------------
V(head.wt.fd.g.fdr10)$label <- NA
V(head.wt.cd.g.fdr10)$label <- NA
V(head.ht.fd.g.fdr10)$label <- NA
V(head.ht.cd.g.fdr10)$label <- NA

V(body.wt.fd.g.fdr10)$label <- NA
V(body.wt.cd.g.fdr10)$label <- NA
V(body.ht.fd.g.fdr10)$label <- NA
V(body.ht.cd.g.fdr10)$label <- NA

V(head.wt.fd.g.fdr5)$label <- NA
V(head.wt.cd.g.fdr5)$label <- NA
V(head.ht.fd.g.fdr5)$label <- NA
V(head.ht.cd.g.fdr5)$label <- NA

V(body.wt.fd.g.fdr5)$label <- NA
V(body.wt.cd.g.fdr5)$label <- NA
V(body.ht.fd.g.fdr5)$label <- NA
V(body.ht.cd.g.fdr5)$label <- NA

par(mfrow=c(4, 4), mar = c(1, 1, 2.5, 1) + 0.1, mgp = c(2, 0, 0) + 0.5)
## FDR = 0.10
plot(head.wt.fd.g.fdr10, layout = layout.fruchterman.reingold, main = "Heads / w1118 / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(head.wt.cd.g.fdr10, layout = layout.fruchterman.reingold, main = "Heads / w1118 / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(head.ht.fd.g.fdr10, layout = layout.fruchterman.reingold, main = "Heads / 5-HT2A / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(head.ht.cd.g.fdr10, layout = layout.fruchterman.reingold, main = "Heads / 5-HT2A / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.wt.fd.g.fdr10, layout = layout.fruchterman.reingold, main = "Bodies / w1118 / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.wt.cd.g.fdr10, layout = layout.fruchterman.reingold, main = "Bodies / w1118 / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.ht.fd.g.fdr10, layout = layout.fruchterman.reingold, main = "Bodies / 5-HT2A / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.ht.cd.g.fdr10, layout = layout.fruchterman.reingold, main = "Bodies / 5-HT2A / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

## FDR = 0.05
plot(head.wt.fd.g.fdr5, layout = layout.fruchterman.reingold, main = "Heads / w1118 / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(head.wt.cd.g.fdr5, layout = layout.fruchterman.reingold, main = "Heads / w1118 / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(head.ht.fd.g.fdr5, layout = layout.fruchterman.reingold, main = "Heads / 5-HT2A / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(head.ht.cd.g.fdr5, layout = layout.fruchterman.reingold, main = "Heads / 5-HT2A / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.wt.fd.g.fdr5, layout = layout.fruchterman.reingold, main = "Bodies / w1118 / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.wt.cd.g.fdr5, layout = layout.fruchterman.reingold, main = "Bodies / w1118 / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.ht.fd.g.fdr5, layout = layout.fruchterman.reingold, main = "Bodies / 5-HT2A / FD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

plot(body.ht.cd.g.fdr5, layout = layout.fruchterman.reingold, main = "Bodies / 5-HT2A / CD",
     vertex.color = "deepskyblue", vertex.frame.color = "grey1", vertex.size = 10)

### Will use FDR = 0.1 as the cut-off
head.wt.fd.g <- head.wt.fd.g.fdr10
head.wt.cd.g <- head.wt.cd.g.fdr10
head.ht.fd.g <- head.ht.fd.g.fdr10
head.ht.cd.g <- head.ht.cd.g.fdr10
head.wt.fd2cd.g <- head.wt.fd2cd.g.fdr10
head.wt.cd2fd.g <- head.wt.cd2fd.g.fdr10
head.ht.fd2cd.g <- head.ht.fd2cd.g.fdr10
head.ht.cd2fd.g <- head.ht.cd2fd.g.fdr10

body.wt.fd.g <- body.wt.fd.g.fdr10
body.wt.cd.g <- body.wt.cd.g.fdr10
body.ht.fd.g <- body.ht.fd.g.fdr10
body.ht.cd.g <- body.ht.cd.g.fdr10
body.wt.fd2cd.g <- body.wt.fd2cd.g.fdr10
body.wt.cd2fd.g <- body.wt.cd2fd.g.fdr10
body.ht.fd2cd.g <- body.ht.fd2cd.g.fdr10
body.ht.cd2fd.g <- body.ht.cd2fd.g.fdr10

# Construct Weighted Networks ---------------------------------------------
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

head.wt.fd.weighted.adj[head.wt.fd.padj > 0.1] <- 0
head.wt.cd.weighted.adj[head.wt.cd.padj > 0.1] <- 0
head.ht.fd.weighted.adj[head.ht.fd.padj > 0.1] <- 0
head.ht.cd.weighted.adj[head.ht.cd.padj > 0.1] <- 0
head.wt.fd2cd.weighted.adj[head.wt.fd2cd.padj > 0.1] <- 0
head.wt.cd2fd.weighted.adj[head.wt.cd2fd.padj > 0.1] <- 0
head.ht.fd2cd.weighted.adj[head.ht.fd2cd.padj > 0.1] <- 0
head.ht.cd2fd.weighted.adj[head.ht.cd2fd.padj > 0.1] <- 0

body.wt.fd.weighted.adj[body.wt.fd.padj > 0.1] <- 0
body.wt.cd.weighted.adj[body.wt.cd.padj > 0.1] <- 0
body.ht.fd.weighted.adj[body.ht.fd.padj > 0.1] <- 0
body.ht.cd.weighted.adj[body.ht.cd.padj > 0.1] <- 0
body.wt.fd2cd.weighted.adj[body.wt.fd2cd.padj > 0.1] <- 0
body.wt.cd2fd.weighted.adj[body.wt.cd2fd.padj > 0.1] <- 0
body.ht.fd2cd.weighted.adj[body.ht.fd2cd.padj > 0.1] <- 0
body.ht.cd2fd.weighted.adj[body.ht.cd2fd.padj > 0.1] <- 0

diag(head.wt.fd.weighted.adj) <- 0
diag(head.wt.cd.weighted.adj) <- 0
diag(head.ht.fd.weighted.adj) <- 0
diag(head.ht.cd.weighted.adj) <- 0
diag(head.wt.fd2cd.weighted.adj) <- 0
diag(head.wt.cd2fd.weighted.adj) <- 0
diag(head.ht.fd2cd.weighted.adj) <- 0
diag(head.ht.cd2fd.weighted.adj) <- 0

diag(body.wt.fd.weighted.adj) <- 0
diag(body.wt.cd.weighted.adj) <- 0
diag(body.ht.fd.weighted.adj) <- 0
diag(body.ht.cd.weighted.adj) <- 0
diag(body.wt.fd2cd.weighted.adj) <- 0
diag(body.wt.cd2fd.weighted.adj) <- 0
diag(body.ht.fd2cd.weighted.adj) <- 0
diag(body.ht.cd2fd.weighted.adj) <- 0

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

## Network edge number check
ecount(head.wt.fd.g)
ecount(head.wt.cd.g)
ecount(head.ht.fd.g)
ecount(head.ht.cd.g)

ecount(head.wt.fd.weighted.g) # should be the same as the unweighted network
ecount(head.wt.cd.weighted.g) # should be the same as the unweighted network
ecount(head.ht.fd.weighted.g) # should be the same as the unweighted network
ecount(head.ht.cd.weighted.g) # should be the same as the unweighted network

ecount(body.wt.fd.g)
ecount(body.wt.cd.g)
ecount(body.ht.fd.g)
ecount(body.ht.cd.g)

ecount(body.wt.fd.weighted.g) # should be the same as the unweighted network
ecount(body.wt.cd.weighted.g) # should be the same as the unweighted network
ecount(body.ht.fd.weighted.g) # should be the same as the unweighted network
ecount(body.ht.cd.weighted.g) # should be the same as the unweighted network

vcount(head.wt.fd.g.fdr10) ### 103
vcount(body.wt.fd.g.fdr10) ### 125

nrow(combinations(103,2))
nrow(combinations(125,2))

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

vcount(head.wt.fd.gc) ### 99
vcount(head.wt.cd.gc) ### 72
vcount(head.ht.fd.gc) ### 90
vcount(head.ht.cd.gc) ### 93
vcount(body.wt.fd.gc) ### 124
vcount(body.wt.cd.gc) ### 121
vcount(body.ht.fd.gc) ### 117
vcount(body.ht.cd.gc) ### 121

ecount(head.wt.fd.gc)/ecount(head.wt.fd.g) ### 1
ecount(head.wt.cd.gc)/ecount(head.wt.cd.g) ### 0.96
ecount(head.ht.fd.gc)/ecount(head.ht.fd.g) ### 0.996
ecount(head.ht.cd.gc)/ecount(head.ht.cd.g) ### 0.998
ecount(body.wt.fd.gc)/ecount(body.wt.fd.g) ### 1
ecount(body.wt.cd.gc)/ecount(body.wt.cd.g) ### 1
ecount(body.ht.fd.gc)/ecount(body.ht.fd.g) ### 0.999
ecount(body.ht.cd.gc)/ecount(body.ht.cd.g) ### 1

# Pathway Analysis --------------------------------------------------------
## Initiate data objects
head.wt.fd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)
head.wt.cd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)
head.ht.fd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)
head.ht.cd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)

body.wt.fd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.wt.cd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.ht.fd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.ht.cd.gc.mSet <- InitDataObjects("conc", "pathora", FALSE)

## Set up mSetObj with the list of compounds
head.wt.fd.gc.mSet <- Setup.MapData(head.wt.fd.gc.mSet, V(head.wt.fd.gc)$name)
head.wt.cd.gc.mSet <- Setup.MapData(head.wt.cd.gc.mSet, V(head.wt.cd.gc)$name)
head.ht.fd.gc.mSet <- Setup.MapData(head.ht.fd.gc.mSet, V(head.ht.fd.gc)$name)
head.ht.cd.gc.mSet <- Setup.MapData(head.ht.cd.gc.mSet, V(head.ht.cd.gc)$name)

body.wt.fd.gc.mSet <- Setup.MapData(body.wt.fd.gc.mSet, V(body.wt.fd.gc)$name)
body.wt.cd.gc.mSet <- Setup.MapData(body.wt.cd.gc.mSet, V(body.wt.cd.gc)$name)
body.ht.fd.gc.mSet <- Setup.MapData(body.ht.fd.gc.mSet, V(body.ht.fd.gc)$name)
body.ht.cd.gc.mSet <- Setup.MapData(body.ht.cd.gc.mSet, V(body.ht.cd.gc)$name)

## Cross reference list of compounds against libraries 
head.wt.fd.gc.mSet <- CrossReferencing(head.wt.fd.gc.mSet, "name")
head.wt.cd.gc.mSet <- CrossReferencing(head.wt.cd.gc.mSet, "name")
head.ht.fd.gc.mSet <- CrossReferencing(head.ht.fd.gc.mSet, "name")
head.ht.cd.gc.mSet <- CrossReferencing(head.ht.cd.gc.mSet, "name")

body.wt.fd.gc.mSet <- CrossReferencing(body.wt.fd.gc.mSet, "name")
body.wt.cd.gc.mSet <- CrossReferencing(body.wt.cd.gc.mSet, "name")
body.ht.fd.gc.mSet <- CrossReferencing(body.ht.fd.gc.mSet, "name")
body.ht.cd.gc.mSet <- CrossReferencing(body.ht.cd.gc.mSet, "name")

## Create the mapping results table
head.wt.fd.gc.mSet <- CreateMappingResultTable(head.wt.fd.gc.mSet)
head.wt.cd.gc.mSet <- CreateMappingResultTable(head.wt.cd.gc.mSet)
head.ht.fd.gc.mSet <- CreateMappingResultTable(head.ht.fd.gc.mSet)
head.ht.cd.gc.mSet <- CreateMappingResultTable(head.ht.cd.gc.mSet)

body.wt.fd.gc.mSet <- CreateMappingResultTable(body.wt.fd.gc.mSet)
body.wt.cd.gc.mSet <- CreateMappingResultTable(body.wt.cd.gc.mSet)
body.ht.fd.gc.mSet <- CreateMappingResultTable(body.ht.fd.gc.mSet)
body.ht.cd.gc.mSet <- CreateMappingResultTable(body.ht.cd.gc.mSet)

## KEGG enrichment test
head.wt.fd.gc.mSet <- SetKEGG.PathLib(head.wt.fd.gc.mSet, "dme", "current")
head.wt.cd.gc.mSet <- SetKEGG.PathLib(head.wt.cd.gc.mSet, "dme", "current")
head.ht.fd.gc.mSet <- SetKEGG.PathLib(head.ht.fd.gc.mSet, "dme", "current")
head.ht.cd.gc.mSet <- SetKEGG.PathLib(head.ht.cd.gc.mSet, "dme", "current")

body.wt.fd.gc.mSet <- SetKEGG.PathLib(body.wt.fd.gc.mSet, "dme", "current")
body.wt.cd.gc.mSet <- SetKEGG.PathLib(body.wt.cd.gc.mSet, "dme", "current")
body.ht.fd.gc.mSet <- SetKEGG.PathLib(body.ht.fd.gc.mSet, "dme", "current")
body.ht.cd.gc.mSet <- SetKEGG.PathLib(body.ht.cd.gc.mSet, "dme", "current")

head.wt.fd.gc.mSet <- SetMetabolomeFilter(head.wt.fd.gc.mSet, F)
head.wt.cd.gc.mSet <- SetMetabolomeFilter(head.wt.cd.gc.mSet, F)
head.ht.fd.gc.mSet <- SetMetabolomeFilter(head.ht.fd.gc.mSet, F)
head.ht.cd.gc.mSet <- SetMetabolomeFilter(head.ht.cd.gc.mSet, F)

body.wt.fd.gc.mSet <- SetMetabolomeFilter(body.wt.fd.gc.mSet, F)
body.wt.cd.gc.mSet <- SetMetabolomeFilter(body.wt.cd.gc.mSet, F)
body.ht.fd.gc.mSet <- SetMetabolomeFilter(body.ht.fd.gc.mSet, F)
body.ht.cd.gc.mSet <- SetMetabolomeFilter(body.ht.cd.gc.mSet, F)

head.wt.fd.gc.mSet <- CalculateOraScore(head.wt.fd.gc.mSet, "rbc", "hyperg")
head.wt.cd.gc.mSet <- CalculateOraScore(head.wt.cd.gc.mSet, "rbc", "hyperg")
head.ht.fd.gc.mSet <- CalculateOraScore(head.ht.fd.gc.mSet, "rbc", "hyperg")
head.ht.cd.gc.mSet <- CalculateOraScore(head.ht.cd.gc.mSet, "rbc", "hyperg")

body.wt.fd.gc.mSet <- CalculateOraScore(body.wt.fd.gc.mSet, "rbc", "hyperg")
body.wt.cd.gc.mSet <- CalculateOraScore(body.wt.cd.gc.mSet, "rbc", "hyperg")
body.ht.fd.gc.mSet <- CalculateOraScore(body.ht.fd.gc.mSet, "rbc", "hyperg")
body.ht.cd.gc.mSet <- CalculateOraScore(body.ht.cd.gc.mSet, "rbc", "hyperg")

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
