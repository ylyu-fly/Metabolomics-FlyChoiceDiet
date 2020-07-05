## Subject: Remove Batch Effect
## Project: Fly Choice Diet
## Part: 2
## Author: Yang Lyu
## Date created: 01/25/2018
## Date modified: 05/31/2020
## May require R 4.0

# Environment Settings ----------------------------------------------------
library(gtools)
library(sva)

# Data Input --------------------------------------------------------------
## Read data
head.edata <- readRDS("Data/ProcessedData/ImputedData-FlyChoiceDiet_Head.RDS")
body.edata <- readRDS("Data/ProcessedData/ImputedData-FlyChoiceDiet_Body.RDS")

head.pheno <- read.table("Data/RawData/Info-Sample-FlyChoiceDiet_Head.txt", 
                         sep = "\t", header = T)
body.pheno <- read.table("Data/RawData/Info-Sample-FlyChoiceDiet_Body.txt", 
                         sep = "\t", header = T)

# Data Clean --------------------------------------------------------------
rownames(head.pheno) <- head.pheno$SampleName
rownames(body.pheno) <- body.pheno$SampleName

head.pheno <- head.pheno[ , -1]
body.pheno <- body.pheno[ , -1]

head.pheno <- head.pheno[mixedorder(rownames(head.pheno)), ]
body.pheno <- body.pheno[mixedorder(rownames(body.pheno)), ]

head.batch <- as.numeric(head.pheno$FlyBatchID)
body.batch <- as.numeric(body.pheno$FlyBatchID)

head.modcombat <- model.matrix(~1, data = head.pheno)
body.modcombat <- model.matrix(~1, data = body.pheno)

# Run Combat --------------------------------------------------------------
head.combat.edata <- ComBat(dat = head.edata, 
                            batch = head.batch, 
                            mod = head.modcombat, 
                            par.prior = TRUE, 
                            prior.plots = FALSE)

body.combat.edata <- ComBat(dat = body.edata, 
                            batch = body.batch, 
                            mod = body.modcombat, 
                            par.prior = TRUE, prior.plots = FALSE)

# Run ANOVA ---------------------------------------------------------------
head.sample <- as.data.frame(matrix(unlist(strsplit(colnames(head.edata), "_")), ncol = 4, byrow = T))
body.sample <- as.data.frame(matrix(unlist(strsplit(colnames(body.edata), "_")), ncol = 4, byrow = T))

colnames(head.sample) = c("Genotype", "Diet", "Tissue", "Replicate")
colnames(body.sample) = c("Genotype", "Diet", "Tissue", "Replicate")

##Head
par(mfrow=c(2, 2), mar = c(4, 3, 1, 0) + 0.5, mgp = c(2, 0.5, 0))

head.diet <- factor(head.sample$Diet, levels = c("FD", "CD", "FD2CD", "CD2FD"))
head.genotype <- factor(head.sample$Genotype, levels = c("w1118","5-HT2A"))
head.batch <- factor(head.batch)

head.edata.batch.fval <- NULL
head.edata.batch.pval <- NULL
for(i in 1:nrow(head.edata)) {
    tmp.data <- data.frame(Expr = head.edata[i, ], Diet = head.diet, Genotype = head.genotype, Batch = head.batch)
    tmp.fit <- aov(Expr ~ Batch + Diet + Genotype + Diet*Genotype, data = tmp.data)
    head.edata.batch.fval[i] <- summary.aov(tmp.fit)[[1]][4][1, 1]
    head.edata.batch.pval[i] <- summary.aov(tmp.fit)[[1]][5][1, 1]
}
hist(head.edata.batch.fval, xlim = c(0, 90), breaks = 50, xlab = "F-value", main = "Raw data", las = 1)
hist(head.edata.batch.pval, xlim = c(0, 1), breaks = 50, xlab = "P-value", main = "Raw data", las = 1)
abline(v = 0.05, lty = 2, col = "red")
text(0.2, 40, "P=0.05")

head.combat.edata.batch.fval <- NULL
head.combat.edata.batch.pval <- NULL
for(i in 1:nrow(head.combat.edata)) {
    tmp.data <- data.frame(Expr = head.combat.edata[i, ], Diet = head.diet, Genotype = head.genotype, Batch = head.batch)
    tmp.fit <- aov(Expr ~ Batch + Diet + Genotype + Diet*Genotype, data = tmp.data)
    head.combat.edata.batch.fval[i] <- summary.aov(tmp.fit)[[1]][4][1, 1]
    head.combat.edata.batch.pval[i] <- summary.aov(tmp.fit)[[1]][5][1, 1]
}
hist(head.combat.edata.batch.fval, xlim = c(0, 90), breaks = 1, 
     xlab = "F-value", main = "After Combat", las = 1)
hist(head.combat.edata.batch.pval, xlim = c(0, 1), breaks = 50,
     xlab = "P-value", main = "After Combat", las = 1)
abline(v = 0.05, lty = 2, col = "red")
text(0.2, 40, "P=0.05")

##Body
par(mfrow=c(2, 2), mar = c(4, 3, 1, 0) + 0.5, mgp = c(2, 0.5, 0))

body.diet <- factor(body.sample$Diet, levels = c("FD", "CD", "FD2CD", "CD2FD"))
body.genotype <- factor(body.sample$Genotype, levels = c("w1118","5-HT2A"))
body.batch <- factor(body.batch)

body.edata.batch.fval <- NULL
body.edata.batch.pval <- NULL
for(i in 1:nrow(body.edata)) {
    tmp.data <- data.frame(Expr = body.edata[i, ], Diet = body.diet, Genotype = body.genotype, Batch = body.batch)
    tmp.fit <- aov(Expr ~ Batch + Diet + Genotype + Diet*Genotype, data = tmp.data)
    body.edata.batch.fval[i] <- summary.aov(tmp.fit)[[1]][4][1, 1]
    body.edata.batch.pval[i] <- summary.aov(tmp.fit)[[1]][5][1, 1]
}
hist(body.edata.batch.fval, xlim = c(0, 90), breaks = 50, xlab = "F-value", main = "Raw data", las = 1)
hist(body.edata.batch.pval, xlim = c(0, 1), breaks = 50, xlab = "P-value", main = "Raw data", las = 1)
abline(v = 0.05, lty = 2, col = "red")
text(0.2, 40, "P=0.05")

body.combat.edata.batch.fval <- NULL
body.combat.edata.batch.pval <- NULL
for(i in 1:nrow(body.combat.edata)) {
    tmp.data <- data.frame(Expr = body.combat.edata[i, ], Diet = body.diet, Genotype = body.genotype, Batch = body.batch)
    tmp.fit <- aov(Expr ~ Batch + Diet + Genotype + Diet*Genotype, data = tmp.data)
    body.combat.edata.batch.fval[i] <- summary.aov(tmp.fit)[[1]][4][1, 1]
    body.combat.edata.batch.pval[i] <- summary.aov(tmp.fit)[[1]][5][1, 1]
}
hist(body.combat.edata.batch.fval, xlim = c(0, 90), breaks = 1, 
     xlab = "F-value", main = "After Combat", las = 1)
hist(body.combat.edata.batch.pval, xlim = c(0, 1), breaks = 50,
     xlab = "P-value", main = "After Combat", las = 1)
abline(v = 0.05, lty = 2, col = "red")
text(0.2, 40, "P=0.05")

# Data Output -------------------------------------------------------------
head.result.file <- "Data/ProcessedData/BatchEffectRemovedData-FlyChoiceDiet_Head.RDS"
body.result.file <- "Data/ProcessedData/BatchEffectRemovedData-FlyChoiceDiet_Body.RDS"

saveRDS(head.combat.edata, file = head.result.file)
saveRDS(body.combat.edata, file = body.result.file)
