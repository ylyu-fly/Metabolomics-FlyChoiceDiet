## Subject: Normalization
## Project: Fly Choice Diet
## Part: 3
## Author: Yang Lyu
## Date created: 01/25/2018
## Date modified: 05/31/2020
## May require R 4.0

# Environment Settings ----------------------------------------------------

# Data Input --------------------------------------------------------------
## Read data
head.data <- readRDS("Data/ProcessedData/ImputedData-FlyChoiceDiet_Head.RDS")
body.data <- readRDS("Data/ProcessedData/ImputedData-FlyChoiceDiet_Body.RDS")

head.data.combat <- readRDS("Data/ProcessedData/BatchEffectRemovedData-FlyChoiceDiet_Head.RDS")
body.data.combat <- readRDS("Data/ProcessedData/BatchEffectRemovedData-FlyChoiceDiet_Body.RDS")

# Data Normalization ------------------------------------------------------
head.data.norm <- head.data
for(i in 1:ncol(head.data)){
    x <- head.data[, i]
    head.data.norm[, i] <- (x - mean(x))/sd(x)
}

body.data.norm <- body.data
for(i in 1:ncol(body.data)){
    x <- body.data[, i]
    body.data.norm[, i] <- (x - mean(x))/sd(x)
}

head.data.combat.norm <- head.data.combat
for(i in 1:ncol(head.data.combat)){
    x <- head.data.combat[, i]
    head.data.combat.norm[, i] <- (x - mean(x))/sd(x)
}

body.data.combat.norm <- body.data.combat
for(i in 1:ncol(body.data.combat)){
    x <- body.data.combat[, i]
    body.data.combat.norm[, i] <- (x - mean(x))/sd(x)
}

# Data Visualization ------------------------------------------------------
plot.head.data <- head.data
plot.body.data <- body.data
plot.head.data.norm <- head.data.norm
plot.body.data.norm <- body.data.norm

plot.head.data.combat <- head.data.combat
plot.body.data.combat <- body.data.combat
plot.head.data.combat.norm <- head.data.combat.norm
plot.body.data.combat.norm <- body.data.combat.norm

colnames(plot.head.data) <- 1:ncol(plot.head.data)
colnames(plot.body.data) <- 1:ncol(plot.body.data)
colnames(plot.head.data.norm) <- 1:ncol(plot.head.data.norm)
colnames(plot.body.data.norm) <- 1:ncol(plot.body.data.norm)

colnames(plot.head.data.combat) <- 1:ncol(plot.head.data.combat)
colnames(plot.body.data.combat) <- 1:ncol(plot.body.data.combat)
colnames(plot.head.data.combat.norm) <- 1:ncol(plot.head.data.combat.norm)
colnames(plot.body.data.combat.norm) <- 1:ncol(plot.body.data.combat.norm)

par(mfrow = c(4, 2), mar = c(3, 3, 1, 0) + 0.5, mgp = c(2, 0.5, 0) + 0.2)
boxplot(plot.head.data, las = 2, main = "Head / Before normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)
boxplot(plot.body.data, las = 2, main = "Body / Before normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)
boxplot(plot.head.data.norm, las = 2, main = "Head / After normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)
boxplot(plot.body.data.norm, las = 2, main = "Body / After normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)

boxplot(plot.head.data.combat, las = 2, main = "Combat Head / Before normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)
boxplot(plot.body.data.combat, las = 2, main = "Combat Body / Before normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)
boxplot(plot.head.data.combat.norm, las = 2, main = "Combat Head / After normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)
boxplot(plot.body.data.combat.norm, las = 2, main = "Combat Body / After normalization", col = "grey",
        xlab = "Sample", ylab = "Metabolite abundance", cex.axis = 0.7)

# Data Output -------------------------------------------------------------
head.result.file <- "Data/ProcessedData/NormalizedData-FlyChoiceDiet_Head.RDS"
body.result.file <- "Data/ProcessedData/NormalizedData-FlyChoiceDiet_Body.RDS"

combat.head.result.file <- "Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Head.RDS"
combat.body.result.file <- "Data/ProcessedData/CombatNormalizedData-FlyChoiceDiet_Body.RDS"

saveRDS(head.data.norm, file = head.result.file)
saveRDS(body.data.norm, file = body.result.file)

saveRDS(head.data.combat.norm, file = combat.head.result.file)
saveRDS(body.data.combat.norm, file = combat.body.result.file)