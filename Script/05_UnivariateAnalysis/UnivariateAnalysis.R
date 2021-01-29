## Subject: Univariate Analysis
## Project: Fly Choice Diet
## Part: 5
## Author: Yang Lyu
## Date created: 01/25/2018
## Date modified: 12/07/2020
## May require R 4.0

# Environment Settings ----------------------------------------------------

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

head.sample$Group <- paste(head.sample$Genotype, head.sample$Diet, sep = "_")
body.sample$Group <- paste(body.sample$Genotype, body.sample$Diet, sep = "_")

head.sample$Group <- factor(head.sample$Group,
                            levels = c("w1118_FD", "w1118_CD", "w1118_FD2CD", "w1118_CD2FD",
                                       "5-HT2A_FD", "5-HT2A_CD", "5-HT2A_FD2CD", "5-HT2A_CD2FD"))

body.sample$group <- factor(body.sample$Group,
                            levels = c("w1118_FD", "w1118_CD", "w1118_FD2CD", "w1118_CD2FD",
                                       "5-HT2A_FD", "5-HT2A_CD", "5-HT2A_FD2CD", "5-HT2A_CD2FD"))

## Subtract data
head.used.i <- head.sample$Group == "w1118_CD"  | head.sample$Group == "w1118_FD" |
    head.sample$Group == "5-HT2A_CD" | head.sample$Group == "5-HT2A_FD"
body.used.i <- body.sample$Group == "w1118_CD"  | body.sample$Group == "w1118_FD" |
    body.sample$Group == "5-HT2A_CD" | body.sample$Group == "5-HT2A_FD"
head.data.used <- head.data[ , head.used.i]
body.data.used <- body.data[ , body.used.i]

## Pool samples from the same treatment
head.data.tr <- as.data.frame(t(head.data))
body.data.tr <- as.data.frame(t(body.data))

head.data.tr$Group <- head.sample$Group
body.data.tr$Group <- body.sample$Group

head.aggregate <- aggregate(. ~ Group, head.data.tr, mean)
body.aggregate <- aggregate(. ~ Group, body.data.tr, mean)

rownames(head.aggregate) <- head.aggregate$Group
rownames(body.aggregate) <- body.aggregate$Group

head.aggregate <- head.aggregate[, -1]
body.aggregate <- body.aggregate[, -1]

head.aggregate.used <- head.aggregate[c(1:2, 5:6), ]
body.aggregate.used <- body.aggregate[c(1:2, 5:6), ]

head.data.scaled <- head.aggregate.used
for(i in 1:ncol(head.data.scaled)) {
    x <- head.data.scaled[, i]
    head.data.scaled[, i] <- (x - mean(x)) / sd(x)
}

body.data.scaled <- body.aggregate.used
for(i in 1:ncol(body.data.scaled)) {
    x <- body.data.scaled[, i]
    body.data.scaled[, i] <- (x - mean(x)) / sd(x)
}

# Run Linear Model --------------------------------------------------------
## Calculate p-values
### head
head.linear.pvals <- data.frame(Diet.pval = numeric(), 
                                Diet.padj = numeric(), 
                                Genotype.pval = numeric(), 
                                Genotype.padj = numeric(), 
                                Diet.Genotype.pval = numeric(),
                                Diet.Genotype.padj = numeric())

for(i in 1:nrow(head.data.used)) {
    each.metabolite.data <- data.frame(Expr = head.data.used[i, ], 
                                       Diet = head.sample[head.used.i, ]$Diet, 
                                       Genotype = head.sample[head.used.i, ]$Genotype)
    each.metabolite.fit   <- aov(Expr ~ Diet + Genotype + Diet*Genotype, data = each.metabolite.data)
    
    head.linear.pvals[i, ]$Diet.pval            <- summary.aov(each.metabolite.fit)[[1]][5][1, 1]
    head.linear.pvals[i, ]$Genotype.pval        <- summary.aov(each.metabolite.fit)[[1]][5][2, 1]
    head.linear.pvals[i, ]$Diet.Genotype.pval   <- summary.aov(each.metabolite.fit)[[1]][5][3, 1]
}

head.linear.pvals$Diet.padj          <- p.adjust(head.linear.pvals$Diet.pval, 
                                                 method = "BH")
head.linear.pvals$Genotype.padj      <- p.adjust(head.linear.pvals$Genotype.pval, 
                                                 method = "BH")
head.linear.pvals$Diet.Genotype.padj <- p.adjust(head.linear.pvals$Diet.Genotype.pval, 
                                                 method = "BH")
rownames(head.linear.pvals) <- rownames(head.data)

### Body
body.linear.pvals <- data.frame(Diet.pval = numeric(), 
                                Diet.padj = numeric(), 
                                Genotype.pval = numeric(), 
                                Genotype.padj = numeric(), 
                                Diet.Genotype.pval = numeric(),
                                Diet.Genotype.padj = numeric())

for(i in 1:nrow(body.data.used)) {
    each.metabolite.data <- data.frame(Expr = body.data.used[i, ], 
                                       Diet = body.sample[body.used.i, ]$Diet, 
                                       Genotype = body.sample[body.used.i, ]$Genotype)
    each.metabolite.fit   <- aov(Expr ~ Diet + Genotype + Diet*Genotype, data = each.metabolite.data)
    
    body.linear.pvals[i, ]$Diet.pval            <- summary.aov(each.metabolite.fit)[[1]][5][1, 1]
    body.linear.pvals[i, ]$Genotype.pval        <- summary.aov(each.metabolite.fit)[[1]][5][2, 1]
    body.linear.pvals[i, ]$Diet.Genotype.pval   <- summary.aov(each.metabolite.fit)[[1]][5][3, 1]
}

body.linear.pvals$Diet.padj          <- p.adjust(body.linear.pvals$Diet.pval, 
                                                 method = "BH")
body.linear.pvals$Genotype.padj      <- p.adjust(body.linear.pvals$Genotype.pval, 
                                                 method = "BH")
body.linear.pvals$Diet.Genotype.padj <- p.adjust(body.linear.pvals$Diet.Genotype.pval, 
                                                 method = "BH")
rownames(body.linear.pvals) <- rownames(body.data)

sum(head.linear.pvals$Diet.pval <= 0.05, na.rm = TRUE)
sum(head.linear.pvals$Genotype.pval <= 0.05, na.rm = TRUE)
sum(head.linear.pvals$Diet.Genotype.pval <= 0.05, na.rm = TRUE)

sum(body.linear.pvals$Diet.pval <= 0.05, na.rm = TRUE)
sum(body.linear.pvals$Genotype.pval <= 0.05, na.rm = TRUE)
sum(body.linear.pvals$Diet.Genotype.pval <= 0.05, na.rm = TRUE)

head.names.diet <- rownames(head.linear.pvals[head.linear.pvals$Diet.pval <= 0.05 &
                                                  head.linear.pvals$Diet.Genotype.pval > 0.05, ])
body.names.diet <- rownames(body.linear.pvals[body.linear.pvals$Diet.pval <= 0.05 &
                                                  body.linear.pvals$Diet.Genotype.pval > 0.05, ])

head.names.genotype <- rownames(head.linear.pvals[head.linear.pvals$Genotype.pval <= 0.05 &
                                                      head.linear.pvals$Diet.Genotype.pval > 0.05, ])
body.names.genotype <- rownames(body.linear.pvals[body.linear.pvals$Genotype.pval <= 0.05 &
                                                      body.linear.pvals$Diet.Genotype.pval > 0.05, ])

head.names.dxg <- rownames(head.linear.pvals[head.linear.pvals$Diet.Genotype.pval <= 0.05, ])
body.names.dxg <- rownames(body.linear.pvals[body.linear.pvals$Diet.Genotype.pval <= 0.05, ])

head.names <- as.data.frame(rownames(head.linear.pvals))
body.names <- as.data.frame(rownames(body.linear.pvals))

head.names.diet <- as.data.frame(head.names.diet)
body.names.diet <- as.data.frame(body.names.diet)

head.names.genotype <- as.data.frame(head.names.genotype)
body.names.genotype <- as.data.frame(body.names.genotype)

head.names.dxg <- as.data.frame(head.names.dxg)
body.names.dxg <- as.data.frame(body.names.dxg)

# Data Output -------------------------------------------------------------
colnames(head.names) <- "MetaboliteID"
colnames(body.names) <- "MetaboliteID"

colnames(head.names.diet) <- "MetaboliteID"
colnames(body.names.diet) <- "MetaboliteID"

colnames(head.names.genotype) <- "MetaboliteID"
colnames(body.names.genotype) <- "MetaboliteID"

colnames(head.names.dxg) <- "MetaboliteID"
colnames(body.names.dxg) <- "MetaboliteID"

## I fixed a few metabolite names with hands. So instead of regenerating the hits just please use the files in the folder.

#write.table(head.names.diet, "Data/ProcessedData/Hits-Metabolite_diet_head.txt", sep = "\t", quote = F, row.names = F, col.names = F)
#write.table(body.names.diet, "Data/ProcessedData/Hits-Metabolite_diet_body.txt", sep = "\t", quote = F, row.names = F, col.names = F)

#write.table(head.names.genotype, "Data/ProcessedData/Hits-Metabolite_genotype_head.txt", sep = "\t", quote = F, row.names = F, col.names = F)
#write.table(body.names.genotype, "Data/ProcessedData/Hits-Metabolite_genotype_body.txt", sep = "\t", quote = F, row.names = F, col.names = F)

#write.table(head.names.dxg, "Data/ProcessedData/Hits-Metabolite_dxg_head.txt", sep = "\t", quote = F, row.names = F, col.names = F)
#write.table(body.names.dxg, "Data/ProcessedData/Hits-Metabolite_dxg_body.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.table(head.linear.pvals[,c(1, 3, 5)], "Data/ProcessedData/Pvals-Metabolite_head.txt", sep = "\t", quote = F, row.names = T, col.names = T)
write.table(body.linear.pvals[,c(1, 3, 5)], "Data/ProcessedData/Pvals-Metabolite_body.txt", sep = "\t", quote = F, row.names = T, col.names = T)

save(head.names, body.names,
     head.names.diet, head.names.genotype, head.names.dxg, 
     body.names.diet, body.names.genotype, body.names.dxg, 
     file = "Data/ProcessedData/Hits-Metabolite.Rdata")