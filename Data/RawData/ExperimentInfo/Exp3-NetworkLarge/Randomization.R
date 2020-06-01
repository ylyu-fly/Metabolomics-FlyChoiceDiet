rm(list = ls())

setwd("/Users/Yang/Research/Project/1_Working/Metabolomics/ChoiceDiet_Network/00_ExperimentDesign/")
samples <- c(paste(1, 1:49, sep = "-"),
            paste(2, 1:48, sep = "-"),
            paste(3, 1:50, sep = "-"),
            paste(4, 1:50, sep = "-"))

x <- sample(samples)

b1 <- x[1:100]
b2 <- x[101:197]
b2 <- c(b2, rep("NA", 3))

b1 <- matrix(b1, nrow = 10, byrow = T)
b2 <- matrix(b2, nrow = 10, byrow = T)

write.table(b1, "box1.txt", quote = F, sep = "\t", row.names = F, col.names = F)
write.table(b2, "box2.txt", quote = F, sep = "\t", row.names = F, col.names = F)
