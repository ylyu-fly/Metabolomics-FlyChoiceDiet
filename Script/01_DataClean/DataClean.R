## Subject: Data Clean
## Project: Fly Choice Diet
## Part: 1
## Author: Yang Lyu
## Date created: 12/02/2017
## Date modified: 05/31/2020
## May require R 4.0

# Environment Settings ----------------------------------------------------
library(gtools)
library(impute)

# Data Input --------------------------------------------------------------
## file name
head.id <- "FlyChoiceDiet_Head"
body.id <- "FlyChoiceDiet_Body"

head.metabolite.file <- paste("Data/RawData/RawData-", head.id, "_Metabolite.txt", sep = "")
body.metabolite.file <- paste("Data/RawData/RawData-", body.id, "_Metabolite.txt", sep = "")

head.qc.file <- paste("Data/RawData/RawData-", head.id, "_QC.txt", sep = "")
body.qc.file <- paste("Data/RawData/RawData-", body.id, "_QC.txt", sep = "")

head.sampleinfo.file <- paste("Data/RawData/Info-Sample-", head.id, ".txt", sep = "")
body.sampleinfo.file <- paste("Data/RawData/Info-Sample-", body.id, ".txt", sep = "")

## Read data
head.metabolite.data <- read.table(head.metabolite.file, sep = "\t", header = T)
body.metabolite.data <- read.table(body.metabolite.file, sep = "\t", header = T)

head.qc.data <- read.table(head.qc.file, sep = "\t", header = T)
body.qc.data <- read.table(body.qc.file, sep = "\t", header = T)

head.sampleinfo.data <- read.table(head.sampleinfo.file, sep = "\t", header = T)
body.sampleinfo.data <- read.table(body.sampleinfo.file, sep = "\t", header = T)

# Data Cleaning -----------------------------------------------------------
## Check Time-Series Effects
par(mfrow=c(5, 1), mar = c(4, 4, 1, 1)+0.5) 
## Head
for(i in 1:206) {
    if(!all(is.na(head.metabolite.data[i,-1]))) {
        plot(1:76, head.metabolite.data[i, -1], 
             main = head.metabolite.data[i, 1],
             xlab = "", ylab = "Abuandance", xlim = c(1, 76), las = 2)
    }
}
## Body
for(i in 1:206) {
    if(!all(is.na(body.metabolite.data[i,-1]))) {
        plot(1:78, body.metabolite.data[i, -1], 
             main = body.metabolite.data[i, 1],
             xlab = "", ylab = "Abuandance", xlim = c(1, 78), las = 2)
    }
}

## Reformat sample name
head.sampleinfo.data$SampleID <- paste("X", head.sampleinfo.data$SampleID, sep = "") 
body.sampleinfo.data$SampleID <- paste("X", body.sampleinfo.data$SampleID, sep = "")

### Above step only required for Sample ID started with numbers
head.colnames <- head.sampleinfo.data$SampleName[match(names(head.metabolite.data), head.sampleinfo.data$SampleID)]
names(body.metabolite.data) <- gsub("[.]","",names(body.metabolite.data))
body.colnames <- body.sampleinfo.data$SampleName[match(names(body.metabolite.data), body.sampleinfo.data$SampleID)]

names(head.metabolite.data)[!is.na(head.colnames)] <- head.colnames[!is.na(head.colnames)]
names(body.metabolite.data)[!is.na(body.colnames)] <- body.colnames[!is.na(body.colnames)]

## Test whether metabolite and qc samples have the same rownames
length(match(head.metabolite.data[ , 1], head.qc.data[ , 1]))
length(match(head.metabolite.data[ , 2], head.qc.data[ , 2]))

length(match(body.metabolite.data[ , 1], body.qc.data[ , 1]))
length(match(body.metabolite.data[ , 2], body.qc.data[ , 2]))

### Continue if 205 metabolites in total

## Clean metabolite data
### Reorder the column names based on sample ID
head.metabolite.data <- head.metabolite.data[ , c(1, 1 + mixedorder(names(head.metabolite.data[ , -1])))]
body.metabolite.data <- body.metabolite.data[ , c(1, 1 + mixedorder(names(body.metabolite.data[ , -1])))]

### Reorder the row names based on alphabet
head.metabolite.data <- head.metabolite.data[mixedorder(head.metabolite.data[ , 1]), ]
body.metabolite.data <- body.metabolite.data[mixedorder(body.metabolite.data[ , 1]), ]

### Convert data frame to numeric (matrix)
head.metabolite.rows <- head.metabolite.data[ , 1]
body.metabolite.rows <- body.metabolite.data[ , 1]

head.metabolite.cols <- names(head.metabolite.data[ , -1])
body.metabolite.cols <- names(body.metabolite.data[ , -1])

head.metabolite.data <- sapply(head.metabolite.data[ , -1], function(x) as.numeric(as.character(x)))
body.metabolite.data <- sapply(body.metabolite.data[ , -1], function(x) as.numeric(as.character(x)))

rownames(head.metabolite.data) <- head.metabolite.rows
rownames(body.metabolite.data) <- body.metabolite.rows

colnames(head.metabolite.data) <- head.metabolite.cols
colnames(body.metabolite.data) <- body.metabolite.cols

## Clean QC data
### Clean head QC data
head.qc.rows <- head.qc.data[ , 1]
head.qc.cols <- names(head.qc.data[ , -1])
### Convert data frame to numeric (matrix)
head.qc.data <- sapply(head.qc.data[ , -1], function(x) as.numeric(as.character(x)))
rownames(head.qc.data) <- head.qc.rows
colnames(head.qc.data) <- head.qc.cols
dim(head.qc.data)

### Clean body QC data
### Subset QC internal control
body.qc.rows <- body.qc.data[ , 1]
body.qc.i.cols <- names(body.qc.data[ , 2:10])
body.qc.i.data <- sapply(body.qc.data[ , 2:10], function(x) as.numeric(as.character(x))) 
rownames(body.qc.i.data) <- body.qc.rows
colnames(body.qc.i.data) <- body.qc.i.cols
dim(body.qc.i.data)
### Subset QC sample control
body.qc.s.cols <- names(body.qc.data[ , 11:19])
body.qc.s.data <- sapply(body.qc.data[ , 11:19], function(x) as.numeric(as.character(x))) 
rownames(body.qc.s.data) <- body.qc.rows
colnames(body.qc.s.data) <- body.qc.s.cols
dim(body.qc.s.data)

# Data QC -----------------------------------------------------------------
## Head Sample & QC Data Distribution
### Plot Sample and log10 Sample
#### Modify plot paramters
par(mfrow = c(1,2), mar = c(4, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.2)
#### Look at distribution of raw and log-transformed data
hist(head.metabolite.data, 50, main = "Raw") 
hist(log10(head.metabolite.data), 50, main = "Log10 transformation")
#### After log-transformation, data look normal distributed
#### Use log10 QC for further analysis
head.metabolite.data.log <- log10(head.metabolite.data)
head.metabolite.data.log[head.metabolite.data.log == 0] <- NA

### Plot QC
par(mfrow = c(1,3), mar = c(4, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.2)
#### Calculate signal-to-noise ratio
head.qc.snr <- apply(head.qc.data, 1, mean, na.rm = T) / apply(head.qc.data, 1, sd, na.rm = T) 
#### Calculate missing metabolites
head.qc.missing <- apply(is.na(head.qc.data), 1, sum)
#### Calculate means by log-transformed metabolites
head.metabolite.means <- apply(head.metabolite.data.log, 1, mean, na.rm = T)
#### Plot Signal-to-Noise Ratio (SNR)
hist(log10(head.qc.snr), 50)
abline(v = 1, col = "red", lty = 2, lwd = 2)
#### Plot means vs missing number
plot(c(0, 9), tapply(head.metabolite.means, head.qc.missing, mean, na.rm = T), 
     ylab="mean metabolite val", pch = 16)
#### Missing QC metabolites are significantly expressed lower
#### Plot missing number vs SNR
plot(head.qc.missing, head.qc.snr, pch = 16, cex = .5)

## Body
### Plot Sample and log10 Sample
#### Modify plot paramters
par(mfrow = c(1,2), mar = c(4, 4, 1, 0) + 0.5, mgp = c(2, 0, 0) + 0.2)
#### Look at distribution of raw and log-transformed data
hist(body.metabolite.data, 50, main = "Raw") 
hist(log10(body.metabolite.data), 50, main = "Log10 transformation")
#### After log-transformation, data look normal distributed
#### Use log10 QC for further analysis
body.metabolite.data.log <- log10(body.metabolite.data)
body.metabolite.data.log[body.metabolite.data.log == 0] <- NA

# Data Removal ------------------------------------------------------------
## Missing Data Removal
### Head
#### Calculate frequency of zeros by metabolite and sample
head.metabolite.zeros <- apply(is.na(head.metabolite.data.log), 1, sum)
table(head.metabolite.zeros)
head.sample.zeros <- apply(is.na(head.metabolite.data.log), 2, sum)
table(head.sample.zeros)
K  <- 8 # K = 8 for 76 samples
head.data.cut <- head.metabolite.data.log[head.metabolite.zeros < K, ]

### Body
#### Calculate frequency of zeros by metabolite and sample
body.metabolite.zeros <- apply(is.na(body.metabolite.data.log), 1, sum)
table(body.metabolite.zeros)
body.sample.zeros <- apply(is.na(body.metabolite.data.log), 2, sum)
table(body.sample.zeros)
K  <- 8 # K = 8 for 78 samples
body.data.cut <- body.metabolite.data.log[body.metabolite.zeros < K, ]

## Internal Controls Removal
rownames(head.data.cut) <- sub(" \\(.*)", "", rownames(head.data.cut))
rownames(body.data.cut) <- sub(" \\(.*)", "", rownames(body.data.cut))

head.data.cut <- head.data.cut[-match(c("1C13-Lactate", "213C-Tyrosine", "Epinephrine/Normetanephrine"), 
                                      rownames(head.data.cut)), ]
body.data.cut <- body.data.cut[-match(c("1C13-Lactate", "213C-Tyrosine", "Epinephrine/Normetanephrine"), 
                                      rownames(body.data.cut)), ]

# Data Imputation ---------------------------------------------------------
## Impute using the KNN algorithm. (Uses 'impute' package in 'Bioconductor')
head.data.imputed <- (impute.knn(head.data.cut))$data
body.data.imputed <- (impute.knn(body.data.cut))$data

# Data Output -------------------------------------------------------------
head.result.file <- "Data/ProcessedData/ImputedData-FlyChoiceDiet_Head.RDS"
body.result.file <- "Data/ProcessedData/ImputedData-FlyChoiceDiet_Body.RDS"

saveRDS(head.data.imputed, file = head.result.file)
saveRDS(body.data.imputed, file = body.result.file)