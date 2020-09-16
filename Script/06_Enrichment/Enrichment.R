## Subject: Enrichment
## Project: Fly Choice Diet
## Part: 6
## Author: Yang Lyu
## Date created: 05/31/2020
## Date modified: 05/31/2020
## May require R 4.0

# Environment Settings ----------------------------------------------------
library(MetaboAnalystR)

# Data Input --------------------------------------------------------------
head.metabolite.all.df <- read.table("Data/ProcessedData/Ref-Metabolite_head.txt", 
                                     sep = "\t", header = F)
body.metabolite.all.df <- read.table("Data/ProcessedData/Ref-Metabolite_body.txt", 
                                     sep = "\t", header = F)

head.metabolite.dxg.df <- read.table("Data/ProcessedData/Hits-Metabolite_dxg_head.txt", 
                                     sep = "\t", header = F)
body.metabolite.dxg.df <- read.table("Data/ProcessedData/Hits-Metabolite_dxg_body.txt", 
                                     sep = "\t", header = F)

head.metabolite.genotype.df <- read.table("Data/ProcessedData/Hits-Metabolite_genotype_head.txt", 
                                          sep = "\t", header = F)
body.metabolite.genotype.df <- read.table("Data/ProcessedData/Hits-Metabolite_genotype_body.txt", 
                                          sep = "\t", header = F)

head.metabolite.diet.df <- read.table("Data/ProcessedData/Hits-Metabolite_diet_head.txt", 
                                      sep = "\t", header = F)
body.metabolite.diet.df <- read.table("Data/ProcessedData/Hits-Metabolite_diet_body.txt", 
                                      sep = "\t", header = F)

head.metabolite.all <- head.metabolite.all.df$V1
body.metabolite.all <- body.metabolite.all.df$V1

head.metabolite.dxg <- head.metabolite.dxg.df$V1
body.metabolite.dxg <- body.metabolite.dxg.df$V1

head.metabolite.genotype <- head.metabolite.genotype.df$V1
body.metabolite.genotype <- body.metabolite.genotype.df$V1

head.metabolite.diet <- head.metabolite.diet.df$V1
body.metabolite.diet <- body.metabolite.diet.df$V1

# Enrichment Analyses -----------------------------------------------------
## Initiate Data Objects
head.all.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.all.mSet <- InitDataObjects("conc", "pathora", FALSE)

head.dxg.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.dxg.mSet <- InitDataObjects("conc", "pathora", FALSE)

head.diet.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.diet.mSet <- InitDataObjects("conc", "pathora", FALSE)

head.genotype.mSet <- InitDataObjects("conc", "pathora", FALSE)
body.genotype.mSet <- InitDataObjects("conc", "pathora", FALSE)

## Set up mSetObj with the list of compounds
head.all.mSet <- Setup.MapData(head.all.mSet, head.metabolite.all)
body.all.mSet <- Setup.MapData(body.all.mSet, body.metabolite.all)

head.dxg.mSet <- Setup.MapData(head.dxg.mSet, head.metabolite.dxg)
body.dxg.mSet <- Setup.MapData(body.dxg.mSet, body.metabolite.dxg)

head.genotype.mSet <- Setup.MapData(head.genotype.mSet, head.metabolite.genotype)
body.genotype.mSet <- Setup.MapData(body.genotype.mSet, body.metabolite.genotype)

head.diet.mSet <- Setup.MapData(head.diet.mSet, head.metabolite.diet)
body.diet.mSet <- Setup.MapData(body.diet.mSet, body.metabolite.diet)

## Cross reference list of compounds against libraries 
head.all.mSet <- CrossReferencing(head.all.mSet, "name")
body.all.mSet <- CrossReferencing(body.all.mSet, "name")

head.dxg.mSet <- CrossReferencing(head.dxg.mSet, "name")
body.dxg.mSet <- CrossReferencing(body.dxg.mSet, "name")

head.genotype.mSet <- CrossReferencing(head.genotype.mSet, "name")
body.genotype.mSet <- CrossReferencing(body.genotype.mSet, "name")

head.diet.mSet <- CrossReferencing(head.diet.mSet, "name")
body.diet.mSet <- CrossReferencing(body.diet.mSet, "name")

## Create the mapping results table
head.all.mSet <- CreateMappingResultTable(head.all.mSet)
body.all.mSet <- CreateMappingResultTable(body.all.mSet)

head.dxg.mSet <- CreateMappingResultTable(head.dxg.mSet)
body.dxg.mSet <- CreateMappingResultTable(body.dxg.mSet)

head.genotype.mSet <- CreateMappingResultTable(head.genotype.mSet)
body.genotype.mSet <- CreateMappingResultTable(body.genotype.mSet)

head.diet.mSet <- CreateMappingResultTable(head.diet.mSet)
body.diet.mSet <- CreateMappingResultTable(body.diet.mSet)

## KEGG Enrichment Test
head.all.mSet <- SetKEGG.PathLib(head.all.mSet, "dme", "current")
body.all.mSet <- SetKEGG.PathLib(body.all.mSet, "dme", "current")

head.dxg.mSet <- SetKEGG.PathLib(head.dxg.mSet, "dme", "current")
body.dxg.mSet <- SetKEGG.PathLib(body.dxg.mSet, "dme", "current")

head.genotype.mSet <- SetKEGG.PathLib(head.genotype.mSet, "dme", "current")
body.genotype.mSet <- SetKEGG.PathLib(body.genotype.mSet, "dme", "current")

head.diet.mSet <- SetKEGG.PathLib(head.diet.mSet, "dme", "current")
body.diet.mSet <- SetKEGG.PathLib(body.diet.mSet, "dme", "current")

head.all.mSet <- SetMetabolomeFilter(head.all.mSet, F)
body.all.mSet <- SetMetabolomeFilter(body.all.mSet, F)

head.dxg.mSet <- SetMetabolomeFilter(head.dxg.mSet, F)
body.dxg.mSet <- SetMetabolomeFilter(body.dxg.mSet, F)

head.genotype.mSet <- SetMetabolomeFilter(head.genotype.mSet, F)
body.genotype.mSet <- SetMetabolomeFilter(body.genotype.mSet, F)

head.diet.mSet <- SetMetabolomeFilter(head.diet.mSet, F)
body.diet.mSet <- SetMetabolomeFilter(body.diet.mSet, F)

head.all.mSet <- CalculateOraScore(head.all.mSet, "rbc", "hyperg")
body.all.mSet <- CalculateOraScore(body.all.mSet, "rbc", "hyperg")

head.dxg.mSet <- CalculateOraScore(head.dxg.mSet, "rbc", "hyperg")
body.dxg.mSet <- CalculateOraScore(body.dxg.mSet, "rbc", "hyperg")

head.genotype.mSet <- CalculateOraScore(head.genotype.mSet, "rbc", "hyperg")
body.genotype.mSet <- CalculateOraScore(body.genotype.mSet, "rbc", "hyperg")

head.diet.mSet <- CalculateOraScore(head.diet.mSet, "rbc", "hyperg")
body.diet.mSet <- CalculateOraScore(body.diet.mSet, "rbc", "hyperg")