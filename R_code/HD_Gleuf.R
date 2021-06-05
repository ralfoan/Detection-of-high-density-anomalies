# Ralph Foorthuis - 2020
# This file contains examples of detection of high-density anomalies.

# See this publication for more information:
# Foorthuis R.M. (2020). Algorithmic Frameworks for the Detection of High-Density Anomalies. Accepted for presentation at IEEE SSCI CIDM 2020 (Symposium on Computational Intelligence in Data Mining), December 2020, Canberra Australia. 



##### Initialization

# Set folder for data files and R code
DatFolder <- "E:/Datasets and example code/"  # Set your own folder path here. Don't forget to end with a slash character.

# Load libraries
library(foreign) # For loading datasets
library(rgl) # If you have rgl installed, then you can inspect a 3D plot 

# If you have installed the package SECODA you can load it
library(SECODA)

# Alternatively, you can load the SECODA functionality from file
# source(paste0(DatFolder, "SECODA.R")) # This is the regular version, which uses the data.table package for good time performance

# SECODA treats numeric and categorical data differently. Before running SECODA() make sure that the data types are declared correctly. Numeric data should be 'integer' or 'numeric', whereas categorical data should be 'factor', 'logical' or 'character'. 


# Load data
datset <- read.arff(paste0(DatFolder, "HDA_Gleuf_D.arff")); NoOfHDAs=6; AnosStartAt = 25848

# Plot 2D + color
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"]))

# Plot 3D + color
require(rgl)
plot3d(datset[,], col=datset$Color, size=2)
# Show anomalies
points3d(datset[AnosStartAt:nrow(datset), 1:3], color=datset[AnosStartAt:nrow(datset), "Color"], size=10, alpha=0.6) # Plot manually inserted anomalies

# Run regular SECODA (without heuristics for optimal precision)
AnomalyTableEW <- SECODA(datset, StartHeuristicsAfterIteration = 9999)

# Plot most extreme anomalies (based on number of true cases)
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 0 HDAs detected, only isolated cases
# Plot 20 most extreme anomalies
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=20), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 0 HDAs detected
# Plot 40 most extreme anomalies
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=40), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 0 HDAs detected
# Plot 100 most extreme anomalies
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=100), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 1 HDA detected
# Only one HDA is detected, after more than 100 early retrieval cases. The other HDAs still remain disguised (masked) by the many Extreme Value Anomalies and Multidimensional Numerical Anomalies.
# Regular low-density (or distance-based, which in terms of functional results is very similar to equiwidth SECODA) is not suited for detecting HDAs, especially not in noisy datasets (that feature many isolated cases).


# Run equidepth SECODA
AnomalyTableED <- SECODA(datset, BinningMethod = "ED", StartHeuristicsAfterIteration = 9999)
# Plot 6
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 4 of out 6
# Plot 10
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=10), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 4 of out 6
# Plot 20
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=20), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 5 of out 6
# Plot 40
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=40), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt])  # 5 of out 6
# Works better than regular equiwidth SECODA, although difficult to identify the last one


# Run IPP SECOHDA
source(paste0(DatFolder, "SECOHDA_AlgorithmIterativePP.R"))
HDAnomalyTableIPP <- SECOHDA(datset, StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)

# plot top HDA anomalies - by selecting the lowest HDA scores
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore <= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 6 out of 6 detected

# All of the HDAs are detected.

# Interestingly, from the same results table you can also plot the extremely isolated anomalies, which have the HIGHEST scores (not the lowest)
# Plot first 9 Extreme Value Anomalies - by selecting the highest HDA scores (the other end of the score vector)
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore >= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=FALSE),n=9), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
# plot first 20 Extreme Value Anomalies
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore >= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=FALSE),n=20), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
# plot first 100 Extreme Value Anomalies
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore >= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=FALSE),n=100), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
# As can be seen, the IPP HDA score vector gives easy access to HDA anomalies on the one side (lowest scores) and 'true' extreme value and multidimensional numerical (isolated) anomalies on the other side (highest scores).

# Note here that both ends of the score vector identify (different classes of) anomalies: HDAs or isolated cases.



# Run Harmonic mean SECOHDA (High-density anomalies) using SECODA as underlying algorithm - HMDH
source(paste0(DatFolder, "SECOHDA_AlgorithmHarmonicMean.R"))
HDAnomalyTableHM <- SECOHDA(datset, WeightCorrection = "SDEN", StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)
HDAnomalyTableHMSSE <- SECOHDA(datset, WeightCorrection = "SSE", StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)
HDAnomalyTableHMNone <- SECOHDA(datset, WeightCorrection = "None", StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)

# plot top HDA anomalies (SDEN) - by selecting the lowest HDA scores
Ano_ID_anoms <- HDAnomalyTableHM[HDAnomalyTableHM$HDAscore <= head(tail(sort(HDAnomalyTableHM$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 3 of out 6
# plot lowest 10 cases
Ano_ID_anoms <- HDAnomalyTableHM[HDAnomalyTableHM$HDAscore <= head(tail(sort(HDAnomalyTableHM$HDAscore, decreasing=TRUE),n=10), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 5 of out 6
# So SDEN also scores reasonably well: most HDAs detected within the first 10 early retrieval cases. But the last one is detected as the 56th case. 


# plot top HDA anomalies (SSE) - by selecting the lowest HDA scores
Ano_ID_anoms <- HDAnomalyTableHMSSE[HDAnomalyTableHMSSE$HDAscore <= head(tail(sort(HDAnomalyTableHMSSE$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 2 out of 6


# plot top HDA anomalies (none/weightless) - by selecting the lowest HDA scores
Ano_ID_anoms <- HDAnomalyTableHMNone[HDAnomalyTableHMNone$HDAscore <= head(tail(sort(HDAnomalyTableHMNone$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 2 out of 6


# Run distance-based anomaly detection (e.g. with dummy variables)
# First make dummy variables via a model matrix
datsetMM <- model.matrix(~x1 + x2 + x3 + Color, datset)[,2:5]  # Create matrix (expected by SOF/spoutlier), and also create dummy variables (using model.matrix)
summary(datsetMM)
normalizeNum <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }  # do_knno and LOF doesn't seem to normalize, so: # Normalization function to rescale numerical attributes to the range 0 and 1: (X - min(X))/(max(X) - min(X))
datsetMMb <- as.matrix(as.data.frame(lapply(as.data.frame(datsetMM), normalizeNum)))
plot3d(datsetMMb[,1:3], col=datset$Color, size=3) # Verify. Indeed all between 0 and 1. 

# KNN_AGG distance-based detection (DDoutlier package)
library(DDoutlier)
scoreKNN_AGG <- KNN_AGG(dataset=datsetMMb, k_min=10, k_max=15)
rankingKNN_AGG <- order(scoreKNN_AGG, decreasing=TRUE) # Order outliers
max(scoreKNN_AGG)  # Most extreme anomaly.
which(scoreKNN_AGG==max(scoreKNN_AGG)) # That has this index
head(rankingKNN_AGG) # Which is indeed on top of the ranking.
# Plot top anomalies
Ano_ID_anoms <- head(rankingKNN_AGG, n=20)
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 0 out of 6
# Plot 40
Ano_ID_anoms <- head(rankingKNN_AGG, n=40)
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 1 out of 6
# Plot 100
Ano_ID_anoms <- head(rankingKNN_AGG, n=100)
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 2 out of 6
# Similar to standard SECODA: mainly isolated cases are identified as anomalies.


# Run distance-based KNN_AGG from DDoutlier in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunKNN_AGGwithIPP.R"))
HDAnomalyTableKNN_AGGIPP <- KNN_AGGwithIPP(datset, QuantileFilterBoost = -9999, k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
# plot top HDA anomalies - by selecting the lowest HDA scores
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=TRUE),n=6), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=2.5); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=12, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 6 out of 6 detected

# Because this is an IPP result, the other end of the score vector contains the isolated cases. 
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore >= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=FALSE),n=9), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
# plot first 20 Extreme Value Anomalies
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore >= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=FALSE),n=20), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
# plot first 100 Extreme Value Anomalies
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore >= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=FALSE),n=100), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
# As can be seen, the IPP HDA score vector gives easy access to HDA anomalies on the one side (lowest scores) and 'true' extreme value and multidimensional numerical (isolated) anomalies on the other side (highest scores).


# Run distance-based KNN_AGG from DDoutlier in combination with HMDH SDEN (harmonic mean SDEN)
source(paste0(DatFolder, "RunKNN_AGGwithHMDH.R"))
HDAnomalyTableKNN_AGGHMDH <- KNN_AGGwithHMDH(datset, WeightCorrection="SDEN", k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
Ano_ID_anoms <- HDAnomalyTableKNN_AGGHMDH[HDAnomalyTableKNN_AGGHMDH$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGHMDH$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 1 out of 6 detected
# Plot first 10
Ano_ID_anoms <- HDAnomalyTableKNN_AGGHMDH[HDAnomalyTableKNN_AGGHMDH$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGHMDH$HDAscore, decreasing=TRUE),n=10), n=1), "Ano_ID"]
plot3d(datset[,], col=datset$Color, size=3); points3d(datset[Ano_ID_anoms, 1:3], color=datset[Ano_ID_anoms, "Color"], size=10, alpha=0.6)
plot(datset[,c(1,3)], pch=16, cex = 0.5, col=as.character(datset[, "Color"])); points(datset[Ano_ID_anoms, c(1,3)], col="lightskyblue", pch=16, cex = 2); points(datset[Ano_ID_anoms, c(1,3)], col=as.character(datset[Ano_ID_anoms, "Color"]), pch=16, cex = 0.5)
length(Ano_ID_anoms[Ano_ID_anoms >= AnosStartAt]) # 1 out of 6 detected
# This does not work very well with KNN-AGG


  
# Run SOF/QSP sampling-based analysis:
library(spoutlier)
set.seed(6)
scoreSOF <- qsp(datsetMM, sample.size=3000) # Determine outlier scores. A higher number of the sample size (standard is 20) gives a more stable result. For reproducibility purposes, a seed can also be set.
rankingSOF <- order(scoreSOF, decreasing=TRUE) # Order outliers
# Check results
max(scoreSOF)  # Most extreme anomaly.
which(scoreSOF==max(scoreSOF)) # That has this index
head(rankingSOF) # Which is indeed on top of the ranking.
# Plot 6 anomalies
plot3d(datset[,], col=datset$Color, size=3)
Ano_ID_anoms <- head(rankingSOF)
# These are the values: datset[Ano_ID_anoms, 1:4]; datsetMM[Ano_ID_anoms,]  # These are indeed consistent
points3d(datset[Ano_ID_anoms, 1:3], color=datset$Color[Ano_ID_anoms], size=12)
# Plot more anomalies
Ano_ID_anoms <- head(rankingSOF, n=15)
plot3d(datset[,1:3], col=datset$Color); points3d(datset[Ano_ID_anoms, 1:3], color=datset$Color[Ano_ID_anoms], size=12)
# Plot more anomalies
Ano_ID_anoms <- head(rankingSOF, n=40)
plot3d(datset[,1:3], col=datset$Color); points3d(datset[Ano_ID_anoms, 1:3], color=datset$Color[Ano_ID_anoms], size=12)
# Is very similar to regular equiwidth SECODA and KNN-AGG. Performance with sufficient number of iterations slightly better than SECODA, and somewhat poorer than SECODA equidepth



