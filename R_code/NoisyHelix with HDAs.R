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
NoisyHelix <- read.arff(paste0(DatFolder, "HDA_NoisyHelix_O.arff")); LabeledAnomalies <- c(9616, 9617, 9618, 9619, 9620, 9621, 9622, 9623, 9624, 9587, 9614, 9588, 9586, 9568, 9604)


# Plot
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5)
points3d(NoisyHelix[LabeledAnomalies, 1:3], col=NoisyHelix[LabeledAnomalies, "color"], size=15, alpha=0.6)
text3d(NoisyHelix[LabeledAnomalies, 1:3], texts=LabeledAnomalies, cex=1, adj=1.5, col=NoisyHelix[LabeledAnomalies, "color"]) 
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5) # Reset



## A. Start with regular density/distance based anomaly detection 

# Do regular global density based anomaly detection with SECODA 

# Equiwidth SECODA
AnomalyTableEW <- SECODA(NoisyHelix, StartHeuristicsAfterIteration=9999)
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=7), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=2*length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  # Only 3 HDAs detected
# This does not work very well. Mostly the isolated non-HDAs are 'detected'.

# Do equidepth SECODA, as this should theoretically work better.
AnomalyTableED <- SECODA(NoisyHelix, BinningMethod = "ED", StartHeuristicsAfterIteration=9999)
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  
# This works a bit better, but the results are not very impressive yet.


# Now do regular distance-based anomaly detection with KNN-AGG.

# Create dummy vars and normalize
NoisyHelixMM <- model.matrix(~x1 + x2 + x3 + color, NoisyHelix)[,2:6]  # Create matrix, and also create dummy variables (using model.matrix)
normalizeNum <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }
NoisyHelixMM <- as.matrix(as.data.frame(lapply(as.data.frame(NoisyHelixMM), normalizeNum)))  # Normalize (rescale numerical values between 0 and 1)
plot3d(NoisyHelixMM[,1:3], col=NoisyHelix$color, size=2.5)

# KNN_AGG distance-based detection (DDoutlier package)
library(DDoutlier)
scoreKNN_AGG <- KNN_AGG(dataset=NoisyHelixMM, k_min=10, k_max=15)

# Plot top isolated anomalies
rankingKNN_AGG <- order(scoreKNN_AGG, decreasing=TRUE) # Order outliers
Ano_ID_anoms <- head(rankingKNN_AGG, n=length(LabeledAnomalies))
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  # No HDAs detected

# Plot more isolated anomalies
rankingKNN_AGG <- order(scoreKNN_AGG, decreasing=TRUE) # Order outliers
Ano_ID_anoms <- head(rankingKNN_AGG, n=2*length(LabeledAnomalies))
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  # Still no HDAs detected
# This also does not work.


# QSP/SOF
library(spoutlier)
set.seed(25)
scoreSOF <- qsp(NoisyHelixMM, sample.size=3000, normalize = TRUE) # Determine outlier scores. A higher number of the sample size (standard is 20) gives a more stable result. For reproducibility purposes, a seed can also be set.
rankingSOF <- order(scoreSOF, decreasing=TRUE) # Order outliers
Ano_ID_anoms <- head(rankingSOF, n=length(LabeledAnomalies))
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  # Hardly any HDAs detected
# Zero detected


# Perhaps with LOF
library(dbscan)
LOFanomalies <- lof(NoisyHelixMM)
rankingLOF <- order(LOFanomalies, decreasing=TRUE) # Order outliers
# Plot anomalies
Ano_ID_anoms <- head(rankingLOF, n=length(LabeledAnomalies))
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)  # Hardly any HDAs detected
# Also not impressive



## B. Now do HDA analysis to target high-density anomalies

# Run SECOHDA (High-density anomalies)
source(paste0(DatFolder, "SECOHDA_AlgorithmIterativePP.R"))
HDAnomalyTableIPP <- SECOHDA(NoisyHelix, QuantileDenominator = 100, TestMode = TRUE)  # Automatisch bepaalde QuantileFilterBoost

# plot first 2 anomalies (or more, if the 10th anomaly score has multiple occurences)
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore <= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)

Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore <= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)

sum(Ano_ID_anoms %in% LabeledAnomalies)
# HDAs are identified correctly



# Run distance-based KNN_AGG from DDoutlier in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunKNN_AGGwithIPP.R"))
HDAnomalyTableKNN_AGGIPP <- KNN_AGGwithIPP(NoisyHelix, QuantileFilterBoost = -9999, k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)

# plot first 2 anomalies (or more, if the 10th anomaly score has multiple occurences)
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=TRUE),n=2), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)

Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)

sum(Ano_ID_anoms %in% LabeledAnomalies)
# All HDAs are identified correctly



# Run distance-based SOF/QSP in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunSOFwithIPP.R"))
set.seed(2)
HDAnomalyTableSOFIPP <- SOFwithIPP(NoisyHelix, SampleSize=3000, Normalize=TRUE, TestMode = TRUE)
# Plot:
Ano_ID_anoms <- HDAnomalyTableSOFIPP[HDAnomalyTableSOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableSOFIPP$HDAscore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)
Ano_ID_anoms <- HDAnomalyTableSOFIPP[HDAnomalyTableSOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableSOFIPP$HDAscore, decreasing=TRUE),n=2*length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)
# This works reasonably well. The results can be improved by increasing the number of iterations, and thereby the precision of the results.


# Run LOF with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunLOFwithIPP.R"))
HDAnomalyTableLOFIPP <- LOFwithIPP(NoisyHelix, k=4, Normalize=TRUE, TestMode = TRUE)
# Plot:
Ano_ID_anoms <- HDAnomalyTableLOFIPP[HDAnomalyTableLOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableLOFIPP$HDAscore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
sum(Ano_ID_anoms %in% LabeledAnomalies)
# With LOF this does not work very well



# Run Harmonic mean SECOHDA (High-density anomalies) using SECODA as underlying algorithm - HMDH
source(paste0(DatFolder, "SECOHDA_AlgorithmHarmonicMean.R"))
HDAnomalyTableHM <- SECOHDA(NoisyHelix, WeightCorrection = "SDEN", StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)
HDAnomalyTableHMSSE <- SECOHDA(NoisyHelix, WeightCorrection = "SSE", StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)
HDAnomalyTableHMNone <- SECOHDA(NoisyHelix, WeightCorrection = "None", StartSECODAHeuristicsAfterIteration = 9999, TestMode = TRUE)


# Run Harmonic mean KNN-AGG (High-density anomalies) using KNN-AGG as underlying algorithm - HMDH
source(paste0(DatFolder, "RunKNN_AGGwithHMDH.R"))
HDAnomalyTableKNN_AGGHMDH <- KNN_AGGwithHMDH(NoisyHelix, WeightCorrection="SDEN", k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
HDAnomalyTableKNN_AGGHMDHSSE <- KNN_AGGwithHMDH(NoisyHelix, WeightCorrection="SSE", k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
HDAnomalyTableKNN_AGGHMDHNone <- KNN_AGGwithHMDH(NoisyHelix, WeightCorrection="None", k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)



### ROC analysis
NoisyHelix$anomalybinary <- 0 # Add binary test value
NoisyHelix[LabeledAnomalies, "anomalybinary"] <- 1  # Give HDAs a value of 1
table(NoisyHelix$anomalybinary)

# Load library
library(pROC)

# ROC equiwidth SECODA
ROC1 <- roc(NoisyHelix$anomalybinary, AnomalyTableEW$AveAnoScore, percent = TRUE) # Calculate ROC and AUC
ROC1 # Show ROC AUC (area under the curve)
plot(ROC1, col="red") # Plot ROC curve
auc(ROC1, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC equidepth SECODA
ROC2 <- roc(NoisyHelix$anomalybinary, AnomalyTableED$AveAnoScore, percent = TRUE) # Calculate ROC and AUC
ROC2 # Show ROC AUC (area under the curve)
plot(ROC2, col="blue", add=TRUE) # Plot ROC curve
auc(ROC2, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA SDEN
ROC3 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableHM$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC3 # Show ROC AUC (area under the curve)
plot(ROC3, col="green", add=TRUE) # Plot ROC curve
auc(ROC3, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC IPP SECOHDA
ROC4 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableIPP$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC4
plot(ROC4, col="purple", add=TRUE) # Plot ROC curve
auc(ROC4, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA SSE
ROC5 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableHMSSE$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC5 # Show ROC AUC (area under the curve)
plot(ROC5, col="seagreen4", add=TRUE) # Plot ROC curve
auc(ROC5, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA without weight correction
ROC6 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableHMNone$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC6 # Show ROC AUC (area under the curve)
plot(ROC6, col="yellowgreen", add=TRUE) # Plot ROC curve
auc(ROC6, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC SOF/QSP / spoutlier
ROC12 <- roc(NoisyHelix$anomalybinary, (max(scoreSOF)-scoreSOF), percent = TRUE) # Calculate ROC and AUC
ROC12
plot(ROC12, col="violetred4", add=TRUE) # Plot ROC curve
auc(ROC12, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC LOF / DBSCAN
ROC13 <- roc(NoisyHelix$anomalybinary, (max(LOFanomalies)-LOFanomalies), percent = TRUE) # Calculate ROC and AUC
ROC13
plot(ROC13, col="violetred3", add=TRUE) # Plot ROC curve
auc(ROC13, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC LOF with IPP
ROC15 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableLOFIPP$HDAscore, percent = TRUE)
ROC15
plot(ROC15, col="black", add=TRUE) # Plot ROC curve
auc(ROC15, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC SOF/QSP with IPP
ROC16 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableSOFIPP$HDAscore, percent = TRUE)
ROC16
plot(ROC16, col="tomato3", add=TRUE) # Plot ROC curve
auc(ROC16, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG (DDoutlier package)
ROC17 <- roc(NoisyHelix$anomalybinary, (max(scoreKNN_AGG)-scoreKNN_AGG), percent = TRUE) # Use reverse scores because all with other algorithms lower scores are more anomalous
ROC17
plot(ROC17, col="tomato3", add=TRUE) # Plot ROC curve
auc(ROC17, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with IPP
ROC18 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableKNN_AGGIPP$HDAscore, percent = TRUE)
ROC18
plot(ROC18, col="navyblue", add=TRUE) # Plot ROC curve
auc(ROC18, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with HMDH SDEN
ROC19 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableKNN_AGGHMDH$HDAscore, percent = TRUE)
ROC19
plot(ROC19, col="turquoise1", add=TRUE) # Plot ROC curve
auc(ROC19, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with HMDH SSE
ROC21 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableKNN_AGGHMDHSSE$HDAscore, percent = TRUE)
ROC21
plot(ROC21, col="violet", add=TRUE) # Plot ROC curve
auc(ROC21, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with HMDH None
ROC22 <- roc(NoisyHelix$anomalybinary, HDAnomalyTableKNN_AGGHMDHNone$HDAscore, percent = TRUE)
ROC22
plot(ROC22, col="violetred", add=TRUE) # Plot ROC curve
auc(ROC22, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension



### Early retrieval analysis and PRC

NoisyHelix$ID <- 1:nrow(NoisyHelix)  # Include explicit ID-field
NoisyHelix$HighDensityAno1 = FALSE; NoisyHelix$HighDensityAno1[LabeledAnomalies] = TRUE  # This way we can re-use existing code

# To be able to use general code, we'll copy the given anomalytable to ChosenAnomalyTable
ChosenAnomalyTable <- HDAnomalyTableIPP  # IPP with SECODA
# ChosenAnomalyTable <- AnomalyTableEW; ChosenAnomalyTable$HDAscore = ChosenAnomalyTable$AveAnoScore  # SECODA equiwidth
# ChosenAnomalyTable <- AnomalyTableED; ChosenAnomalyTable$HDAscore = ChosenAnomalyTable$AveAnoScore  # SECODA equidepth
# ChosenAnomalyTable <- HDAnomalyTableHM  # HMDH SDEN (with SECODA)
# ChosenAnomalyTable <- HDAnomalyTableHMNone  # HMDH without weight correction (with SECODA)
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(scoreKNN_AGG)), HDAscore=(max(scoreKNN_AGG)-scoreKNN_AGG))  # KNN-AGG - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(scoreSOF)), HDAscore=(max(scoreSOF)-scoreSOF))  # SOF - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(LOFanomalies)), HDAscore=(max(LOFanomalies)-LOFanomalies))  # LOF - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGHMDH # HMDH SDEN with KNN-AGG
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGHMDHSSE # HMDH SSE with KNN-AGG
# ChosenAnomalyTable <- HDAnomalyTableSOFIPP # IPP with SOF
# ChosenAnomalyTable <- HDAnomalyTableLOFIPP # IPP with LOF
# ChosenAnomalyTable <- HDAnomalyTableHMSSE  # SSE
# ChosenAnomalyTable <- HDAnomalyTableRFIPP # IPP with random forest
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGIPP

# # Plot (de-comment with CTRL-SHIFT-C)
# Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1), "Ano_ID"]
# plot3d(NoisyHelix[,1:3], col=NoisyHelix$color, size=2.5); points3d(NoisyHelix[Ano_ID_anoms, 1:3], col=NoisyHelix[Ano_ID_anoms, "color"], size=20, alpha=0.6)
# sum(Ano_ID_anoms %in% LabeledAnomalies)

# Do early retrieval analysis:
Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(LabeledAnomalies)), n=1) # Set threshold
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= Threshold1, "Ano_ID"] # Determine anomalies

IdentifiedCases <- Ano_ID_anoms %in% NoisyHelix[NoisyHelix$HighDensityAno1==TRUE, "ID"] # As boolean
sum(IdentifiedCases, na.rm=TRUE) # Number of true anomalies that are identified correctly as extreme / early retrieval anomalies
table(IdentifiedCases)
cat(paste0("True in the table above refers to the number of correctly identified HDA anomalies."))
# Sensitivity/recall: TP/(TP+FN) = the proportion of actual positives that are correctly identified as such (e.g., the percentage of sick people who are correctly identified as having the condition)
cat(paste0("Percentage of correctly identified anomalies (sensitivity/recall): ", sum(IdentifiedCases, na.rm=TRUE)/length(LabeledAnomalies), "."))
# Precision/PPV: = TP/(TP+FP):
cat(paste0("Percentage of denoted anomalies that are indeed anomalies (precision/PPV): ", sum(IdentifiedCases, na.rm=TRUE) / length(Ano_ID_anoms), "."))
cat(paste0("Percentage of denoted anomalies that are indeed anomalies (precision/PPV): ", sum(IdentifiedCases, na.rm=TRUE) / ( (sum(IdentifiedCases, na.rm=TRUE) + (nrow(NoisyHelix)-length(Ano_ID_anoms)) - ( (nrow(NoisyHelix)-length(Ano_ID_anoms)) - (length(Ano_ID_anoms)-sum(IdentifiedCases, na.rm=TRUE)) )  ) ), " (alternative calculation)."))
# Accuracy: (TP+TN)/(TP+FP+FN+TN) = the ratio of the correctly labeled cases to all cases:
cat(paste0("Percentage of correctly classified cases (accuracy): ", (sum(IdentifiedCases, na.rm=TRUE) + ( (nrow(NoisyHelix)-length(Ano_ID_anoms)) - (length(NoisyHelix[NoisyHelix$HighDensityAno1==TRUE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE)) ) ) / nrow(NoisyHelix), "."))  # Denoted negatives: (nrow(NoisyHelix)-length(Ano_ID_anoms))  # From this you should subtract the false negatives, which are: (length(dat1[dat1$HighDensityAno1==TRUE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE))
# Specificity: = TN/(TN+FP):
IdentifiedCases <- Ano_ID_anoms %in% NoisyHelix[NoisyHelix$HighDensityAno1==FALSE, "ID"] # As boolean
sum(IdentifiedCases, na.rm=TRUE) # Number of non-HDA cases that are identified incorrectly as HDA cases
cat(paste0("Percentage of correctly identified normal cases (specificity): ", (length(NoisyHelix[NoisyHelix$HighDensityAno1==FALSE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE)) / length(NoisyHelix[NoisyHelix$HighDensityAno1==FALSE, "ID"]), "."))


# Confusion matrix based evaluation measures
# We already have a ChosenAnomalyTable. Now also choose the ROC object that corresponds with this
ChosenROCobject <- ROC4  # ROC4 = IPP
# ChosenROCobject <- ROC1  # ROC1 = SECODA
# ChosenROCobject <- ROC2  # ROC2 = SECODA ED
# ChosenROCobject <- ROC3  # ROC3 = Harmonic Mean / HMDH SDEN SECOHDA
# ChosenROCobject <- ROC6  # ROC6 = Harmonic Mean no weight correction SECOHDA
# ChosenROCobject <- ROC17  # ROC17 = KNN-AGG
# ChosenROCobject <- ROC12  # ROC12 = SOF
# ChosenROCobject <- ROC13  # ROC13 = LOF
# ChosenROCobject <- ROC16  # ROC16 = IPP with SOF
# ChosenROCobject <- ROC15  # ROC15 = IPP with LOF
# ChosenROCobject <- ROC5  # ROC5 = Harmonic Mean SSE SECOHDA
# ChosenROCobject <- ROC18  # ROC18 = IPP with KNN-AGG

# So now include all cases with the threshold score (which especially with HDAnomalyTableHM leads to many more cases included in Ano_ID_anoms)
HDpred <- rep(0, nrow(ChosenAnomalyTable)) # Create vector with zeros
Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(NoisyHelix[NoisyHelix$HighDensityAno1==TRUE, "HighDensityAno1"])), n=1)  # Determine the threshold
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= Threshold1, "Ano_ID"]
HDpred[ChosenAnomalyTable$Ano_ID %in% Ano_ID_anoms] <- 1
# Alternative would be: HDpred <- rep(0, nrow(ChosenAnomalyTable)); HDpred[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])), n=1)] <- 1
table(HDpred)
table(HDpred, NoisyHelix$anomalybinary) # Confusion matrix
# Get confusion matrix elements from pROC:
coords(ChosenROCobject, Threshold1, ret=c("threshold", "tn", "tp", "fn", "fp"), transpose=TRUE)
# Accuracy from confusion matrix: (TP+TN)/(TP+FP+FN+TN):
( coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("tn"), transpose=TRUE) ) / ( coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("tn"), transpose=TRUE)  + coords(ChosenROCobject, Threshold1, ret=c("fp"), transpose=TRUE)  + coords(ChosenROCobject, Threshold1, ret=c("fn"), transpose=TRUE) )
# Accuracy directly from vectors (Accuracy: Percentage of correctly classified cases)
mean(HDpred == NoisyHelix$anomalybinary)
# Accuracy from pROC:
coords(ChosenROCobject, Threshold1, ret=c("accuracy"), transpose=TRUE) 

# We can use the threshold and ROC also to calculate the other evaluation scores with the pROC package
coords(ChosenROCobject, Threshold1, ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "npv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv", "tn", "tp", "fn", "fp"), transpose=TRUE) # Print all metrics

# Using best Youden threshold
coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "npv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv", "tn", "tp", "fn", "fp"), transpose=TRUE)  # Print all metrics for the best Youden threshold. 


# F1 measure
# The F1 Score is the 2*((precision*recall)/(precision+recall)). It is also called the F Score or the F Measure. Put another way, the F1 score conveys the balance between the precision and the recall.
# Using confusion matrix: F1 = 2TP / (2TP + FP + FN)   
cat(paste0("F1 measure: ", 2*coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) / (2*coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("fp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("fn"), transpose=TRUE)), " (directly via confusion matrix)."));
cat(paste0("F1 measure: ", 2 * ((coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) * coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE))/(coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE)+coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE))), "."))
cat(paste0("F1 measure: ", 2 /  (1/coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE)), " (alternative calculation)."));
# Using best Youden ROC threshold:
cat(paste0("F1 measure: ", 2*coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("tp"), transpose=TRUE) / (2*coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("fp"), transpose=TRUE) + coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("fn"), transpose=TRUE)), " (directly via confusion matrix)."));
cat(paste0("F1 measure: ", 2 / (1/(coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("ppv"), transpose=TRUE)) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("sensitivity"), transpose=TRUE)), " (alternative calculation)."));  


# Calculate GMPR/GMRP measure, based on precision (ppv) and sensitivity (recall)
sqrt( (coords(ChosenROCobject, Threshold1, ret="ppv", transpose=TRUE)) * (coords(ChosenROCobject, Threshold1, ret="sensitivity", transpose=TRUE)) )
sqrt( (coords(ChosenROCobject, Threshold1, ret="ppv", transpose=TRUE)/100) * (coords(ChosenROCobject, Threshold1, ret="sensitivity", transpose=TRUE)/100) )
# Using best Youden threshold:
sqrt( (coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="ppv", transpose=TRUE)) * (coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="sensitivity", transpose=TRUE)) )


# HMFM
# Harmonic mean of four measures/metrics: Sensitivity/Recall, Specificity, Precision/PPV, Accuracy
cat(paste0("HMFM measure: ", (4 / (1/coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("specificity"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("accuracy"), transpose=TRUE))) / 100, "."));
# Using best Youden threshold:
cat(paste0("HMFM measure: ", (4 / (1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("sensitivity"), transpose=TRUE) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("specificity"), transpose=TRUE) + 1/(coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("ppv"), transpose=TRUE)) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("accuracy"), transpose=TRUE))) / 100, "."))


# Calculate Cohen's kappa:
library(fmsb) # ! Note: package fmsb masks roc-object of pROC
Kappa.test(HDpred, NoisyHelix$anomalybinary)   
# Alternatieve manier met zelfde uitkomst:
TP = coords(ChosenROCobject, Threshold1, ret="tp", transpose=TRUE); FP = coords(ChosenROCobject, Threshold1, ret="fp", transpose=TRUE); FN = coords(ChosenROCobject, Threshold1, ret="fn", transpose=TRUE); TN = coords(ChosenROCobject, Threshold1, ret="tn", transpose=TRUE);
matrix1 <- matrix(c(TN, FP, FN, TP),2,2)
# matrix1 <- matrix(c(TP, FP, FN, TN),2,2) # Dit werkt ook (gespiegeld)
matrix1
Kappa.test(matrix1)
# Using best Youden threshold:
TP2 = coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="tp", transpose=TRUE); FP2 = coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="fp", transpose=TRUE); FN2 = coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="fn", transpose=TRUE); TN2 = coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="tn", transpose=TRUE);
matrix2 <- matrix(c(TN2, FP2, FN2, TP2),2,2)
matrix2
Kappa.test(matrix2)


# Calculate Matthews correlation coefficient
# MCC takes into account all four values in the confusion matrix, and a high value (close to 1) means that both classes are predicted well, even if one class is disproportionately under- (or over-) represented. The result value can be between -1 and +1. [https://towardsdatascience.com/the-best-classification-metric-youve-never-heard-of-the-matthews-correlation-coefficient-3bf50a2f3e9a]
source(paste0(DatFolder, "Matthews.Correlation.Coefficient.R"))
# The function should also be available in the package mltools
mcc(TP=TP, FP=FP, TN=TN, FN=FN)  # Using explict true positives etc
mcc(preds=HDpred, actuals=NoisyHelix$anomalybinary)  # Alternative using two binary vectors
# Using best Youden threshold:
mcc(TP=TP2, FP=FP2, TN=TN2, FN=FN2)  # Using explict true positives etc
# The newer version also allows confusion matrices (and uses data.table):
source(paste0(DatFolder, "Matthews.Correlation.Coefficient2.R"))
mcc(TP=TP, FP=FP, TN=TN, FN=FN)  # Using explict true positives etc
mcc(preds=HDpred, actuals=NoisyHelix$anomalybinary)  # Alternative using two binary vectors. (needs data.table)
mcc(confusionM=matrix1) # (needs data.table). Example: https://rdrr.io/cran/mltools/man/mcc.html
# Using best Youden threshold:
mcc(TP=TP2, FP=FP2, TN=TN2, FN=FN2)  # Using explict true positives etc
mcc(confusionM=matrix2) # (needs data.table)


# PRC
library(precrec)
# https://cran.r-project.org/web/packages/precrec/vignettes/introduction.html
table(NoisyHelix$anomalybinary)
NoisyHelix$anomalybinary[NoisyHelix$anomalybinary == 1] <- -1  # Recode anomalies to -1 for precrec package
table(NoisyHelix$anomalybinary)
sscurves <- evalmod(scores = ChosenAnomalyTable$HDAscore, labels = NoisyHelix$anomalybinary)
# For SOF/spoutlier: sscurves <- evalmod(scores = (max(scoreSOF)-scoreSOF), labels = NoisyHelix$anomalybinary)
# For LOF: sscurves <- evalmod(scores = (max(LOFanomalies)-LOFanomalies), labels = NoisyHelix$anomalybinary)
sscurves  # Print info
plot(sscurves) # ROC en PRC
plot(sscurves, "PRC") # Only precision-recall (PRC)
# Plot using autoplot / ggplot2. The autoplot function outputs ROC and Precision-Recall curves by using the ggplot2 package.
library(ggplot2)
autoplot(sscurves)
autoplot(sscurves, "PRC")


