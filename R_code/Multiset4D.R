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
dat1 <- read.arff(paste0(DatFolder, "HDA_Dataset_Multiset4D.arff"))

# Count anomalies
cat(paste0("There are ", nrow(dat1[dat1$AnomalyType1 != 0, ]), " true anomalies."))
cat(paste0("There are ", length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"]), " high-density anomalies and ", length(dat1[dat1$AnomalyType1!="0" & dat1$HighDensityAno1==FALSE, "AnomalyType1"]), " low-density anomalies."))
NumberOfHDAs = length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])


# Plot the data
plot3d(dat1[,1:3], col=dat1$color)

# Plot the data including anomalies
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[dat1$AnomalyType1!="0", 1:3], col=dat1[dat1$AnomalyType1!="0", "color"], size=12, alpha=0.6) # Show all labeled anomalies
text3d(dat1[which(dat1[,"AnomalyType1"] != "0"), 1:3], texts=which(dat1[,"AnomalyType1"] != "0"), cex=0.7, adj=1.3, col=dat1[which(dat1[,"AnomalyType1"] != "0"), "color"])  # Show ID's
text3d(dat1[which(dat1[,"AnomalyType1"] != "0"), 1:3], texts=paste(dat1[dat1$AnomalyType1 != 0, "AnomalyType1"], " - ", dat1[dat1$AnomalyType1 != 0, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.8), col=dat1[which(dat1[,"AnomalyType1"] != "0"), "color"])  # Show anomaly types
# Note that a couple of extreme value and isolated anomalies are not labelled, because they were not manually inserted in the dataset creation phase.

# Show all manually inserted HD anomalies
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[dat1$HighDensityAno1==TRUE, 1:3], col=dat1[dat1$HighDensityAno1==TRUE, "color"], size=14, alpha=0.6) 
text3d(dat1[dat1$HighDensityAno1==TRUE, 1:3], texts=which(dat1[,"HighDensityAno1"] == TRUE), cex=0.7, adj=1.3, col=dat1[dat1$HighDensityAno1==TRUE, "color"])  # Show ID's


# Check amount of anomalies
table(dat1$color)
table(dat1$AnomalyType1)
table(dat1$HighDensityAno1)
library(plyr)
ddply(dat1, c('AnomalyType1', 'HighDensityAno1'), function(x) count=nrow(x))
sum(ddply(dat1, c('AnomalyType1', 'HighDensityAno1'), function(x) count=nrow(x))$V1) # This is indeed the total amount of cases


#### Perform anomaly detection


# Run algorithm in standard equiwidth mode
AnomalyTableEW <- SECODA(dat1[,1:4])

# plot most extreme anomalies identified by algorithm
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=10)
# Some of these are HDA's, but, as expected from a general-purpose anomaly detection algorithm, many isolated anomalies are also identified.
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=NumberOfHDAs*2), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=10)
# Plot some more

# Run algorithm in equidepth mode, which should theoretically favor high-density anomalies at the cost of extreme value anomalies
AnomalyTableED <- SECODA(dat1[,1:4], BinningMethod = "ED")

# plot first anomalies
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=10)
# Still many isolated cases, although it is known that this setting should identify more high-density anomalies


# Run distance-based anomaly detection (e.g. with dummy variables)
# First create dummy variables via a model matrix
dat1MM <- model.matrix(~x1 + x2 + x3 + color, dat1)[,2:17]  # Create matrix (expected by SOF/spoutlier), and also create dummy variables (using model.matrix)
summary(dat1MM)
# Run analysis:
# First try the basic algorithm. Load basic nearest neighbor outlier detection method from Ramaswamy et al. (2000):
source(paste0(DatFolder, "kNNo.R"))
Ano_ID_anoms <- do_knno(dat1MM, 3, NumberOfHDAs)  # k=3 and ask for the top 22 outliers (NumberOfHDAs)
Ano_ID_anoms # These are the outliers
dat1[Ano_ID_anoms,] # This is the data
points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# The results make a lot of sense if you are looking for isolated cases rather than HDAs. But in any case, we need gradual scores, and also a more scalable algorithm.
library(spoutlier)
set.seed(25)
scoreSOF <- qsp(dat1MM, sample.size=3000, normalize = TRUE) # Determine outlier scores. A higher number of the sample size (standard is 20) gives a more stable result. For reproducibility purposes, a seed can also be set.
rankingSOF <- order(scoreSOF, decreasing=TRUE) # Order outliers
# Plot 6 anomalies
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- head(rankingSOF)
# These are the values: dat1[Ano_ID_anoms, 1:4]; dat1MM[Ano_ID_anoms,]  # These are indeed consistent
points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# Plot more anomalies
Ano_ID_anoms <- head(rankingSOF, n=NumberOfHDAs)
plot3d(dat1[,1:3], col=dat1$color); points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# Plot more anomalies
Ano_ID_anoms <- head(rankingSOF, n=NumberOfHDAs*2)
plot3d(dat1[,1:3], col=dat1$color); points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# Is very similar to regular equiwidth SECODA. Performance with sufficient number of iterations slightly better than SECODA, and somewhat poorer than SECODA equidepth


# Run distance-based SOF/QSP in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunSOFwithIPP.R"))
set.seed(25)
HDAnomalyTableSOFIPP <- SOFwithIPP(dat1[,1:4], SampleSize=3000, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- HDAnomalyTableSOFIPP[HDAnomalyTableSOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableSOFIPP$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
# This works well because of the IPP framework


# KNN_AGG distance-based detection (DDoutlier package)
# First normalize data
normalizeNum <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }  # LOF doesn't seem to normalize, so: # Normalization function to rescale numerical attributes to the range 0 and 1: (X - min(X))/(max(X) - min(X))
dat1MMb <- as.matrix(as.data.frame(lapply(as.data.frame(dat1MM), normalizeNum)))
plot3d(dat1MMb[,1:3], col=dat1$color) # Verify. Indeed all between 0 and 1.
library(DDoutlier)
scoreKNN_AGG <- KNN_AGG(dataset=dat1MMb, k_min=10, k_max=15)
rankingKNN_AGG <- order(scoreKNN_AGG, decreasing=TRUE) # Order outliers
# Plot top anomalies
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- head(rankingKNN_AGG, n=NumberOfHDAs)
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
sum(Ano_ID_anoms %in% which(dat1$HighDensityAno1==TRUE)) # Only 2 HDAs detected
intersect(Ano_ID_anoms, which(dat1$HighDensityAno1==TRUE)) # Namely cases 7839 and 7853
text3d(dat1[Ano_ID_anoms, 1:3], texts=paste(Ano_ID_anoms, " - ", dat1[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.8), col=dat1[Ano_ID_anoms, "color"])  # Show anomaly types

# Run distance-based KNN_AGG from DDoutlier in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunKNN_AGGwithIPP.R"))
HDAnomalyTableKNN_AGGIPP <- KNN_AGGwithIPP(dat1[,1:4], QuantileFilterBoost = -9999, k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
text3d(dat1[Ano_ID_anoms, 1:3], texts=paste(Ano_ID_anoms, " - ", dat1[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.8), col=dat1[Ano_ID_anoms, "color"])  # Show anomaly types
# This works well

# Run distance-based KNN_AGG from DDoutlier in combination with HMDH SDEN (harmonic mean SDEN)
source(paste0(DatFolder, "RunKNN_AGGwithHMDH.R"))
HDAnomalyTableKNN_AGGHMDH <- KNN_AGGwithHMDH(dat1[,1:4], WeightCorrection="SDEN", k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- HDAnomalyTableKNN_AGGHMDH[HDAnomalyTableKNN_AGGHMDH$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGHMDH$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
text3d(dat1[Ano_ID_anoms, 1:3], texts=paste(Ano_ID_anoms, " - ", dat1[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.8), col=dat1[Ano_ID_anoms, "color"])  # Show anomaly types
# This performs less well in terms of early retrieval


# LOF density based local outlier detection
library(dbscan)
LOFanomalies <- lof(dat1MMb)
summary(LOFanomalies)
rankingLOF <- order(LOFanomalies, decreasing=TRUE) # Order outliers
# Plot 6 anomalies
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- head(rankingLOF)
# These are the values: dat1[Ano_ID_anoms, 1:4]; dat1MM[Ano_ID_anoms,]; dat1MMb[Ano_ID_anoms,]  # These are indeed consistent
points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# Plot more anomalies
Ano_ID_anoms <- head(rankingLOF, n=NumberOfHDAs)
plot3d(dat1[,1:3], col=dat1$color); points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# Plot more anomalies
Ano_ID_anoms <- head(rankingLOF, n=NumberOfHDAs*2)
plot3d(dat1[,1:3], col=dat1$color); points3d(dat1[Ano_ID_anoms, 1:3], color=dat1$color[Ano_ID_anoms], size=12)
# Is similar to regular equiwidth SECODA. Many isolated cases are identified as anomalies.


# Run LOF with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunLOFwithIPP.R"))
HDAnomalyTableLOFIPP <- LOFwithIPP(dat1[,1:4], k=4, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- HDAnomalyTableLOFIPP[HDAnomalyTableLOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableLOFIPP$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
# More
plot3d(dat1[,1:3], col=dat1$color)
Ano_ID_anoms <- HDAnomalyTableLOFIPP[HDAnomalyTableLOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableLOFIPP$HDAscore, decreasing=TRUE),n=2*NumberOfHDAs), n=1), "Ano_ID"]
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
# Many HDAs detected in early retrieval results, but not perfect


# Run SECOHDA (High-density anomalies)
# Run Harmonic Mean Detection of HDAs (HMDH):
source(paste0(DatFolder, "SECOHDA_AlgorithmHarmonicMean.R"))
HDAnomalyTableHM <- SECOHDA(dat1[,1:4], WeightCorrection = "SDEN", TestMode = TRUE)
HDAnomalyTableHMSSE <- SECOHDA(dat1[,1:4], WeightCorrection = "SSE", TestMode = TRUE)
HDAnomalyTableHMNone <- SECOHDA(dat1[,1:4], WeightCorrection = "None", TestMode = TRUE)


# Run IPP
source(paste0(DatFolder, "SECOHDA_AlgorithmIterativePP.R"))
HDAnomalyTableIPP <- SECOHDA(dat1[,1:4], QuantileDenominator = 100, TestMode = TRUE)  # Automatically determined QuantileFilterBoost
HDAnomalyTableIPP2 <- SECOHDA(dat1[,1:4], QuantileDenominator = 1000, TestMode = TRUE)  # Automatically determined QuantileFilterBoost, but more detailed analysis with QuantileDenominator=1000
HDAnomalyTableIPPQF6 <- SECOHDA(dat1[,1:4], QuantileDenominator = 100, QuantileFilterBoost = 6, TestMode = TRUE)  # QuantileFilterBoost = 6, seems to perform well.
# Plot IPP:
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore <= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
text3d(dat1[Ano_ID_anoms, 1:3], texts=paste(Ano_ID_anoms, " - ", dat1[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.8), col=dat1[Ano_ID_anoms, "color"])  # Show anomaly types
# This works well with IPP


# To be able to use general code, we'll copy the given anomalytable to ChosenAnomalyTable
ChosenAnomalyTable <- HDAnomalyTableIPP  # IPP with SECODA
# ChosenAnomalyTable <- AnomalyTableEW; ChosenAnomalyTable$HDAscore = ChosenAnomalyTable$AveAnoScore
# ChosenAnomalyTable <- AnomalyTableED; ChosenAnomalyTable$HDAscore = ChosenAnomalyTable$AveAnoScore
# ChosenAnomalyTable <- HDAnomalyTableHM  # SDEN
# ChosenAnomalyTable <- HDAnomalyTableHMSSE  # SSE
# ChosenAnomalyTable <- HDAnomalyTableHMNone  # No weight correction
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(scoreKNN_AGG)), HDAscore=(max(scoreKNN_AGG)-scoreKNN_AGG))  # KNN-AGG - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(scoreSOF)), HDAscore=(max(scoreSOF)-scoreSOF))  # SOF - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(LOFanomalies)), HDAscore=(max(LOFanomalies)-LOFanomalies))  # LOF - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- HDAnomalyTableRFIPP # IPP with random forest
# ChosenAnomalyTable <- HDAnomalyTableSOFIPP # IPP with SOF
# ChosenAnomalyTable <- HDAnomalyTableLOFIPP # IPP with LOF
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGIPP
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGHMDH # HMDH SDEN with KNN-AGG

# # Test:
# Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
# plot3d(dat1[,1:3], col=dat1$color)
# points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=20, alpha=0.7)


# Plot more and more extreme HDAs
NumberOfHDAs = length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])  # Reset
NumberOfHDAs = NumberOfHDAs + 1  # By selecting and running these four statements you can iteratively show more identified anomalies
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)

# Plot anomalies below certain score
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= 3.2, "Ano_ID"]  # Voor de HDAnomalyTableHM is 530 een goede score-grens (en zie wat er gebeurt als je de score daarna neemt, namelijk 600)
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=15)

# Check the other end of the scores (isolated non-HDAs, which IPP can detect)
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore >= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=FALSE),n=NumberOfHDAs), n=1), "Ano_ID"]
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
# More
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore >= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=FALSE),n=NumberOfHDAs*3), n=1), "Ano_ID"]
plot3d(dat1[,1:3], col=dat1$color)
points3d(dat1[Ano_ID_anoms, 1:3], col=dat1[Ano_ID_anoms, "color"], size=25, alpha=0.5)
# These indeed are the isolated cases!
# Works not only for IPP with SECODA, but also for IPP with KNN-AGG and QSP/SOF and other density and distance based methods. (In theory it also works for LOF, but note that this algorithm tends to identify cases relatively close to local clusters. Random forest also gives mixed results, due to that underlying algorithm.)




#### Full evaluation 


# A - Met ROC en AUC

dat1$anomalybinary <- 0 # Add binary test value
dat1$anomalybinary[dat1$HighDensityAno1==TRUE] <- 1  # Give HDAs a value of 1
table(dat1$anomalybinary)

# Load library
library(pROC)

# ROC equiwidth SECODA
ROC1 <- roc(dat1$anomalybinary, AnomalyTableEW$AveAnoScore, percent = TRUE) # Calculate ROC and AUC
ROC1 # Show ROC AUC (area under the curve)
plot(ROC1, col="red") # Plot ROC curve
auc(ROC1, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC equidepth SECODA
ROC2 <- roc(dat1$anomalybinary, AnomalyTableED$AveAnoScore, percent = TRUE) # Calculate ROC and AUC
ROC2 # Show ROC AUC (area under the curve)
plot(ROC2, col="blue", add=TRUE) # Plot ROC curve
auc(ROC2, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA SDEN
ROC3 <- roc(dat1$anomalybinary, HDAnomalyTableHM$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC3 # Show ROC AUC (area under the curve)
plot(ROC3, col="green", add=TRUE) # Plot ROC curve
auc(ROC3, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC IPP SECOHDA
ROC4 <- roc(dat1$anomalybinary, HDAnomalyTableIPP$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC4 # Show ROC AUC (area under the curve)
plot(ROC4, col="purple", add=TRUE) # Plot ROC curve
auc(ROC4, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA SSE
ROC5 <- roc(dat1$anomalybinary, HDAnomalyTableHMSSE$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC5 # Show ROC AUC (area under the curve)
plot(ROC5, col="seagreen4", add=TRUE) # Plot ROC curve
auc(ROC5, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA without weight correction
ROC6 <- roc(dat1$anomalybinary, HDAnomalyTableHMNone$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC6 # Show ROC AUC (area under the curve)
plot(ROC6, col="yellowgreen", add=TRUE) # Plot ROC curve
auc(ROC6, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC IPP SECOHDA with Quantilefilterboost set at 6
ROC7 <- roc(dat1$anomalybinary, HDAnomalyTableIPPQF6$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC7 # Show ROC AUC (area under the curve)
plot(ROC7, col="turquoise4", add=TRUE) # Plot ROC curve
auc(ROC7, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC IPP SECOHDA QuantileDenominator=1000
ROC11 <- roc(dat1$anomalybinary, HDAnomalyTableIPP2$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC11 # Show ROC AUC (area under the curve)
plot(ROC11, col="brown", add=TRUE) # Plot ROC curve
auc(ROC11, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC SOF/QSP / spoutlier
ROC12 <- roc(dat1$anomalybinary, (max(scoreSOF)-scoreSOF), percent = TRUE) # Calculate ROC and AUC
ROC12
plot(ROC12, col="violetred4", add=TRUE) # Plot ROC curve
auc(ROC12, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC LOF / DBSCAN
ROC13 <- roc(dat1$anomalybinary, (max(LOFanomalies)-LOFanomalies), percent = TRUE) # Calculate ROC and AUC
ROC13
plot(ROC13, col="violetred3", add=TRUE) # Plot ROC curve
auc(ROC13, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC LOF with IPP
ROC15 <- roc(dat1$anomalybinary, HDAnomalyTableLOFIPP$HDAscore, percent = TRUE)
ROC15
plot(ROC15, col="black", add=TRUE) # Plot ROC curve
auc(ROC15, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC SOF/QSP with IPP
ROC16 <- roc(dat1$anomalybinary, HDAnomalyTableSOFIPP$HDAscore, percent = TRUE)
ROC16
plot(ROC16, col="tomato3", add=TRUE) # Plot ROC curve
auc(ROC16, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG (DDoutlier package)
ROC17 <- roc(dat1$anomalybinary, (max(scoreKNN_AGG)-scoreKNN_AGG), percent = TRUE) # Use reverse scores because all with other algorithms lower scores are more anomalous
ROC17
plot(ROC17, col="tomato3", add=TRUE) # Plot ROC curve
auc(ROC17, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with IPP
ROC18 <- roc(dat1$anomalybinary, HDAnomalyTableKNN_AGGIPP$HDAscore, percent = TRUE)
ROC18
plot(ROC18, col="navyblue", add=TRUE) # Plot ROC curve
auc(ROC18, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with HMDH SDEN
ROC19 <- roc(dat1$anomalybinary, HDAnomalyTableKNN_AGGHMDH$HDAscore, percent = TRUE)
ROC19
plot(ROC19, col="turquoise1", add=TRUE) # Plot ROC curve
auc(ROC19, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension



# B - Early retrieval & confusion matrix 

# Since we do not analyze much cases here (and the false positives therefore, per definition, cannot be extremely high), it is mainly the sensitivity/recall that is relevant here. This represents the percentage of true anomalies amongst the few most extreme anomalies returned by the algorithm.  
# Met harde threshold checken of de anomalieën bij de meest laagste anomaliescores zitten
dat1$ID <- 1:nrow(dat1)  # Include explicit ID-field

# Met threshold = aantal HDA's
NumberOfHDAs = length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])  # Reset to original number
cat(paste0("Threshold set to retrieve ", NumberOfHDAs, " anomalies."))
Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1) # Set threshold
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= Threshold1, "Ano_ID"] # Determine anomalies
IdentifiedCases <- Ano_ID_anoms %in% dat1[dat1$HighDensityAno1==TRUE, "ID"] # As boolean
sum(IdentifiedCases, na.rm=TRUE) # Number of true anomalies that are identified correctly as extreme / early retrieval anomalies
table(IdentifiedCases)
cat(paste0("True in the table above refers to the number of correctly identified HDA anomalies."))
# Sensitivity/recall: TP/(TP+FN) = the proportion of actual positives that are correctly identified as such (e.g., the percentage of sick people who are correctly identified as having the condition)
cat(paste0("Percentage of correctly identified anomalies (sensitivity/recall): ", sum(IdentifiedCases, na.rm=TRUE)/NumberOfHDAs, "."))
# Negatively denoted by algorithm: nrow(dat1)-length(Ano_ID_anoms)  # False negatives is then: (nrow(dat1)-length(Ano_ID_anoms)) - ( (nrow(dat1)-length(Ano_ID_anoms)) - (length(Ano_ID_anoms)-sum(IdentifiedCases, na.rm=TRUE)) )
# Precision/PPV: = TP/(TP+FP):
cat(paste0("Percentage of denoted anomalies that are indeed anomalies (precision/PPV): ", sum(IdentifiedCases, na.rm=TRUE) / length(Ano_ID_anoms), "."))
cat(paste0("Percentage of denoted anomalies that are indeed anomalies (precision/PPV): ", sum(IdentifiedCases, na.rm=TRUE) / ( (sum(IdentifiedCases, na.rm=TRUE) + (nrow(dat1)-length(Ano_ID_anoms)) - ( (nrow(dat1)-length(Ano_ID_anoms)) - (length(Ano_ID_anoms)-sum(IdentifiedCases, na.rm=TRUE)) )  ) ), " (alternative calculation)."))
# Accuracy: (TP+TN)/(TP+FP+FN+TN) = the ratio of the correctly labeled cases to all cases:
cat(paste0("Percentage of correctly classified cases (accuracy): ", (sum(IdentifiedCases, na.rm=TRUE) + ( (nrow(dat1)-length(Ano_ID_anoms)) - (length(dat1[dat1$HighDensityAno1==TRUE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE)) ) ) / nrow(dat1), "."))  # Denoted negatives: (nrow(dat1)-length(Ano_ID_anoms))  # From this you should subtract the false negatives, which are: (length(dat1[dat1$HighDensityAno1==TRUE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE))
# Specificity: = TN/(TN+FP):
IdentifiedCases <- Ano_ID_anoms %in% dat1[dat1$HighDensityAno1==FALSE, "ID"] # As boolean
sum(IdentifiedCases, na.rm=TRUE) # Number of non-HDA cases that are identified incorrectly as HDA cases
cat(paste0("Percentage of correctly identified normal cases (specificity): ", (length(dat1[dat1$HighDensityAno1==FALSE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE)) / length(dat1[dat1$HighDensityAno1==FALSE, "ID"]), "."))



# Confusion matrix based evaluation measures
# We already have a ChosenAnomalyTable. Now also choose the ROC object that corresponds with this
ChosenROCobject <- ROC4  # ROC4 = IPP
# ChosenROCobject <- ROC1  # ROC1 = SECODA
# ChosenROCobject <- ROC2  # ROC2 = SECODA ED
# ChosenROCobject <- ROC3  # ROC3 = Harmonic Mean SDEN SECOHDA
# ChosenROCobject <- ROC5  # ROC5 = Harmonic Mean SSE SECOHDA
# ChosenROCobject <- ROC6  # ROC6 = Harmonic Mean no weight correction SECOHDA
# ChosenROCobject <- ROC12  # ROC12 = SOF
# ChosenROCobject <- ROC13  # ROC13 = LOF
# ChosenROCobject <- ROC15  # ROC15 = IPP with LOF
# ChosenROCobject <- ROC16  # ROC16 = IPP with SOF
# ChosenROCobject <- ROC17  # ROC17 = KNN-AGG
# ChosenROCobject <- ROC18  # ROC18 = IPP with KNN-AGG
# ChosenROCobject <- ROC19  # ROC19 = Harmonic Mean SDEN with KNN-AGG (HMDH)


# Now include all cases with the threshold score (which especially with HDAnomalyTableHM leads to many more cases included in Ano_ID_anoms)
HDpred <- rep(0, nrow(ChosenAnomalyTable)) # Create vector with zeros
Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])), n=1)  # Determine the threshold
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= Threshold1, "Ano_ID"]
HDpred[ChosenAnomalyTable$Ano_ID %in% Ano_ID_anoms] <- 1
# Alternative would be: HDpred <- rep(0, nrow(ChosenAnomalyTable)); HDpred[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])), n=1)] <- 1
table(HDpred)
table(HDpred, dat1$anomalybinary) # Confusion matrix
# Get confusion matrix elements from pROC:
coords(ChosenROCobject, Threshold1, ret=c("threshold", "tn", "tp", "fn", "fp"), transpose=TRUE)
# Accuracy from confusion matrix: (TP+TN)/(TP+FP+FN+TN):
( coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("tn"), transpose=TRUE) ) / ( coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("tn"), transpose=TRUE)  + coords(ChosenROCobject, Threshold1, ret=c("fp"), transpose=TRUE)  + coords(ChosenROCobject, Threshold1, ret=c("fn"), transpose=TRUE) )
# Accuracy directly from vectors (Accuracy: Percentage of correctly classified cases)
mean(HDpred == dat1$anomalybinary)
# Accuracy from pROC:
coords(ChosenROCobject, Threshold1, ret=c("accuracy"), transpose=TRUE) 

# We can use the threshold and ROC also to calculate the other evaluation scores with the pROC package
coords(ChosenROCobject, Threshold1, ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "npv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv", "tn", "tp", "fn", "fp"), transpose=TRUE) # Print all metrics

# Using best Youden threshold
coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "npv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv", "tn", "tp", "fn", "fp"), transpose=TRUE) # Print all metrics for the best Youden threshold. 


# F1 measure
# The F1 Score is the 2*((precision*recall)/(precision+recall)). It is also called the F Score or the F Measure. Put another way, the F1 score conveys the balance between the precision and the recall.
# Using confusion matrix: F1 = 2TP / (2TP + FP + FN)    - Zie ook: http://onlineconfusionmatrix.com/
cat(paste0("F1 measure: ", 2*coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) / (2*coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("fp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("fn"), transpose=TRUE)), " (directly via confusion matrix)."));
cat(paste0("F1 measure: ", 2 * ((coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) * coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE))/(coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE)+coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE))), "."))
cat(paste0("F1 measure: ", 2 /  (1/coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE)), " (alternative calculation)."));
# Using best Youden ROC threshold:
cat(paste0("F1 measure: ", 2*coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("tp"), transpose=TRUE) / (2*coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("fp"), transpose=TRUE) + coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("fn"), transpose=TRUE)), " (directly via confusion matrix)."));
cat(paste0("F1 measure: ", 2 / (1/(coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("ppv"), transpose=TRUE)) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("sensitivity"), transpose=TRUE)), " (alternative calculation)."));  


# Calculate GMPR/GMRP measure, based on precision (ppv) and sensitivity (recall)
# Zie "Polis set en ROC analysis.txt"
sqrt( (coords(ChosenROCobject, Threshold1, ret="ppv", transpose=TRUE)) * (coords(ChosenROCobject, Threshold1, ret="sensitivity", transpose=TRUE)) )
sqrt( (coords(ChosenROCobject, Threshold1, ret="ppv", transpose=TRUE)/100) * (coords(ChosenROCobject, Threshold1, ret="sensitivity", transpose=TRUE)/100) )
# Using best Youden threshold:
sqrt( (coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="ppv", transpose=TRUE)) * (coords(ChosenROCobject, "best", print.thres.best.method="youden", ret="sensitivity", transpose=TRUE)) )


# HMFM
# Harmonic mean of four measures/metrics: Sensitivity/Recall, Specificity, Precision/PPV, Accuracy
cat(paste0("HMFM measure: ", (4 / (1/coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("specificity"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("accuracy"), transpose=TRUE))) / 100, "."));
# Using best Youden threshold:
cat(paste0("HMFM measure: ", (4 / (1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("sensitivity"), transpose=TRUE) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("specificity"), transpose=TRUE) + 1/(coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("ppv"), transpose=TRUE)) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("accuracy"), transpose=TRUE))) / 100, "."))
# ! Let op: multiply by 100 problem?? Lijkt er niet op, want ppv/precision is bij IPP laag (1.191296), maar dat hoort zo. Handmatig: tp / (tp + fp):  coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("tp"), transpose=TRUE) / (coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("fp"), transpose=TRUE) ) * 100 # Als percentage, want doet pROC ook.


# Calculate Cohen's kappa:
library(fmsb) # ! Note: package fmsb masks roc-object of pROC
Kappa.test(HDpred, dat1$anomalybinary)   
# Alternative:
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
source(paste0(DatFolder, "Matthews.Correlation.Coefficient.R"))  # Load algorithm. Zie "Matthews correlation coefficient - mcc - mltools.mht" of https://rdrr.io/cran/mltools/man/mcc.html
# The function should also be available in the package mltools
mcc(TP=TP, FP=FP, TN=TN, FN=FN)  # Using explict true positives etc
mcc(preds=HDpred, actuals=dat1$anomalybinary)  # Alternative using two binary vectors
# Using best Youden threshold:
mcc(TP=TP2, FP=FP2, TN=TN2, FN=FN2)  # Using explict true positives etc
# The newer version also allows confusion matrices (and uses data.table):
source(paste0(DatFolder, "Matthews.Correlation.Coefficient2.R"))
mcc(TP=TP, FP=FP, TN=TN, FN=FN)  # Using explict true positives etc
mcc(preds=HDpred, actuals=dat1$anomalybinary)  # Alternative using two binary vectors. Uses data.table package.
mcc(confusionM=matrix1) # Uses data.table package.
# Using best Youden threshold:
mcc(TP=TP2, FP=FP2, TN=TN2, FN=FN2)  # Using explict true positives etc
mcc(confusionM=matrix2) # Uses data.table package.


# PRC
library(precrec)
# https://cran.r-project.org/web/packages/precrec/vignettes/introduction.html

table(dat1$anomalybinary)
dat1$anomalybinary[dat1$anomalybinary == 1] <- -1  # Recode anomalies to -1 for precrec package
table(dat1$anomalybinary)
sscurves <- evalmod(scores = ChosenAnomalyTable$HDAscore, labels = dat1$anomalybinary)
# For SOF/spoutlier: sscurves <- evalmod(scores = (max(scoreSOF)-scoreSOF), labels = dat1$anomalybinary)
# For LOF: sscurves <- evalmod(scores = (max(LOFanomalies)-LOFanomalies), labels = dat1$anomalybinary)
sscurves  # Print info
plot(sscurves) # ROC en PRC
plot(sscurves, "PRC") # Only precision-recall (PRC)
# Plot met autoplot / ggplot2. The autoplot function outputs ROC and Precision-Recall curves by using the ggplot2 package.
library(ggplot2)
autoplot(sscurves)
autoplot(sscurves, "PRC")

# The auc function outputs a data frame with the AUC (Area Under the Curve) scores.
aucs <- precrec::auc(sscurves)  # Het "precrec::" is nodig om er zeker van te zijn dat de auc() van het precrec package wordt gebruikt en niet die van pROC (die een foutmelding geeft)
# View(aucs) # View as table
knitr::kable(aucs) # View with knitr
subset(aucs, curvetypes == "PRC") # Only the PRC. Or: aucs[aucs$curvetypes=="PRC","aucs"] # Only the PRC. ROC: aucs[aucs$curvetypes=="ROC","aucs"]

