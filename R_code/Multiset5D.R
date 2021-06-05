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
NieuweSet <- read.arff(paste0(DatFolder, "HDA_Dataset_Multiset5D.arff"))


# Count anomalies
cat(paste0("There are ", nrow(NieuweSet[NieuweSet$AnomalyType1 != 0, ]), " true anomalies."))  
NoOfHDAs <- nrow(NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$HighDensityAno1 == TRUE, ])
cat(paste0("There are ", NoOfHDAs, " true high-density anomalies."))


# Plot 4D with colar as categorical variabele
plot3d(NieuweSet[,1:3], col=NieuweSet$Color, size=1.5)  # Plot (excluding the Shape variable)
# Show info about manually inserted anomalies
points3d(NieuweSet[NieuweSet$AnomalyType1 != 0, 1:3], col=NieuweSet[NieuweSet$AnomalyType1 != 0, "Color"], size=9, alpha=0.7) # Plot all manually inserted anomalies
# points3d(NieuweSet[NieuweSet$HighDensityAno1 == TRUE, 1:3], col=NieuweSet[NieuweSet$HighDensityAno1 == TRUE, "Color"], size=9, alpha=0.7) # Plot all manually inserted HDA anomalies 
text3d(NieuweSet[NieuweSet$AnomalyType1 != 0, 1:3], texts=which(NieuweSet[, "AnomalyType1"] != 0), cex=0.8, adj=1.3, col=NieuweSet[NieuweSet$AnomalyType1 != 0, "Color"])  # Add row numbers
text3d(NieuweSet[NieuweSet$AnomalyType1 != 0, 1:3], texts=paste(NieuweSet[NieuweSet$AnomalyType1 != 0, "AnomalyType1"], " - ", NieuweSet[NieuweSet$AnomalyType1 != 0, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[NieuweSet$AnomalyType1 != 0, "Color"])

# Plot 5D:
# All cases small size (including anomalies):
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)
# Alternatively, plot regular cases normal size, and anomalies large:
plot3d(NieuweSet[NieuweSet$Shape=="p" & NieuweSet$AnomalyType1 == 0, 1:3], col=NieuweSet[NieuweSet$Shape=="p" & NieuweSet$AnomalyType1 == 0, 4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p" & NieuweSet$AnomalyType1 == 0, 1:3], col=NieuweSet[NieuweSet$Shape!="p" & NieuweSet$AnomalyType1 == 0, 4], texts=NieuweSet[NieuweSet$Shape!="p" & NieuweSet$AnomalyType1 == 0, 5], cex=0.4)
points3d(NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$Shape == "p", 1:3], col=NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$Shape =="p", "Color"], size=10, alpha=0.7); text3d(NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$Shape != "p", 1:3], col=NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$Shape != "p", 4], texts=NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$Shape != "p",5], cex=1.4)
# And if you want the anomaly info:
text3d(NieuweSet[NieuweSet$AnomalyType1 != 0, 1:3], texts=which(NieuweSet[, "AnomalyType1"] != 0), cex=0.8, adj=1.3, col=NieuweSet[NieuweSet$AnomalyType1 != 0, "Color"])  # Add row numbers
text3d(NieuweSet[NieuweSet$AnomalyType1 != 0, 1:3], texts=paste(NieuweSet[NieuweSet$AnomalyType1 != 0, "AnomalyType1"], " - ", NieuweSet[NieuweSet$AnomalyType1 != 0, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[NieuweSet$AnomalyType1 != 0, "Color"])
# Show only high-density anomalies:
plot3d(NieuweSet[NieuweSet$Shape=="p" & NieuweSet$HighDensityAno1 == FALSE, 1:3], col=NieuweSet[NieuweSet$Shape=="p" & NieuweSet$HighDensityAno1 == FALSE, 4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p" & NieuweSet$HighDensityAno1 == FALSE, 1:3], col=NieuweSet[NieuweSet$Shape!="p" & NieuweSet$HighDensityAno1 == FALSE, 4], texts=NieuweSet[NieuweSet$Shape!="p" & NieuweSet$HighDensityAno1 == FALSE, 5], cex=0.4)
points3d(NieuweSet[NieuweSet$Shape == "p" & NieuweSet$HighDensityAno1 == TRUE, 1:3], col=NieuweSet[NieuweSet$Shape =="p" & NieuweSet$HighDensityAno1 == TRUE, "Color"], size=10, alpha=0.7); text3d(NieuweSet[NieuweSet$Shape != "p" &  NieuweSet$HighDensityAno1 == TRUE, 1:3], col=NieuweSet[NieuweSet$Shape != "p" & NieuweSet$HighDensityAno1 == TRUE, 4], texts=NieuweSet[NieuweSet$Shape != "p" & NieuweSet$HighDensityAno1 == TRUE,5], cex=1.4)
text3d(NieuweSet[NieuweSet$HighDensityAno1 == TRUE, 1:3], texts=which(NieuweSet$HighDensityAno1 == TRUE), cex=0.8, adj=1.3, col=NieuweSet[NieuweSet$HighDensityAno1 == TRUE, "Color"])  # Add row numbers
text3d(NieuweSet[NieuweSet$HighDensityAno1 == TRUE, 1:3], texts=paste(NieuweSet[NieuweSet$HighDensityAno1 == TRUE, "AnomalyType1"], " - ", NieuweSet[NieuweSet$HighDensityAno1 == TRUE, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[NieuweSet$HighDensityAno1 == TRUE, "Color"])


# Check amount of anomalies
table(NieuweSet$Color)
table(NieuweSet$Shape)
library(plyr)
ddply(NieuweSet, c('AnomalyType1', 'HighDensityAno1'), function(x) count=nrow(x))
sum(ddply(NieuweSet, c('AnomalyType1', 'HighDensityAno1'), function(x) count=nrow(x))$V1) # This is indeed the total amount of cases
# Check amount of cases per Color-Shape combination
ddply(NieuweSet, c('Color', 'Shape'), function(x) count=nrow(x))
sum(ddply(NieuweSet, c('Color', 'Shape'), function(x) count=nrow(x))$V1) # This is indeed the total amount of cases


# Regular anomaly detection with SECODA
AnomalyTableEW <- SECODA(NieuweSet[,1:5], StartHeuristicsAfterIteration=9999)  # In order to obtain the most precize results with the underlying SECODA algorithm we discard the pruning.
# If we increase the number of iterations, the precision of the results is improved (because more iterations mean more detailed discretization runs, which brings the results closer to precize numerical distance-based calculations):
# AnomalyTableEW <- SECODA(NieuweSet[,1:5], StartHeuristicsAfterIteration=9999, MinimumNumberOfIterations = 80)
Ano_ID_anoms <- AnomalyTableEW[AnomalyTableEW$AveAnoScore <= head(tail(sort(AnomalyTableEW$AveAnoScore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
plot3d(NieuweSet[,1:3], col=NieuweSet$Color, size=1.5) # Plot 4D
points3d(NieuweSet[Ano_ID_anoms,1:3], col=NieuweSet$Color[Ano_ID_anoms], size=9, alpha=0.7)
# All types of anomalies are detected, many of which are numerically isolated
# Show with scores: 
text3d(NieuweSet[Ano_ID_anoms,1:3], texts=AnomalyTableEW[Ano_ID_anoms, "AveAnoScore"], cex=0.8, adj=1.4, col=NieuweSet[Ano_ID_anoms, "Color"])

# Plot in 5D. Some of them are not easily recognizable in 4D, but will be if you plot them 5D:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
# Note there is a bit of redundancy in the plotting (anomalies shown both small and large), but that is not a real issue in this context.

# Check if they're real (high-density) anomalies
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])
# They are anomalies, but not necessarily high-density ano's. ('True' is high-density, 'False' is low-density or no anomaly at all.)


# Run algorithm in equidepth mode, which should theoretically favor high-density anomalies at the cost of extreme value anomalies
AnomalyTableED <- SECODA(NieuweSet[,1:5], BinningMethod = "ED", StartHeuristicsAfterIteration=9999)
# Plot
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- AnomalyTableED[AnomalyTableED$AveAnoScore <= head(tail(sort(AnomalyTableED$AveAnoScore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])


# Run distance-based (spoutlier package, random sampling based)
# First create dummy variables via a model matrix
NieuweSetMM <- model.matrix(~X1+X2+X3+Color+Shape, NieuweSet)[,2:17]
summary(NieuweSetMM)
normalizeNum <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }  # do_knno and LOF doesn't seem to normalize, so: # Normalization function to rescale numerical attributes to the range 0 and 1: (X - min(X))/(max(X) - min(X))
NieuweSetMMb <- as.matrix(as.data.frame(lapply(as.data.frame(NieuweSetMM), normalizeNum)))
plot3d(NieuweSetMMb[,1:3], col=NieuweSet$Color, size=1.5) # Verify. Indeed all between 0 and 1. 
# Run analysis:
library(spoutlier)
set.seed(2)
scoreSOF <- qsp(NieuweSetMM, sample.size=3000, normalize = TRUE) # Determine outlier scores. A higher number of the sample size (standard is 20) gives a more stable result. For reproducibility purposes, a seed can also be set.
# scoreSOF <- qsp(NieuweSetMM, sample.size=5000, normalize = TRUE) # Determine outlier scores. A higher number of the sample size (standard is 20) gives a more stable result. For reproducibility purposes, a seed can also be set.
rankingSOF <- order(scoreSOF, decreasing=TRUE) # Order outliers
# Plot 6 anomalies
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingSOF)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
# Check if they're real (high-density) anomalies
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])
# They are anomalies, but not necessarily high-density ano's. ('True' is high-density, 'False' is low-density or no anomaly at all.)
# Plot more:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingSOF, n=NoOfHDAs)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])


# Run distance-based SOF in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunSOFwithIPP.R"))
set.seed(3)
HDAnomalyTableSOFIPP <- SOFwithIPP(NieuweSet[,1:5], QuantileFilterBoost = -9999, SampleSize=3000, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- HDAnomalyTableSOFIPP[HDAnomalyTableSOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableSOFIPP$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])


# KNN_AGG distance-based detection (DDoutlier package)
library(DDoutlier)
scoreKNN_AGG <- KNN_AGG(dataset=NieuweSetMMb, k_min=10, k_max=15)
rankingKNN_AGG <- order(scoreKNN_AGG, decreasing=TRUE) # Order outliers
# Plot top anomalies
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingKNN_AGG, n=NoOfHDAs)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
# Check if they're real (high-density) anomalies
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])


# Run distance-based KNN_AGG from DDoutlier in combination with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunKNN_AGGwithIPP.R"))
HDAnomalyTableKNN_AGGIPP <- KNN_AGGwithIPP(NieuweSet[,1:5], QuantileFilterBoost = -9999, k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- HDAnomalyTableKNN_AGGIPP[HDAnomalyTableKNN_AGGIPP$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGIPP$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])


# Run distance-based KNN_AGG from DDoutlier in combination with HMDH SDEN
source(paste0(DatFolder, "RunKNN_AGGwithHMDH.R"))
HDAnomalyTableKNN_AGGHMSDEN <- KNN_AGGwithHMDH(NieuweSet[,1:5], WeightCorrection="SDEN", k_min=10, k_max=15, Normalize=TRUE, TestMode = TRUE)
Ano_ID_anoms <- HDAnomalyTableKNN_AGGHMSDEN[HDAnomalyTableKNN_AGGHMSDEN$HDAscore <= head(tail(sort(HDAnomalyTableKNN_AGGHMSDEN$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]


# LOF density based local outlier detection
library(dbscan)
LOFanomalies <- lof(NieuweSetMMb)
rankingLOF <- order(LOFanomalies, decreasing=TRUE) # Order outliers
# Check results
max(LOFanomalies)  # Most extreme anomaly. Could be Inf if there are duplicates in the set.
which(LOFanomalies==max(LOFanomalies)) # These cases have the maximum (or Inf) score.
# Plot 6 anomalies:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingLOF)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
# These are indeed (non-HDA) outliers
# Plot more anomalies
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingLOF, n=NoOfHDAs)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
# They are anomalies, but quite some false positives (i.e. non-HDAs)
# Plot more anomalies
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingLOF, n=NoOfHDAs*2)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
# Some strange results amongst the blue cases. Many of them are intuitively not outliers, and quite some isolated cases are not seen as outliers. This is likely due to LOF's relative density measure.
# Plot more anomalies
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- head(rankingLOF, n=NoOfHDAs*4)
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=12, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=2); rm(Ano_ID_anomsDF)
# More strange results: Many isolated anomalies have not been discovered, despite them clearly being Type I or IV. This may hamper the detection of HDAs when LOF is combined with IPP (especially the completely numerical run may not be able to identify the noise correctly).


# Run LOF with IPP - The QuantileFilterBoost will be determined by SECODA, because there is no clear-cut equivalent (and if there was it may be difficult to compare results).
source(paste0(DatFolder, "RunLOFwithIPP.R"))
HDAnomalyTableLOFIPP <- LOFwithIPP(NieuweSet[,1:5], k=4, Normalize=TRUE, TestMode = TRUE)
# Plot:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- HDAnomalyTableLOFIPP[HDAnomalyTableLOFIPP$HDAscore <= head(tail(sort(HDAnomalyTableLOFIPP$HDAscore, decreasing=TRUE),n=NoOfHDAs*2), n=1), "Ano_ID"]
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])
# This looks reasonable However, several anomalies are not discovered and it takes a high threshold (and many false positives) to identify them.
# The anomalies missed are essentially the same as with regular LOF.


# Run SECOHDA (High-density anomalies): SECODA with harmonic mean method
source(paste0(DatFolder, "SECOHDA_AlgorithmHarmonicMean.R"))
HDAnomalyTableHM <- SECOHDA(NieuweSet[,1:5], WeightCorrection = "SDEN", StartSECODAHeuristicsAfterIteration=9999, TestMode = TRUE)
HDAnomalyTableHMSSE <- SECOHDA(NieuweSet[,1:5], WeightCorrection = "SSE", StartSECODAHeuristicsAfterIteration=9999, TestMode = TRUE)
HDAnomalyTableHMNone <- SECOHDA(NieuweSet[,1:5], WeightCorrection = "None", StartSECODAHeuristicsAfterIteration=9999, TestMode = TRUE)
# Plot SDEN HM:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- HDAnomalyTableHM[HDAnomalyTableHM$HDAscore <= head(tail(sort(HDAnomalyTableHM$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])
# This doesn't give the best results


# HDA Anomaly detection using SECODA with IPP
source(paste0(DatFolder, "SECOHDA_AlgorithmIterativePP.R"))
HDAnomalyTableIPP <- SECOHDA(NieuweSet[,1:5], QuantileDenominator = 100, QuantileFilterBoost = -9999, MinimumNumberOfSECODAIterations=5, StartSECODAHeuristicsAfterIteration=9999, TestMode = TRUE) # In order to obtain the most precize results with the underlying SECODA algorithm we discard the pruning.
# Plot IPP in 5D:
plot3d(NieuweSet[NieuweSet$Shape=="p",1:3], col=NieuweSet[NieuweSet$Shape=="p",4], size=1.5); text3d(NieuweSet[NieuweSet$Shape!="p",1:3], col=NieuweSet[NieuweSet$Shape!="p",4], texts=NieuweSet[NieuweSet$Shape!="p",5], cex=0.4)  # Plot all
Ano_ID_anoms <- HDAnomalyTableIPP[HDAnomalyTableIPP$HDAscore <= head(tail(sort(HDAnomalyTableIPP$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1), "Ano_ID"]
Ano_ID_anomsDF <- NieuweSet[Ano_ID_anoms,]
points3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape == "p", "Color"], size=10, alpha=0.7); text3d(Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", 1:3], col=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Color"], texts=Ano_ID_anomsDF[Ano_ID_anomsDF$Shape != "p", "Shape"], cex=1.4); rm(Ano_ID_anomsDF)
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=Ano_ID_anoms, cex=0.8, adj=1.2, col=NieuweSet[Ano_ID_anoms, "Color"])
text3d(NieuweSet[Ano_ID_anoms, 1:3], texts=paste(NieuweSet[Ano_ID_anoms, "AnomalyType1"], " - ", NieuweSet[Ano_ID_anoms, "HighDensityAno1"]), cex=0.7, adj=c(0.6, 1.5), col=NieuweSet[Ano_ID_anoms, "Color"])
# This gives good results



# To be able to use general code, we'll copy the given anomalytable to ChosenAnomalyTable
ChosenAnomalyTable <- HDAnomalyTableIPP
# ChosenAnomalyTable <- AnomalyTableEW; ChosenAnomalyTable$HDAscore = ChosenAnomalyTable$AveAnoScore # If you want to analyze regular SECODA, you need to change the name of the variable containing the scores
# ChosenAnomalyTable <- AnomalyTableED; ChosenAnomalyTable$HDAscore = ChosenAnomalyTable$AveAnoScore # If you want to analyze regular SECODA, you need to change the name of the variable containing the scores
# ChosenAnomalyTable <- HDAnomalyTableHM  # SDEN
# ChosenAnomalyTable <- HDAnomalyTableHMSSE  # SSE
# ChosenAnomalyTable <- HDAnomalyTableHMNone  # None / no weights
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(scoreKNN_AGG)), HDAscore=(max(scoreKNN_AGG)-scoreKNN_AGG))  # KNN-AGG - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(scoreSOF)), HDAscore=(max(scoreSOF)-scoreSOF))  # SOF - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- data.frame(Ano_ID=seq(1:length(LOFanomalies)), HDAscore=(max(LOFanomalies)-LOFanomalies))  # LOF - Draai scores om zodat lagere scores ook meer anomalous zijn.
# ChosenAnomalyTable <- HDAnomalyTableSOFIPP # IPP with SOF
# ChosenAnomalyTable <- HDAnomalyTableLOFIPP # IPP with LOF
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGIPP
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGHMSKEW # KNN-AGG with HMDH 'SKEW'
# ChosenAnomalyTable <- HDAnomalyTableKNN_AGGHMSDEN # KNN-AGG with HMDH 'SDEN'





#### Full evaluation 


# A - With ROC and AUC

NieuweSet$anomalybinary <- 0 # Add binary test value
NieuweSet$anomalybinary[NieuweSet$HighDensityAno1==TRUE] <- 1  # Give HDAs a value of 1
table(NieuweSet$anomalybinary)

# Load library
library(pROC)

# ROC equiwidth SECODA
ROC1 <- roc(NieuweSet$anomalybinary, AnomalyTableEW$AveAnoScore, percent = TRUE) # Calculate ROC and AUC
ROC1 # Show ROC AUC (area under the curve)
plot(ROC1, col="red") # Plot ROC curve
auc(ROC1, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC equidepth SECODA
ROC2 <- roc(NieuweSet$anomalybinary, AnomalyTableED$AveAnoScore, percent = TRUE) # Calculate ROC and AUC
ROC2 # Show ROC AUC (area under the curve)
plot(ROC2, col="blue", add=TRUE) # Plot ROC curve
auc(ROC2, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SDEN SECOHDA
ROC3 <- roc(NieuweSet$anomalybinary, HDAnomalyTableHM$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC3 # Show ROC AUC (area under the curve)
plot(ROC3, col="green", add=TRUE) # Plot ROC curve
auc(ROC3, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC IPP SECOHDA
ROC4 <- roc(NieuweSet$anomalybinary, HDAnomalyTableIPP$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC4 # Show ROC AUC (area under the curve)
plot(ROC4, col="purple", add=TRUE) # Plot ROC curve
auc(ROC4, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA SSE
ROC5 <- roc(NieuweSet$anomalybinary, HDAnomalyTableHMSSE$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC5 # Show ROC AUC (area under the curve)
plot(ROC5, col="seagreen4", add=TRUE) # Plot ROC curve
auc(ROC5, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC Harmonic Mean SECOHDA without weight correction
ROC6 <- roc(NieuweSet$anomalybinary, HDAnomalyTableHMNone$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC6 # Show ROC AUC (area under the curve)
plot(ROC6, col="olivedrab", add=TRUE) # Plot ROC curve
auc(ROC6, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC SOF/QSP / spoutlier
ROC12 <- roc(NieuweSet$anomalybinary, (max(scoreSOF)-scoreSOF), percent = TRUE) # Calculate ROC and AUC. Reverse scores, otherwise the AUC is calculated correctly, but the confusion matrix based metrics below will be inconsistent
ROC12
plot(ROC12, col="pink3", add=TRUE) # Plot ROC curve
auc(ROC12, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC LOF / dbscan
ROC13 <- roc(NieuweSet$anomalybinary, (max(LOFanomalies)-LOFanomalies), percent = TRUE) # Calculate ROC and AUC
ROC13
plot(ROC13, col="violetred3", add=TRUE) # Plot ROC curve
auc(ROC13, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC LOF with IPP
ROC15 <- roc(NieuweSet$anomalybinary, HDAnomalyTableLOFIPP$HDAscore, percent = TRUE)
ROC15
plot(ROC15, col="black", add=TRUE) # Plot ROC curve
auc(ROC15, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC SOF/QSP with IPP
ROC16 <- roc(NieuweSet$anomalybinary, HDAnomalyTableSOFIPP$HDAscore, percent = TRUE)
ROC16
plot(ROC16, col="tomato3", add=TRUE) # Plot ROC curve
auc(ROC16, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG (DDoutlier package)
ROC17 <- roc(NieuweSet$anomalybinary, (max(scoreKNN_AGG)-scoreKNN_AGG), percent = TRUE)  # Use reverse scores because all with other algorithms lower scores are more anomalous
ROC17
plot(ROC17, col="tomato3", add=TRUE) # Plot ROC curve
auc(ROC17, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with IPP
ROC18 <- roc(NieuweSet$anomalybinary, HDAnomalyTableKNN_AGGIPP$HDAscore, percent = TRUE)
ROC18
plot(ROC18, col="navyblue", add=TRUE) # Plot ROC curve
auc(ROC18, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension

# ROC KNN_AGG with Harmonic Mean SECOHDA SDEN
ROC21 <- roc(NieuweSet$anomalybinary, HDAnomalyTableKNN_AGGHMSDEN$HDAscore, percent = TRUE) # Calculate ROC and AUC
ROC21 # Show ROC AUC (area under the curve)
plot(ROC21, col="royalblue", add=TRUE) # Plot ROC curve
auc(ROC21, partial.auc=c(100, 90), partial.auc.focus="sp", partial.auc.correct=TRUE) # Partial AUC for specificity dimension




# B - Early retrieval & confusion matrix 

# Since we do not analyze much cases here (and the false positives therefore, per definition, cannot be extremely high), it is mainly the sensitivity/recall that is relevant here. This represents the percentage of true anomalies amongst the few most extreme anomalies returned by the algorithm.  
NieuweSet$ID <- 1:nrow(NieuweSet)

# With threshold = number of HDAs
NoOfHDAs <- nrow(NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$HighDensityAno1 == TRUE, ])  # Reset
cat(paste0("Threshold set to get ", NoOfHDAs, " anomalies."))
Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1) # Set threshold
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= Threshold1, "Ano_ID"] # Determine anomalies
# Alternatief is de gewone manier: Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=NumberOfHDAs), n=1), "Ano_ID"]
IdentifiedCases <- Ano_ID_anoms %in% NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$HighDensityAno1 == TRUE, "ID"]
sum(IdentifiedCases, na.rm=TRUE) # Number of true anomalies that are identified
table(IdentifiedCases)
cat(paste0("True in the table above refers to the number of correctly identified HDA anomalies."))
# Sensitivity/recall: TP/(TP+FN) = the proportion of actual positives that are correctly identified as such (e.g., the percentage of sick people who are correctly identified as having the condition)
cat(paste0("Fraction of correctly identified anomalies (sensitivity/recall): ", sum(IdentifiedCases, na.rm=TRUE)/NoOfHDAs, "."))
# Precision/PPV: = TP/(TP+FP):
cat(paste0("Fraction of denoted anomalies that are indeed anomalies (precision/PPV): ", sum(IdentifiedCases, na.rm=TRUE) / length(Ano_ID_anoms), "."))
cat(paste0("Fraction of denoted anomalies that are indeed anomalies (precision/PPV): ", sum(IdentifiedCases, na.rm=TRUE) / ( (sum(IdentifiedCases, na.rm=TRUE) + (nrow(NieuweSet)-length(Ano_ID_anoms)) - ( (nrow(NieuweSet)-length(Ano_ID_anoms)) - (length(Ano_ID_anoms)-sum(IdentifiedCases, na.rm=TRUE)) )  ) ), " (alternative calculation)."))
# Accuracy: (TP+TN)/(TP+FP+FN+TN) = the ratio of the correctly labeled cases to all cases:
cat(paste0("Fraction of correctly classified cases (accuracy): ", (sum(IdentifiedCases, na.rm=TRUE) + ( (nrow(NieuweSet)-length(Ano_ID_anoms)) - (length(NieuweSet[NieuweSet$HighDensityAno1==TRUE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE)) ) ) / nrow(NieuweSet), "."))  # Denoted negatives: (nrow(dat1)-length(Ano_ID_anoms))  # From this you should subtract the false negatives, which are: (length(dat1[dat1$HighDensityAno1==TRUE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE))
# Specificity:
IdentifiedCases <- Ano_ID_anoms %in% NieuweSet[NieuweSet$HighDensityAno1==FALSE, "ID"] # As boolean
sum(IdentifiedCases, na.rm=TRUE) # Number of non-HDA cases that are identified incorrectly as HDA cases
cat(paste0("Fraction of correctly identified normal cases (specificity): ", (length(NieuweSet[NieuweSet$HighDensityAno1==FALSE, "ID"]) - sum(IdentifiedCases, na.rm=TRUE)) / length(NieuweSet[NieuweSet$HighDensityAno1==FALSE, "ID"]), "."))


# Confusion matrix based evaluation measures
# We already have a ChosenAnomalyTable. Now also choose the ROC object that corresponds with this
ChosenROCobject <- ROC4  # ROC4 = IPP
# ChosenROCobject <- ROC1  # ROC1 = SECODA EW
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
# ChosenROCobject <- ROC20  # ROC20 = KNN_AGG with Harmonic Mean SECOHDA / HMDH SKEW
# ChosenROCobject <- ROC21  # ROC21 = KNN_AGG with Harmonic Mean SECOHDA / HMDH SDEN

# We will look at the early retrieval anomalies in detail because these are typically the anomalies that one will consider for further analysis or action (i.e. check every metric; the late retrieval anomalies will be studied more generally using ROC curves).
# As opposed to full ROC analysis, we need to pick a specific threshold. We start with the number of HDA anomalies

HDpred <- rep(0, nrow(ChosenAnomalyTable)) # Create vector with zeros
NoOfHDAs <- nrow(NieuweSet[NieuweSet$AnomalyType1 != 0 & NieuweSet$HighDensityAno1 == TRUE, ])  # Reset
Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=NoOfHDAs), n=1) # Reset threshold. Alternative: Threshold1 <- head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(NieuweSet[NieuweSet$HighDensityAno1==TRUE, "HighDensityAno1"])), n=1)  # Determine the threshold
Ano_ID_anoms <- ChosenAnomalyTable[ChosenAnomalyTable$HDAscore <= Threshold1, "Ano_ID"]
HDpred[ChosenAnomalyTable$Ano_ID %in% Ano_ID_anoms] <- 1
# Alternative would be: HDpred <- rep(0, nrow(ChosenAnomalyTable)); HDpred[ChosenAnomalyTable$HDAscore <= head(tail(sort(ChosenAnomalyTable$HDAscore, decreasing=TRUE),n=length(dat1[dat1$HighDensityAno1==TRUE, "HighDensityAno1"])), n=1)] <- 1
table(HDpred)
table(HDpred, NieuweSet$anomalybinary) # Confusion matrix
# Get these same confusion matrix elements from pROC:
coords(ChosenROCobject, Threshold1, ret=c("threshold", "tn", "tp", "fn", "fp"), transpose=TRUE)
# Accuracy from confusion matrix: (TP+TN)/(TP+FP+FN+TN):
( coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("tn"), transpose=TRUE) ) / ( coords(ChosenROCobject, Threshold1, ret=c("tp"), transpose=TRUE) + coords(ChosenROCobject, Threshold1, ret=c("tn"), transpose=TRUE)  + coords(ChosenROCobject, Threshold1, ret=c("fp"), transpose=TRUE)  + coords(ChosenROCobject, Threshold1, ret=c("fn"), transpose=TRUE) )
# Accuracy directly from vectors (Accuracy: Percentage of correctly classified cases)
mean(HDpred == NieuweSet$anomalybinary)
# Accuracy from pROC:
coords(ChosenROCobject, Threshold1, ret=c("accuracy"), transpose=TRUE) 

# We can use the threshold and ROC also to calculate the other evaluation scores with the pROC package
coords(ChosenROCobject, Threshold1, ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "npv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv", "tn", "tp", "fn", "fp"), transpose=TRUE) # Print all metrics

# Using best Youden threshold
# Threshold: coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("threshold"), transpose=TRUE) # Threshold
coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("threshold", "specificity", "sensitivity", "accuracy", "precision", "npv", "1-specificity", "1-sensitivity", "1-accuracy", "1-npv", "1-ppv", "tn", "tp", "fn", "fp"), transpose=TRUE) # Print all metrics for the best Youden threshold. 


# F1 measure (based on precision/ppv and recall)
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
cat(paste0("HMFM measure: ", (4 / (1/coords(ChosenROCobject, Threshold1, ret=c("sensitivity"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("specificity"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("ppv"), transpose=TRUE) + 1/coords(ChosenROCobject, Threshold1, ret=c("accuracy"), transpose=TRUE)) / 100), "."));
# Using best Youden threshold:
cat(paste0("HMFM measure: ", (4 / (1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("sensitivity"), transpose=TRUE) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("specificity"), transpose=TRUE) + 1/(coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("ppv"), transpose=TRUE)) + 1/coords(ChosenROCobject, "best", print.thres.best.method="youden", ret=c("accuracy"), transpose=TRUE)) / 100), "."))


# Calculate Cohen's kappa:
library(fmsb) # ! Note: package fmsb masks roc-object of pROC
Kappa.test(HDpred, NieuweSet$anomalybinary)   
# Alternative
TP = coords(ChosenROCobject, Threshold1, ret="tp", transpose=TRUE); FP = coords(ChosenROCobject, Threshold1, ret="fp", transpose=TRUE); FN = coords(ChosenROCobject, Threshold1, ret="fn", transpose=TRUE); TN = coords(ChosenROCobject, Threshold1, ret="tn", transpose=TRUE);
matrix1 <- matrix(c(TN, FP, FN, TP),2,2)
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
mcc(preds=HDpred, actuals=NieuweSet$anomalybinary)  # Alternative using two binary vectors
# Using best Youden threshold:
mcc(TP=TP2, FP=FP2, TN=TN2, FN=FN2)  # Using explict true positives etc
# The newer version also allows confusion matrices (and uses data.table):
source(paste0(DatFolder, "Matthews.Correlation.Coefficient2.R"))
mcc(TP=TP, FP=FP, TN=TN, FN=FN)  # Using explict true positives etc
mcc(preds=HDpred, actuals=NieuweSet$anomalybinary)  # Needs data.table
mcc(confusionM=matrix1) # Needs data.table
# Using best Youden threshold:
mcc(TP=TP2, FP=FP2, TN=TN2, FN=FN2)  # Using explict true positives etc
mcc(confusionM=matrix2) # Needs data.table


# PRC
library(precrec)
# https://cran.r-project.org/web/packages/precrec/vignettes/introduction.html

table(NieuweSet$anomalybinary)
NieuweSet$anomalybinary[NieuweSet$anomalybinary == 1] <- -1  # Recode anomalies to -1 for precrec package
table(NieuweSet$anomalybinary)
sscurves <- evalmod(scores = ChosenAnomalyTable$HDAscore, labels = NieuweSet$anomalybinary)
# For SOF/spoutlier: sscurves <- evalmod(scores = (max(scoreSOF)-scoreSOF), labels = NieuweSet$anomalybinary)
# For LOF: sscurves <- evalmod(scores = (max(LOFanomalies)-LOFanomalies), labels = NieuweSet$anomalybinary)
sscurves  # Print info
plot(sscurves) # ROC en PRC
plot(sscurves, "PRC") # Only precision-recall (PRC)

# The auc function outputs a data frame with the AUC (Area Under the Curve) scores.
aucs <- precrec::auc(sscurves)  # Het "precrec::" is nodig om er zeker van te zijn dat de auc() van het precrec package wordt gebruikt en niet die van pROC (die een foutmelding geeft)
# View(aucs) # View as table
knitr::kable(aucs) # View with knitr
subset(aucs, curvetypes == "PRC") # Only the PRC. Or: aucs[aucs$curvetypes=="PRC","aucs"] # Only the PRC. ROC: aucs[aucs$curvetypes=="ROC","aucs"]


