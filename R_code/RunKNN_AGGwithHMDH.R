# SECOHDA anomaly detection algorithm for High-Density Anomalies
# (c) Ralph Foorthuis
# 2017, 2019

KNN_AGGwithHMDH <- function(datset, WeightCorrection="SDEN", k_min=10, k_max=15, Normalize=TRUE, TestMode=TRUE) {
  
  # Check hard conditions
  if (WeightCorrection != "None" && WeightCorrection != "SSE" && WeightCorrection != "SDEN" && WeightCorrection != "SKEW") stop("WeightCorrection should be 'None' (no correction), 'SSE' (Single Shannon Entropy measure), 'SDEN' (Single-class Density) or 'SKEW'.")
  
  if (TestMode==TRUE) { cat(paste("Starting initialization of HDA analysis with", WeightCorrection, "weight correction.\n")) }
  
  # Load KNN_AGG functionality
  require(DDoutlier)
  
  # Initialize internal parameters and functions
  VarsTable <- data.frame(VariableName=character(), DataType=character(), stringsAsFactors=FALSE)  # The set of variables and their datatypes that is being analyzed. 
  RescaleVector <- function(vec, toMin, toMax) (toMax-toMin) / (max(vec, na.rm = TRUE)-min(vec, na.rm = TRUE)) * (vec-max(vec, na.rm = TRUE)) + toMax  # Rescaling function.  Example: RescaleVector(c(-10, -9, -5, 2, 6), toMin=0, toMax=100)
  
  # Analyze data types and put in the VarsTable
  if (TestMode==TRUE) { cat(paste("Starting analysis of data types.\n\n")) }
  for (i in names(datset)) {
    if( class(datset[[i]])=="numeric" | class(datset[[i]])=="integer" ) { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="numerical")) 
    } else if (class(datset[[i]])=="factor" | class(datset[[i]])=="character" | class(datset[[i]])=="logical") { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="categorical")) 
    } else { cat(paste0("Warning: Variable '", i, "' of incompatible data type '", class(datset[[i]]), "' is not included in the anomaly detection. The algorithm may need to be updated.\n")) }
  }
  # Check if the dataset is suited for High-Density Anomaly analysis
  if ( length(VarsTable[VarsTable$DataType=="categorical", "DataType"]) <= 0 || length(VarsTable[VarsTable$DataType=="numerical", "DataType"]) <= 0 ) stop("Error: SECOHDA analysis requires both continuous and categorical data.\n") 
  
  VariablesToAnalyze <- c(as.character(VarsTable[VarsTable$DataType=="numerical", "VariableName"]), as.character(VarsTable[VarsTable$DataType=="categorical", "VariableName"]))
  
  # We can now start the HDA analysis:
  
  # Conduct mixed data analysis on the set with all valid variables, in order to detect all types of anomalies (including low-density occurrences)
  
  cat("Creating dummy variables for categorical data.\n")     
  for (i in VariablesToAnalyze) {
    if (i == VariablesToAnalyze[1]) { 
      FormulaString = "~" 
      FormulaString = paste0(FormulaString, " ", i)
    } else {
      FormulaString = paste0(FormulaString, " + ", i)
    }
  }
  FormulaString = as.formula(FormulaString)
  datsetMM <- model.matrix(FormulaString, datset)
  datsetMM <- datsetMM[,2:(ncol(datsetMM))]
  if (Normalize == TRUE) {   # Normalize data if necessary
    normalizeNum <- function(x) { return ((x - min(x)) / (max(x) - min(x))) }  # Normalization function to rescale numerical attributes to the range 0 and 1: (X - min(X))/(max(X) - min(X))
    datsetMM <- as.matrix(as.data.frame(lapply(as.data.frame(datsetMM), normalizeNum)))
  }  
  
  # Run KNN_AGG for mixed data analysis
  cat("Starting mixed data analysis with KNN_AGG.\n")
  KNN_AGGanomalies <- KNN_AGG(dataset=datsetMM, k_min=k_min, k_max=k_max)
  HDAnomalyTableKNN_AGG <- data.frame(Ano_IDM=seq(1:length(KNN_AGGanomalies)), AveAnoScoreMixed=KNN_AGGanomalies)
  
  # Conduct numerical analysis on the continuous variables
  cat("Starting numerical analysis with KNN_AGG.\n")
  KNN_AGGanomalies <- KNN_AGG(dataset=datsetMM[,VarsTable[VarsTable$DataType=="numerical", "VariableName"]], k_min=k_min, k_max=k_max) 
  HDAnomalyTableKNN_AGG$Ano_IDN <- seq(1:length(KNN_AGGanomalies))
  HDAnomalyTableKNN_AGG$AveAnoScoreNum <- KNN_AGGanomalies  
  
  # Calculate HDA scores
  if (TestMode==TRUE) { cat(paste("Calculating HDA scores \n")) }
  HDAnomalyTableKNN_AGG$AveAnoScoreMixedRev <- HDAnomalyTableKNN_AGG$AveAnoScoreMixed  # Does not need to be reversed (like the original SECODA version) because the higher scores already represent more anomalousness, but the column name is kept for simplicity
  HDAnomalyTableKNN_AGG$AveAnoScoreNumRescaled <- max(HDAnomalyTableKNN_AGG$AveAnoScoreNum, na.rm=TRUE) - HDAnomalyTableKNN_AGG$AveAnoScoreNum  # Reverse scores, so that highest scores represent the most dense areas
  HDAnomalyTableKNN_AGG$AveAnoScoreNumRescaled <- RescaleVector(HDAnomalyTableKNN_AGG$AveAnoScoreNumRescaled, toMin = min(HDAnomalyTableKNN_AGG$AveAnoScoreMixedRev), toMax = max(HDAnomalyTableKNN_AGG$AveAnoScoreMixedRev)) # Rescale numerical scores (which can be expected to have a bigger range) to the scale of the mixed scores. So the numerical score does not weigh more in and by itself.
    
  if (WeightCorrection == "None") {
    WeightADSnum <- 1  # No correction, so the weight of the numerical anomaly score is 1
  } else if (WeightCorrection == "SSE" || WeightCorrection == "SDEN" || WeightCorrection == "SKEW") {
    # A correction of the weight of the numerical anomaly score will be conducted. For this we will use the (concatenation of) categorical attribute values.
    HDAnomalyTableKNN_AGG$CatValues <- do.call(paste, c(as.data.frame(datset[HDAnomalyTableKNN_AGG$Ano_IDM,VarsTable[VarsTable$DataType=="categorical","VariableName"]]), list(sep="."))) # Insert concatenation of categorical values. This provides a list of categorical value combinations. 
    if (WeightCorrection == "SSE") { # Use single Shannon entropy
      # ! De-comment the two lines below to also include density/spatial information in the Shannon entropy
      # HDAnomalyTableKNN_AGG$DiscretizedDensity = as.character(cut(HDAnomalyTableKNN_AGG$AveAnoScoreNum, breaks = 7))  # Discretize the density scores, which can then be included as dispersion/spatial information in the entropy measure.
      # HDAnomalyTableKNN_AGG$CatValues <- do.call(paste, c(as.data.frame(HDAnomalyTableKNN_AGG[HDAnomalyTableKNN_AGG$Ano_IDM, c("CatValues", "DiscretizedDensity")]), list(sep=".")))  # Add to categorical concatenation.
	  
	  Probabilities <- table(HDAnomalyTableKNN_AGG$CatValues)/length(HDAnomalyTableKNN_AGG$CatValues) # Calculate probabilities of each combination
      SSE <- -sum(Probabilities * log2(Probabilities)) # Calculate Shannon information entropy
      WeightADSnum <- SSE/log2(length(unique(HDAnomalyTableKNN_AGG$CatValues))) # Calculate weight as the relative entropy, i.e. the fraction entropy of the theoretical maximum entropy (see e.g. Maxion and Tan 2000). The maximum entropy is: log2(length(unique(HDAnomalyTableKNN_AGG$CatValues)))
      # Alternative would be to calculate SSE, and subsequently the maximum entropy by a vector of equal probabilities:  Probabilities <- rep(1/length(unique(HDAnomalyTableKNN_AGG$CatValues)), length(unique(HDAnomalyTableKNN_AGG$CatValues))); SSEmax <- -sum(Probabilities * log2(Probabilities)); WeightADSnum <- SSE/SSEmax # First create vector with equal probabilities, then theoretical maximum Shannon entropy, and the weight.
    } else if (WeightCorrection == "SDEN") { # Use Single-class Density
      DensityPerCatValues <- aggregate(HDAnomalyTableKNN_AGG[, c("AveAnoScoreMixed", "AveAnoScoreNum")], list(HDAnomalyTableKNN_AGG$CatValues), mean)
      colnames(DensityPerCatValues)[1] <- "CatValue"
      HighestAverageDensity <- max(DensityPerCatValues$AveAnoScoreMixed)
      CatValuesWithHighestScores <- DensityPerCatValues[DensityPerCatValues$AveAnoScoreMixed >= HighestAverageDensity, "CatValue"] # Identify CatValue(s) with highest density scores
      AverageDensityOfLowerAverageDensities <- 1/mean(1/(DensityPerCatValues[!DensityPerCatValues$CatValue %in% CatValuesWithHighestScores, "AveAnoScoreMixed"])) # Use the harmonic mean to calculate the average density of the CatValues with lower density scores      
      # The arithmetic mean works less well: AverageDensityOfLowerAverageDensities <- mean(DensityPerCatValues[!DensityPerCatValues$CatValue %in% CatValuesWithHighestScores, "AveAnoScoreMixed"]) # Use the arithmetic mean to calculate the average density of the CatValues with lower density scores
      WeightADSnum <- AverageDensityOfLowerAverageDensities/HighestAverageDensity # Determine the weight by calculating the fraction
    } else if (WeightCorrection == "SKEW") { # Use Skew
      
    }
    # 
  }
  
  cat(paste("Weight for the numerical density vector set at", WeightADSnum, "\n"))
  
  HDAnomalyTableKNN_AGG$HDAscoreRev <- (1+WeightADSnum) / (1/HDAnomalyTableKNN_AGG$AveAnoScoreMixedRev + WeightADSnum/HDAnomalyTableKNN_AGG$AveAnoScoreNumRescaled) # Calculate (weighted) harmonic mean of reversed mixed data score and rescaled non-reversed numerical score, using WeightADSnum as the weight for the numerical scores and 1 as the weight for the mixed scores.
  HDAnomalyTableKNN_AGG$HDAscore <- 1 + max(HDAnomalyTableKNN_AGG$HDAscoreRev, na.rm=TRUE) - HDAnomalyTableKNN_AGG$HDAscore # Reverse HDA scores (so that the most extreme anomalies get lowest scores, analogous to regular SECODA)
  
  cat(paste("Finished calculation \n"))
  
  # Return HDA scores
  colnames(HDAnomalyTableKNN_AGG)[1] <- "Ano_ID"
  return(HDAnomalyTableKNN_AGG[, c("Ano_ID", "HDAscore")]) 
  
}

