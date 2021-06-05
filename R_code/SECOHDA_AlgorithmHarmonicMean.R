# SECOHDA anomaly detection algorithm for High-Density Anomalies using HMDH
# (c) Ralph Foorthuis
# 2019, 2020

SECOHDA <- function(datset, WeightCorrection="SDEN", TestMode=TRUE, MinimumNumberOfSECODAIterations=7, StartSECODAHeuristicsAfterIteration=10) {
  
  # Check hard conditions
  if (WeightCorrection != "None" && WeightCorrection != "SSE" && WeightCorrection != "SDEN" && WeightCorrection != "SKEW") stop("WeightCorrection should be 'None' (no correction), 'SSE' (Single Shannon Entropy measure), 'SDEN' (Single-class Density) or 'SKEW' (Skew weights).")
  
  if (TestMode==TRUE) { cat(paste("Starting initialization of HDA analysis with", WeightCorrection, "weight correction.\n")) }
  
  # Load SECODA functionality
  library(SECODA)  
  
  # Initialize internal parameters and functions
  VarsTable <- data.frame(VariableName=character(), DataType=character(), stringsAsFactors=FALSE)  # The set of variables and their datatypes that is being analyzed. 
  RescaleVector <- function(vec, toMin, toMax) (toMax-toMin) / (max(vec, na.rm = TRUE)-min(vec, na.rm = TRUE)) * (vec-max(vec, na.rm = TRUE)) + toMax  # Rescaling function. Example: RescaleVector(c(-10, -9, -5, 2, 6), toMin=0, toMax=100)
  
  # Analyze data types and put in the VarsTable
  if (TestMode==TRUE) { cat(paste("Starting analysis of data types.\n\n")) }
  for (i in names(datset)) {
    if( class(datset[[i]])=="numeric" | class(datset[[i]])=="integer" ) { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="numerical")) 
    } else if (class(datset[[i]])=="factor" | class(datset[[i]])=="character" | class(datset[[i]])=="logical") { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="categorical")) 
    } else { cat(paste0("Warning: Variable '", i, "' of incompatible data type '", class(datset[[i]]), "' is not included in the anomaly detection. The SECODA algorithm may need to be updated.\n")) }
  }
  # Check if the dataset is suited for High-Density Anomaly analysis
  if ( length(VarsTable[VarsTable$DataType=="categorical", "DataType"]) <= 0 || length(VarsTable[VarsTable$DataType=="numerical", "DataType"]) <= 0 ) stop("Error: SECOHDA analysis requires both continuous and categorical data.\n") 
  
  # We can now start the HDA analysis:
  
  # Conduct mixed data analysis
  cat("Starting mixed data analysis.\n")   
  HDAnomalyTable <- SECODA(datset, MinimumNumberOfIterations=MinimumNumberOfSECODAIterations, StartHeuristicsAfterIteration=StartSECODAHeuristicsAfterIteration, TestMode="Normal")
  colnames(HDAnomalyTable) <- c("Ano_IDM","AveAnoScoreMixed") # Geef variabele andere naam
  
  # Conduct numerical analysis
  cat("Starting numerical analysis.\n")   
  AnomalyTableTemp <- SECODA(datset[,VarsTable[VarsTable$DataType=="numerical", "VariableName"]], MinimumNumberOfIterations=MinimumNumberOfSECODAIterations, StartHeuristicsAfterIteration=StartSECODAHeuristicsAfterIteration, TestMode="Normal")
  HDAnomalyTable$Ano_IDN <- AnomalyTableTemp$Ano_ID; HDAnomalyTable$AveAnoScoreNum <- AnomalyTableTemp$AveAnoScore
  rm(AnomalyTableTemp)
  
  # Calculate HDA scores
  if (TestMode==TRUE) { cat(paste("Calculating HDA scores \n")) }
  HDAnomalyTable$AveAnoScoreMixedRev <- 1 + max(HDAnomalyTable$AveAnoScoreMixed, na.rm=TRUE) - HDAnomalyTable$AveAnoScoreMixed  # Reverse mixed scores (so that the most extreme anomalies get highest scores). In order to calculate the harmonic mean, we want to have anomalies to have the highest scores. This way, the biggest HD anomalies are those cases that score high on both mixed data anomalies and (reversed) numerical data anomalies.
  HDAnomalyTable$AveAnoScoreNumRescaled <- RescaleVector(HDAnomalyTable$AveAnoScoreNum, toMin = min(HDAnomalyTable$AveAnoScoreMixedRev), toMax = max(HDAnomalyTable$AveAnoScoreMixedRev)) # Rescale numerical scores (which can be expected to have a bigger range) to the scale of the mixed scores. So the numerical score does not weigh more in and by itself.
  
  if (WeightCorrection == "None") {
    WeightAASnum <- 1  # No correction, so the weight of the numerical anomaly score is 1
  } else if (WeightCorrection == "SSE" || WeightCorrection == "SDEN" || WeightCorrection == "SKEW") {
    # A correction of the weight of the numerical anomaly score will be conducted. For this we will use the (concatenation of) categorical attribute values.
    HDAnomalyTable$CatValues <- do.call(paste, c(as.data.frame(datset[HDAnomalyTable$Ano_IDM,VarsTable[VarsTable$DataType=="categorical","VariableName"]]), list(sep="."))) # Insert concatenation of categorical values. This provides a list of categorical value combinations. 
    if (WeightCorrection == "SSE") { # Use single Shannon entropy
      # ! De-comment the two lines below to also include density/spatial information in the Shannon entropy
      # HDAnomalyTable$DiscretizedDensity = as.character(cut(HDAnomalyTable$AveAnoScoreNum, breaks = 7))  # Discretize the density scores, which can then be included as dispersion/spatial information in the entropy measure.
      # HDAnomalyTable$CatValues <- do.call(paste, c(as.data.frame(HDAnomalyTable[HDAnomalyTable$Ano_IDM, c("CatValues", "DiscretizedDensity")]), list(sep=".")))  # Add to categorical concatenation.
      
      Probabilities <- table(HDAnomalyTable$CatValues)/length(HDAnomalyTable$CatValues) # Calculate probabilities of each combination
      SSE <- -sum(Probabilities * log2(Probabilities)) # Calculate Shannon information entropy
      WeightAASnum <- SSE/log2(length(unique(HDAnomalyTable$CatValues))) # Calculate weight as the relative entropy, i.e. the fraction entropy of the theoretical maximum entropy (see e.g. Maxion and Tan 2000). The maximum entropy is: log2(length(unique(HDAnomalyTable$CatValues)))
      # Alternative would be to calculate SSE, and subsequently the maximum entropy by a vector of equal probabilities:  Probabilities <- rep(1/length(unique(HDAnomalyTable$CatValues)), length(unique(HDAnomalyTable$CatValues))); SSEmax <- -sum(Probabilities * log2(Probabilities)); WeightAASnum <- SSE/SSEmax # First create vector with equal probabilities, then theoretical maximum Shannon entropy, and the weight.
    } else if (WeightCorrection == "SDEN") { # Use Single-class Density
      DensityPerCatValues <- aggregate(HDAnomalyTable[, c("AveAnoScoreMixed", "AveAnoScoreNum")], list(HDAnomalyTable$CatValues), mean)
      colnames(DensityPerCatValues)[1] <- "CatValue"
      HighestAverageDensity <- max(DensityPerCatValues$AveAnoScoreMixed)
      CatValuesWithHighestScores <- DensityPerCatValues[DensityPerCatValues$AveAnoScoreMixed >= HighestAverageDensity, "CatValue"] # Identify CatValue(s) with highest density scores
      AverageDensityOfLowerAverageDensities <- 1/mean(1/(DensityPerCatValues[!DensityPerCatValues$CatValue %in% CatValuesWithHighestScores, "AveAnoScoreMixed"])) # Use the harmonic mean to calculate the average density of the CatValues with lower density scores      
      # The arithmetic mean works less well: AverageDensityOfLowerAverageDensities <- mean(DensityPerCatValues[!DensityPerCatValues$CatValue %in% CatValuesWithHighestScores, "AveAnoScoreMixed"]) # Use the arithmetic mean to calculate the average density of the CatValues with lower density scores
      WeightAASnum <- AverageDensityOfLowerAverageDensities/HighestAverageDensity # Determine the weight by calculating the fraction
    } else if (WeightCorrection == "SKEW") { # Use Skew
      
    }
  }
  
  cat(paste("Weight for the numerical density vector set at", WeightAASnum, "\n"))
  
  HDAnomalyTable$HDAscoreRev <- (1+WeightAASnum) / (1/HDAnomalyTable$AveAnoScoreMixedRev + WeightAASnum/HDAnomalyTable$AveAnoScoreNumRescaled) # Calculate (weighted) harmonic mean of reversed mixed data score and rescaled non-reversed numerical score, using WeightAASnum as the weight for the numerical scores and 1 as the weight for the mixed scores.
  HDAnomalyTable$HDAscore <- 1 + max(HDAnomalyTable$HDAscoreRev, na.rm=TRUE) - HDAnomalyTable$HDAscore # Reverse HDA scores (so that the most extreme anomalies get lowest scores, analogous to regular SECODA)
  
  cat(paste("Finished calculation \n"))
  
  # Return HDA scores
  colnames(HDAnomalyTable)[1] <- "Ano_ID"
  return(HDAnomalyTable[, c("Ano_ID", "HDAscore")]) 
  
}

