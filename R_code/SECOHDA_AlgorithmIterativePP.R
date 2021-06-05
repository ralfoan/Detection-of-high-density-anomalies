# SECOHDA anomaly detection algorithm for High-Density Anomalies, using the Iterative Partial Push method
# (c) Ralph Foorthuis
# 2019, 2020

SECOHDA <- function(datset, QuantileDenominator = 100, QuantileFilterBoost = -9999, TestMode=TRUE, MinimumNumberOfSECODAIterations=7, StartSECODAHeuristicsAfterIteration=10) {
  # The QuantileDenominator determines the degree of detail during the analysis. The higher the parameter, the more detailed the analysis (and the slower the performance). The QuantileDenominator is typically 100 or 1000.
  # The QuantileFilterBoost represents the (in)sensitivity. It is the percentage of numerically isolated anomalies that are additionally filtered away from the general anomalies. The higher the QuantileFilterBoost, the less risk of isolated anomalies being labelled as most extreme anomalies (i.e. less false positives amongst the lowest scores), but the higher the risk of true high-density anomalies being missed (i.e. more false negatives). A value of -9999 means this setting is determined automatically.
  
  # Check hard conditions
  if ( class(datset) != "data.frame" && class(datset) != "matrix") stop("Input datset should be a data.frame or a matrix.")
  if ( class(QuantileFilterBoost) != "integer" && class(QuantileFilterBoost) != "numeric" ) stop("QuantileFilterBoost parameter should be a number between 0 and 100.")
  if ( QuantileFilterBoost != -9999 && (QuantileFilterBoost < 0 || QuantileFilterBoost > 100) ) stop("QuantileFilterBoost parameter should be either -9999 (for automatically determining the optimal QuantileFilterBoost) or a number between 0 and 100.")
  
  if (TestMode==TRUE) { 
    cat(paste0("Initializing HDA analysis, with QuantileDenominator ", QuantileDenominator, " and QuantileFilterBoost ", QuantileFilterBoost, ".\n")) 
    if (QuantileFilterBoost == -9999) cat("QuantileFilterBoost set at -9999 (automatic), so optimal value for QuantileFilterBoost will be set by SECOHDA.\n\n")
  }
  
  # Load SECODA functionality
  library(SECODA)  
  
  # Initialize internal parameters and functions
  StartIteration <- TRUE
  CurrentIteration <- 0  # The current iteration.
  VarsTable <- data.frame(VariableName=character(), DataType=character(), stringsAsFactors=FALSE)  # The set of variables and their datatypes that is being analyzed. 
  
  # Analyze data types and put in the VarsTable
  if (TestMode==TRUE) { cat(paste("Starting analysis of data types.\n\n")) }
  for (i in names(datset)) {
    if( class(datset[[i]])=="numeric" | class(datset[[i]])=="integer" ) { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="numerical")) 
    } else if (class(datset[[i]])=="factor" | class(datset[[i]])=="character" | class(datset[[i]])=="logical") { VarsTable <- rbind(VarsTable, data.frame(VariableName=i, DataType="categorical")) 
    } else { cat(paste0("Warning: Variable '", i, "' of incompatible data type '", class(datset[[i]]), "' is not included in the anomaly detection. The SECODA algorithm may need to be updated.\n")) }
  }
  VariablesToAnalyze <- c(as.character(VarsTable[VarsTable$DataType=="numerical", "VariableName"]), as.character(VarsTable[VarsTable$DataType=="categorical", "VariableName"]))
  if ( length(VarsTable[VarsTable$DataType=="categorical", "DataType"]) <= 0 || length(VarsTable[VarsTable$DataType=="numerical", "DataType"]) <= 0 ) stop("SECOHDA analysis requires both continuous and categorical data.") 
  
  # Start HDA analysis
  
  # Conduct mixed data analysis on the set with all valid variables, in order to detect all types of anomalies (including low-density occurrences)
  cat("Starting mixed data analysis.\n")   
  HDAnomalyTable <- SECODA(datset[,VariablesToAnalyze], MinimumNumberOfIterations=MinimumNumberOfSECODAIterations, StartHeuristicsAfterIteration=StartSECODAHeuristicsAfterIteration, TestMode="Normal") 
  colnames(HDAnomalyTable) <- c("Ano_IDM","AveAnoScoreMixed") # Change variable names
  
  # Conduct numerical analysis on the continuous variables
  cat("Starting numerical analysis.\n")   
  HDAnomalyTableNumObject <- SECODA(datset[,VarsTable[VarsTable$DataType=="numerical", "VariableName"]], MinimumNumberOfIterations=MinimumNumberOfSECODAIterations, StartHeuristicsAfterIteration=StartSECODAHeuristicsAfterIteration, TestMode="FullReturn")  # Run anomaly detection on continuous variables. Choose FullReturn mode in order to obtain process information, such as the number of iterations. We will need this to determine the overall multivariate density of the dataset.
  HDAnomalyTable$Ano_IDN <- HDAnomalyTableNumObject$AnomalyScores$Ano_ID; HDAnomalyTable$AveAnoScoreNum <- HDAnomalyTableNumObject$AnomalyScores$AveAnoScore  # Add results to HDAnomalyTable
  ProgressTableNum <- HDAnomalyTableNumObject$ProgressTable  # Store process information
  rm(HDAnomalyTableNumObject)
  
  # KAN WEG:
  HDAnomalyTable$AveAnoScoreNumRev <- 1 + max(HDAnomalyTable$AveAnoScoreNum, na.rm=TRUE) - HDAnomalyTable$AveAnoScoreNum # Reverse numerical scores
  
  # If needed, calculate QuantileFilterBoost
  if (QuantileFilterBoost == -9999) {  # -9999 represents automatically determining the QuantileFilterBoost based on the overall (numerical) multivariate density
    NumberOfNumericVariables <- length(VarsTable[VarsTable$DataType=="numerical", "VariableName"])  # Select variables that need to be analyzed to determine the overall density.
    UltimateArity <- max(ProgressTableNum$Arity)  # Determine the optimal amount number of discretization bins/intervals, as determined by SECODA when calculating the AveAnoScoreNum
    ExpectedRandomDensity <- nrow(datset) / UltimateArity^NumberOfNumericVariables  # This is the density (the number of data points per multidimensional bin) that you would expect if the data points were randomly distributed throughout the numerical space (in terms of the number of multi-dimensional bins used in the numerical SECODA analysis).
    QuantileFilterBoost <- ExpectedRandomDensity / mean(HDAnomalyTable$AveAnoScoreNum) * 100 * 2  # Calculate new QuantileFilterBoost by taking the ratio of the expected density and the observed density.
    cat(paste0("New QuantileFilterBoost automatically set at ", QuantileFilterBoost, ".\n"))
  }
  
  # Calculate HDA scores
  if (TestMode==TRUE) { cat(paste("Calculating HDA scores.\n")) }
  HDAnomalyTable$HDAscore <- as.integer(NA)  # Create variable to store the score (which will be the iteration plus the decimals based on the other anomaly scores) with which the HD anomaly is identified.
  
  cat(paste("Percentage complete: "))
  while (StartIteration==TRUE) {
    CurrentIteration <- CurrentIteration + 1
    Ano_IDs_Mixed <- HDAnomalyTable[HDAnomalyTable$AveAnoScoreMixed < quantile(HDAnomalyTable$AveAnoScoreMixed, probs=CurrentIteration/QuantileDenominator, names=FALSE), "Ano_IDM"] # Identify the set of mixed data anomalies based on the current fraction of CurrentIteration/QuantileDenominator
    Ano_IDs_Numerical <- HDAnomalyTable[HDAnomalyTable$AveAnoScoreNum < quantile(HDAnomalyTable$AveAnoScoreNum, probs=min( ((QuantileDenominator-1)/QuantileDenominator), ((CurrentIteration+QuantileDenominator/100*QuantileFilterBoost)/QuantileDenominator) ), names=FALSE), "Ano_IDN"]  # Identify the set of numerical anomalies based on the current fraction of CurrentIteration/QuantileDenominator. To avoid filtering too little numerical anomalies away (resulting in non-HDA's being 'detected'), select an additional percentage of numerical cases here, so as to filter away more compared to Ano_IDs_Mixed (which is done by the +QuantileDenominator/100*QuantileFilterBoost statement; if the QuantileFilterBoost = 1 then an additional 1% is filtered away, if it is 2 then 2%, and so on). The min() statement is to ensure the final iterations do not yield a value above 1, which would give an error. The intuition behind using the QuantileFilterBoost is that it accounts for the degree of dispersion/clustering in the distribution. If the degree of dispersion is low (i.e. the degree of clustering is high), then the QuantileFilterBoost will be low and only few additional cases are filtered away. However, if there is not much high-density clustering then many cases are scattered, and more low-density cases thus need to be filtered away later (with the setdiff() statement) to avoid too many false positives (i.e. too many isolated cases are denoted HDAs).  
    
    # Filter away isolated cases from the total set of anomalies
    Ano_IDs_Mixed_With_Numerical_Excluded <- setdiff(Ano_IDs_Mixed, Ano_IDs_Numerical)
    
    # Store the current HDA score (iteration number and sub-order based on the other anomaly scores) for the identified HD anomalies, but only if no iteration number is present yet (i.e. is a NA)
    IDsOfNAs <- which(is.na(HDAnomalyTable$HDAscore)) # Identify rows with empty Score cells. In the first iteration this will be all rows, and this set get smaller after each iteration because more and more rows will have been scored.
    Ano_IDs_ToUpdate <- intersect(Ano_IDs_Mixed_With_Numerical_Excluded, IDsOfNAs) # The intersection of HDA anomalies and NA's: these are the HDA anomalies that have not been identified in a previous iteration
    
    # Add decimals to iteration score for precision. All cases will get a unique HDA score.
    HDtemp <- HDAnomalyTable[Ano_IDs_ToUpdate,] # Create data.frame with cases to update
    HDtemp <- HDtemp[with(HDtemp, order(AveAnoScoreMixed, -AveAnoScoreNum, Ano_IDN)), ] # Sort on AveAnoScoreMixed (ascending), AveAnoScoreNum (descending because high-density is more anomalous), Ano_IDN (ascending) to determine order within this set (lower in the order is more anomalous)
    
    if (nrow(HDtemp) > 0) HDtemp$Score <- as.numeric(paste0(CurrentIteration, ".", formatC(seq(1:nrow(HDtemp)), width=nchar(as.character(nrow(HDtemp))), flag="0"))) # Put in the score in HDtemp. Only if there are anomalies found (because seq(1:0) leads to 2 results otherwise, which you cannot insert in an empty HDtemp)
    HDAnomalyTable$HDAscore[HDtemp$Ano_IDM] <- HDtemp$Score # Store HDA scores in HDAnomalyTable. Do not overwrite previously identified HDAs, as the first iteration number needs to be retained in order to acknowledge that they are more extreme HDAs (a lower Iteration number means a lower end score, and represents a more extreme HDA anomaly).
    
    if (CurrentIteration %% (QuantileDenominator/10) == 0) cat(paste0(CurrentIteration/QuantileDenominator*100, "%  ")) # Print progress for every 10% progress.
    
    if (CurrentIteration >= QuantileDenominator) StartIteration=FALSE
  }
  
  cat(paste("\n\nNumber of potential HDA cases identified:", nrow(datset)-length(which(is.na(HDAnomalyTable$HDAscore))),  "\n"))
  
  # The above code identified candidate high-density cases. The isolated cases are still not included. So score them by giving them high scores, with the most isolated cases getting the most the highest scores.
  HDAnomalyTable$HDAscore <- ifelse(is.na(HDAnomalyTable$HDAscore), (1 + max(HDAnomalyTable$AveAnoScoreNum, na.rm=TRUE) - HDAnomalyTable$AveAnoScoreNum) + QuantileDenominator, HDAnomalyTable$HDAscore) # If the Iteration has a value then keep that. Otherwise (if it's empty), then fill it with the anomaly score of numerical/density-based data anomalies (and add the QuantileDenominator (i.e. the highest iteration) to ensure they are higher than the identified HDA anomalies).  
  
  # Return HDA scores
  colnames(HDAnomalyTable)[1] <- "Ano_ID"
  return(HDAnomalyTable[, c("Ano_ID", "HDAscore")]) 
}

