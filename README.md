# Detection-of-high-density-anomalies
Algorithms for the R environment that are able to detect high-density anomalies. Such anomalies are deviant cases positioned in the most normal regions of the data space.

# Info

Ralph Foorthuis
October 10th 2020


This zip-file contains the labeled datasets and R code to detect high-density anomalies, as describted in the publication 'Algorithmic Frameworks for the Detection of High-Density Anomalies' (Foorthuis, 2020). 


Notes:

1. Make sure R (and possibly RStudio) is installed.
2. Install SECODA (or use the "SECODA.R" code file) and other packages (foreign, RGL, data.table; and possibly DDoutlier, dbscan, spoutlier, pROC, fmsb, precrec, plyr and ggplot2).
3. The datasets are in ARFF format and include metadata and comments. The .txt files contain the R code examples.
4. Conduct high-density anomaly detection using the IPP (Iterative Partial Push) or HMDH (Harmonic Mean Detection of HDAs) framework (see below). An easy way to start is using the code in "HD_Gleuf.txt". It is highly advised to install the pacakge RGL for 3D visualization.



Files offering algorithms for the detection of high-density anomalies:

- "SECOHDA_AlgorithmIterativePP.R": IPP (Iterative Partial Push) framework using SECODA as underlying algorithm.
- "SECOHDA_AlgorithmHarmonicMean.R": HMDH (Harmonic Mean Detection of HDAs) framework using SECODA as underlying algorithm.
- "SECODA.R", "SECODA_0.5.4.tar.gz" and "SECODA_0.5.4.zip": The SECODA algorithm for general-purpose (non-HDA) detection of anomalies. Can be used as an underlying algorithm in the HDA frameworks.
- These files offer IPP or HMDH functionality with nearest neighbor or LOF as underlying algorithm. See the publication for more details. Files: "RunSOFwithIPP.R", "RunLOFwithIPP.R", "RunKNN_AGGwithIPP.R", "RunKNN_AGGwithHMDH.R".

