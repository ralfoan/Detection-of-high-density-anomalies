# Detection-of-high-density-anomalies
Algorithms for the R environment that are able to detect high-density anomalies. Such anomalies are deviant cases positioned in the most normal regions of the data space.

# Info

Ralph Foorthuis,
June 6th 2021


This zip-file contains the labeled datasets and R code to detect high-density anomalies, as describted in the publication [Algorithmic Frameworks for the Detection of High-Density Anomalies](https://arxiv.org/abs/2010.04705) (Foorthuis, 2020). 


Notes:

1. Make sure R (and possibly RStudio) is installed.
2. Install the [SECODA](https://github.com/ralfoan/SECODA) R package (or use the "SECODA.R" code file) and other packages (foreign, RGL, data.table; and possibly DDoutlier, dbscan, spoutlier, pROC, fmsb, precrec, plyr and ggplot2).
3. The datasets are in ARFF format and include metadata and comments. The .R files with the dataset in the file name (e.g. "Multiset4D.R") contain the R code examples that illustrate how to analyze the data. (Note that many examples are also for regular anomaly detection, to facilitate a comparison with the HDA algorithmic frameworks.) The other .R files are the algorithms and algorithmic frameworks (see below).
4. Conduct high-density anomaly detection using the IPP (Iterative Partial Push) or HMDH (Harmonic Mean Detection of HDAs) framework (see below). An easy way to start is using the code in "HD_Gleuf.R". It is highly advised to install the package RGL for 3D visualization.


Files offering algorithms for the detection of high-density anomalies:

- "SECOHDA_AlgorithmIterativePP.R": IPP (Iterative Partial Push) framework using SECODA as underlying algorithm.
- "SECOHDA_AlgorithmHarmonicMean.R": HMDH (Harmonic Mean Detection of HDAs) framework using SECODA as underlying algorithm.
- "SECODA.R": The SECODA algorithm for general-purpose (non-HDA) detection of anomalies. Can be used as an underlying algorithm in the HDA frameworks. SECODA is also available as a package (see "SECODA_0.5.4.tar.gz" and "SECODA_0.5.4.zip" in the [SECODA repository](https://github.com/ralfoan/SECODA)).
- These files offer IPP or HMDH functionality with nearest neighbor or LOF as underlying algorithm. See the publication for more details. Files: "RunSOFwithIPP.R", "RunLOFwithIPP.R", "RunKNN_AGGwithIPP.R", "RunKNN_AGGwithHMDH.R".

