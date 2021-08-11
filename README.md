![banner](REGGAE_banner.png)
===

# REGGAE 
Regression Generator and Analyzer

Allows user to run statistical analysis on .csv datasets using the R programming language and free software environment.
Statistical diagnostics include:

  * Multivariate Linear or Random Forest (RF) regression models 
  * feature selection (e.g., full, stepwise, dredge, or minimal feature correlation)
  * pairwise correlations
  * cross validation analysis (e.g., q<sup>2</sup>, Leave-One-Out CV, K-fold CV, and external validation R<sup>2</sup> values)
  * QSAR criteria for an acceptable model
  * principal component analysis (PCA)
  * build scaled/unscaled train and test sets (e.g., random or Universal training set)
  * ANOVA analysis
  * and, plots for selected analysis.


### Features
The program produces a linear regression model by default by building the model (or specifying the varibles to include in the model):
 * `-b` or `--buildmodel` 

Or, to build a linear model using the feature selection process:
 * `-m` or `--model` - Options include: full, stepwise, dredge, or mincorr. Mincorr requires `-c` option with specified correlation value. 
To include a Random Forest model or an optimized paramerized RF model, select options respectively: rforest or opt-forest with specified variables using `-b`.
 
Arguments for statistical analysis include: 
 * `-c` or `--corrplot` for variable correlations. Requires a value between 0 and 1. NOTE: Corrplot is not displaying any text labels. Still can obtain correlation values. 
 * `-q` or `--crossvalidation` for all CV analysis.
 * `-d` or `--diagnostics` for F-value comparisons on full and reduced model, QSAR Test criteria, and collinearity diagnostics.
 * `-p` or `--pca` for principal component analysis and grouping with k-means clustering. Requires number of clusters. Can specify number of PCs with second number. (e.g., 4,3 gives 4 clusters, 3 components)
 * `-r` or `--randsample` for 

Additional argument specifications include:
 * `-i` or `--inputfile`
 * `-o` or `--outputfile`
 * `-y` or `--yresponse`
 * `-x` or `--extdata`
 * `-e` or `--exportdata`
 * `-s` or `--seed`
 * `-K` or `--Kfoldvalue`
 * `-v` or `--verbose`
 * `-h` or `--help` 


### Examples 



### Install
1. Create a new conda environment with all the r-essentials conda packages built from CRAN:
   `conda create -n r_env r-essentials r-base`
   
2. Activate the r_env:
   `conda activate r_env`
   Note: To list the r packages already installed in r_env: `conda list`
   
3. Install all packages required for REGGAE with conda:
   `conda install --yes --file requirements.txt`
   
4. Confirm by running REGGAE help options:
   `Rscript reggae.r -h`


### Dependencies
Anaconda installation required for r packages installation. 
All available r packages with conda: https://docs.anaconda.com/anaconda/packages/r-language-pkg-docs/

### References
