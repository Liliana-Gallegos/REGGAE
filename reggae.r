#!/usr/local/bin/Rscript

##################################################
#### REGGAE - Regression Generator & Analyzer ####
#### VERSION: 6   -  2021-16-02               ####
#### AUTHOR: Liliana C. Gallegos              ####
#### EMAIL: lilianac.gallegos@colostate.edu   ####
##################################################

# Loading packages
options(warn=-1)
suppressPackageStartupMessages({
  library(optparse)
  library(dplyr)
  library(corrplot)
  library(MuMIn)
  library(cvq2)
  library(car)
  library(ggplot2)
  library(caret)
  library(R1magic)
  # library(DEMOVA)
  library(randomForest)  # conda install -c r r-randomforest
  library(Metrics) # conda install -c conda-forge r-metrics
  library(RColorBrewer)
  library(scales)
  })

option_list = list(
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Print extra output [defaul = %default]"),
  make_option(c("-i", "--inputfile"), default=NULL,
              help="Requires input data file in csv format. NOTE: uses first column as row names.", metavar="character"),
  make_option(c("-o", "--outputfile"), type="character", default="REGGAE-analysis-output.txt",
              help="Optional: output file name [default = %default]", metavar="character"),
  make_option(c("-x", "--extdata"), type="character", default= FALSE,
              help="Validate model using external csv data set. Specify the name of csv data file."),
  make_option(c("-e", "--exportdata"), type="character", default= FALSE,
              help="Export data sets. Select option: scaled, unscaled, predicted"),
  make_option(c("-m", "--model"), type="character", default=FALSE,
              help="Types of linear regression model include: full, stepwise, dredge, mincorr, and rforest."),
  make_option(c("-b", "--buildmodel"), type="character", default= FALSE,
              help="Build linear model from input variables."),
  make_option(c("-y", "--yresponse"), default="None",
              help="Requires defining the y-response variable for given dataframe. Required to run model."),
  make_option(c("-p", "--pca"), type="character", default= FALSE,
              help="Run principal component analysis and group with k-means clustering. Can be used to split data along with -r option. Requires number of clusters. Can specify number of PCs with second number. (e.g., 4,3 gives 4 clusters, 3 components)"),
  make_option(c("-r", "--randsample"), type="numeric", default= 1,
              help="Train:Test random split - [default = %default] gives no split; Select 0 for pre-defined Train/Test Split. (ie. 0.8 gives 80% train)"),
  make_option(c("-s", "--seed"), type="numeric", default= 42,
              help="Optional: specify the seed for random sample split. [Default seed = %default]"),
  make_option(c("-q", "--crossvalidation"), action="store_true", default= FALSE,
              help="Performs leave-one-out CV and K-fold CV (5 iterations) on training data; external validation on test dataset."),
  make_option(c("-K", "--Kfoldvalue"), type="numeric", default= 5,
              help="To adjust K-fold value - [default = %default]"),
  make_option(c("-d", "--diagnostics"), action="store_true", default= FALSE,
              help="Diagnostics include: F-value comparisons, QSAR Test criteria, collinearity diagnostics, and outlier testing. Plots the top 5 largest residuals with Rstudent > 4."),
  make_option(c("-c", "--corrplot"), type="numeric", default= FALSE,
              help="Returns correlation plot between variables. Requires value between 0 and 1.") );

opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

# Start writing to an output file
filename = opt$outputfile
sink(filename, split = TRUE)
if (is.null(opt$inputfile)){
  print_help(opt_parser)
  stop(" o CSV file argument must be supplied (input file).csv", call.=FALSE)}

# Input Data
df1 = read.csv( opt$inputfile, header=TRUE, sep = ",", row.names = 1)
cat('********* ANALYSIS REQUESTED *********\n ')
cat(' -v', as.character(opt[1]),
    ' -i', as.character(opt[2]),
    ' -o', as.character(opt[3]),
    ' -x', as.character(opt[4]),
    ' -e', as.character(opt[5]),
    ' -m', as.character(opt[6]),
    ' -b', as.character(opt[7]),
    ' -y', as.character(opt[8]),
    ' -p', as.character(opt[9]),
    ' -r', as.character(opt[10]),
    ' -s', as.character(opt[11]),
    ' -q', as.character(opt[12]),
    ' -K', as.character(opt[13]),
    ' -d', as.character(opt[14]),
    ' -c', as.character(opt[15]) )

cat('\n\n***** RESULTS PROVIDED BY REGGAE *****\n')
if (opt$randsample == 0){
  cat("\no Using data option with pre-defined Test/Train split. \n")
  f = sapply(df1, levels)
  fnames = names(f)
  for (i in 1:length(fnames)){
    if (length(f[[fnames[i]]]) > 1) {
      cat(' Labels: ')
      cat_col = fnames[i]
      cat_levels = f[[fnames[i]]]
      cat(' ',cat_levels, '\n') }
  } } else { df = select_if(df1, is.numeric)}


# Data cleaning: remove constant columns
df = df1[vapply(df1, function(x) length(unique(x)) > 1, logical(1L))]
if ( ncol(df)<ncol(df1)){
  cat('\no Data cleaning: \n Columns dropped from dataset: ')
  for ( i in 1:ncol(df1) ){
    if ( vapply(df1, function(x) length(unique(x)) > 1, logical(1L))[i] == FALSE ){
      NA_col = names(vapply(df1, function(x) length(unique(x)) > 1, logical(1L))[i])
      cat(NA_col, ' ')} }
  cat('\n') }


# Define y-response variable
if (opt$yresponse == "None"){
  cat('\no Error: Missing y-variable. Define argument: -y \n\n')
  if ( opt$model != FALSE ){
    cat('  Required to run regression model.\n\n') }
  if ( opt$buildmodel != FALSE ){
    cat('  Required to run regression model.\n\n') }
  } else if ( c(opt$yresponse) %in% names(df) == FALSE) {
    y_var = FALSE
    cat('\no Variable not in dataframe: ', opt$yresponse,
               '\n Select from: ', c(names(df)), '\n\n')
  } else if (c(opt$yresponse) %in% names(df) == TRUE){
    y_var = TRUE
    colnames(df)[which(names(df) == opt$yresponse)] <- "y"
    if (opt$extdata != FALSE) {
      df_extdata = select_if(read.csv( opt$extdata, header=TRUE, sep = ",", row.names = 1),is.numeric)
      colnames(df_extdata)[which(names(df_extdata) == opt$yresponse)] <- "y"} }

# TRAIN/TEST split by predefined column or random sampling
if ( opt$randsample == 0 ){
  # Pre-defined test/train split
  ally_data <- df  # To store all data with y-defined as variable
  data_presplit <- split(df, df[cat_col])
  tr <- grep("TRAIN",cat_levels,ignore.case=TRUE,value=TRUE)
  tt <- grep("TEST",cat_levels,ignore.case=TRUE,value=TRUE)
  if ( length(tr) == 1 && length(tt) == 1 ){
    data_tr <- data_presplit[[tr]]
    data_tt <- data_presplit[[tt]]
    datatr_x <- select(data_tr, select = -c("y", all_of(cat_col)))
    datatt_x <- select(data_tt, select = -c("y", all_of(cat_col))) }
  else { cat('\no Error: Pre-defined data option needs column with defined "train" and "test"/"validation" labels.\n\n ') }
  cat('\no Using Pre-selected "Train" Samples: ', signif( nrow(data_tr)/nrow(df)*100 ,2), "%");
  if (length(data_presplit) > 2){
    data_val <- data_presplit[[3]]
    # vl <- grep("VALIDATION",cat_levels,ignore.case=TRUE,value=TRUE)
    dataval_x <- select(data_val, select = -c("y", all_of(cat_col)))
    cat('\n  Total Number of Validation Samples: ', nrow(data_val))}

  # Random splitting or Full data set
  } else if ( opt$randsample > 0 && opt$randsample <= 1 ){
    data_presplit = 0
    set.seed(opt$seed)
    df = select_if(df, is.numeric)
    train.index <- sample(nrow(df), (opt$randsample)*nrow(df), replace = FALSE)
    data_tr <- df[train.index,]
    data_tt <- df[-train.index,]
    datatr_x <- select(data_tr, select = -c("y"))
    datatt_x <- select(data_tt, select = -c("y"))
    if (opt$randsample != 1 && opt$pca == FALSE) { cat( '\no Using Random Sampling for "Train" Samples: ', signif( nrow(data_tr)/nrow(df)*100 ,2), "%"); }

    # PCA analysis and UTS split
    if (opt$pca != FALSE){
      pc_ks = strsplit(opt$pca, ",")
      ks = as.numeric(pc_ks[[1]][1]); pcs = as.numeric(pc_ks[[1]][2])
      df_pca <- data.frame(scale(select(df, select = -c("y"))))
      if (ncol(df_pca)>nrow(df_pca)) {
        cat('\no NOTE: Dataset includes more variables than samples. PC analysis will run with only the first', nrow(df_pca), 'variables.\n')
        pca <- princomp(df_pca[1:nrow(df_pca)])}
      else{ pca <- princomp(df_pca) }  # pca <- prcomp(df_pca)
      pca_sum = summary(pca)
      # Determine number of clusters using sum of squares within (variance between the groups)
      ssw <- (nrow(df_pca)-1)*sum(apply(df_pca,2,var))
      for (i in 2:length(pca_sum[["sdev"]])) ssw[i] <- sum(kmeans(df_pca,centers=i)$withinss)
      # Determines number of principal components
      if (ks > 1) {
        n_k= ks
        n_c= 2 # default of two principal components
        if (is.na(pcs) == FALSE) { n_c=pcs } }
      else{
        for (i in 1:length(pca_sum[["sdev"]])) { comp_i = pca_sum[["sdev"]][i] - pca_sum[["sdev"]][i+1]
        if (comp_i < 0.065) { n_c=i; break } }
        for (i in 2:length(pca_sum[["sdev"]])){ clust_i = abs(ssw[i] - ssw[i+1])
          if (clust_i < 15) { n_k=i; break}} }

      pc_only <- prcomp(df_pca)
      comp <- data.frame(pc_only$x[,1:n_c])
      km <- kmeans(comp, n_k, nstart=25, iter.max=1000)
      clust <- names(table(km$clust))
      cat("\no Principal Component Analysis \n  Number of PCs: ", n_c, "\n  K-means clusters: ", n_k, "\n")
      index<-list()
      for (i in 1:n_k){
        set = na.omit(df_pca[km$clust==clust[i],])
        cat("\n  Cluster ",clust[i], "set: ", nrow(set), "samples")
        assign( paste(i) , row.names(set) )
        if (opt$verbose == TRUE) { cat("\n  Set Samples: ",as.numeric(get(paste(i))), "\n") }
        set.seed(opt$seed)
        i_list <- as.numeric(match(get(paste(i)), rownames(df_pca)))
        index[[i]] <- sample(i_list, (opt$randsample)*length(get(paste(i))), replace = FALSE) }; cat("\n")

      if (opt$randsample > 0 && opt$randsample < 1){
        train.index = unlist(index, recursive = FALSE);
        cat( '\n\no Using PCA Splitting for "Train" Samples: ', signif( length(train.index)/nrow(df_pca)*100 ,2), "%");
        data_tr <- df[train.index,]
        data_tt <- df[-train.index,]
        datatr_x <- select(data_tr, select = -c("y"))
        datatt_x <- select(data_tt, select = -c("y")) }}

    # Using FULL data set
    if (opt$randsample == 1 && opt$pca == FALSE) {
      data_tr = select_if(df, is.numeric)
      datatr_x <- select(data_tr, select = -c("y"))
      cat('\no Using Full dataset: ' , signif( nrow(data_tr)/nrow(df)*100 ,2), "%");}}

# STANDARDIZE Data sets
# Splitting into y-response and x-variables of Train set
datatr_y <- data_tr["y"]
## find mean and sd column-wise of training data (on only numerical x-variables)
trainMean <- apply(datatr_x,2,mean)
trainSd <- apply(datatr_x,2,sd)
## centered AND scaled
datatr_scaled <- sweep(sweep(datatr_x, 2L, trainMean), 2, trainSd, "/")
data <- cbind(datatr_y,datatr_scaled )
# Standardize External set if given
if (length(data_presplit) > 2){
  dataval_y <- data_val["y"]
  dataval_scaled <- sweep(sweep(dataval_x, 2L, trainMean), 2, trainSd, "/")
  data_val <- cbind(dataval_y,dataval_scaled ) }
if (opt$extdata != FALSE){
  dataext_y = df_extdata["y"]
  dataext_x = select(df_extdata, select = -c("y"))
  dataext_xscaled <- sweep(sweep(dataext_x, 2L, trainMean), 2, trainSd, "/")
  data_extval <- cbind(dataext_y,dataext_xscaled)
  cat("\n  Total Number of External Samples: ", nrow(data_extval))}
# Standaridize Test set if split
if ( nrow(data_tt) != 0 ) {
  datatt_y <- data_tt["y"]
  datatt_scaled <- sweep(sweep(datatt_x, 2L, trainMean), 2, trainSd, "/")
  data_tt <- cbind(datatt_y,datatt_scaled ) }
cat("\n  Total Number of Training Samples: ", nrow(data_tr),
    "\n  Total Number of Testing Samples: ", nrow(data_tt),
    "\n  Total Number of Variables (including response): ", ncol(data_tr) , "\n")


# Correlation analysis
if (opt$corrplot) {
  # Data cleaning since using whole df
  num_data = select_if(df, is.numeric)
  precorr = num_data[vapply(num_data, function(x) length(unique(x)) > 1, logical(1L))]

  if (opt$corrplot >= 1){
    corr = "None"
    cat('\no Error: Correlation value must be between 0 and 1.\n')
  } else {
    set_value = opt$corrplot
    if ( opt$randsample != 0 ){
    corr = round(cor(precorr), 2) ## MAYBE EDIT TO precorr
    res <- cor.mtest(precorr, conf.level = .95) }  ## MAYBE EDIT TO precorr
    # for predefined test/train split data
    if ( opt$randsample == 0 ){
      corr = round(cor(precorr), 2)
      res <- cor.mtest(precorr, conf.level = .95) }
    cat('\no Correlations >', set_value,'on Full dataset between variables: \n')
    hyper_grid <- expand.grid(
      a = 1:nrow(corr[,] ),
      b = 1:ncol(corr[,] ) )
    for ( i in 1:nrow(hyper_grid)  ) {
      var1 = hyper_grid$a[i]
      var2 = hyper_grid$b[i]
      if (var1>var2) {
        if (abs( corr[var1,][var2] ) > set_value && abs( corr[var1,][var2]) < 1.0 ) {
          cat(" ",paste(names(corr[var1,][var2]), "-", names(corr[var2,][var1]) ),
              "=", corr[var1,][var2], "\n" ) } } }} }


# Define formula
find_formula <- function(model){
  as.formula( paste('     y ~',paste( sprintf("%s",names(coefficients(model)[-1]) ) , collapse=" + "))) }


# Build Linear Model
if (opt$buildmodel != FALSE){
  data = select_if(data, is.numeric)
  m = strsplit(opt$buildmodel, ",")
  m.names = m[[1]]
  data = data[, (colnames(data)) %in% c("y", m.names ) ] # with scaled values
  data_tr = data_tr[, (colnames(data_tr)) %in% c("y", m.names ) ] # with unscaled values
  if (nrow(data_tt) > 0){
    data_tt = data_tt[, (colnames(data_tt)) %in% c("y", m.names ) ] }
  if (length(data_presplit) > 2){
    data_val = data_val[, (colnames(data_val)) %in% c("y", m.names ) ] }

  model <- lm(y ~ ., data=data)
  model_uns <- lm(y ~., data=data_tr)  # unscaled values
  summary_best1 = summary(model)
  features = summary_best1$coefficients[,0]
  cat('\n\no Linear regression model with SELECTED features: ',
      '\n  Number of features (including response): ', nrow(features), '\n\n')

  cat(' a) scaled coefficients:\n ', paste("y = ", round(coefficients(model)[1],3), " + ",
                    paste( sprintf("%.3f * %s",coefficients(model)[-1],names(coefficients(model)[-1])), collapse=" + ") ), ' \n')

  cat('\n b) unscaled:\n ', paste( "y =", round(coef(model_uns)[1],2), "+"), paste(sprintf("%.2f * %s",coef(model_uns)[-1],names(coef(model_uns)[-1])), collapse=" + "))
  cat('\n\n  R2-train = ', signif( summary_best1$r.squared, 2),
      '\n  adj R2-train = ', signif( summary_best1$adj.r.squared, 2),
      '\n   RMSE-train = ', signif(rmse(data_tr$y, model$fitted.values),2), '\n\n')
  # Random Forest Model
  if (opt$model == "opt-rforest"  && y_var == TRUE) {
    print('OPTIMAL RFOREST')
    # Optimize parameters: default hyper_grid settings
    hyper_grid.all <- expand.grid(
      mtry       = c(3:(ncol(data)-1)),
      ntree      = c(300:500), RMSE= 0, r2_train= 0, r2_test= 0, r2_val= 0 )
    for(i in 1:nrow(hyper_grid.all)) {
      set.seed(opt$seed)
      # train model
      RFmodel <- randomForest(formula= y ~ ., data= data, importance= TRUE, ntree= hyper_grid.all$ntree[i], mtry= hyper_grid.all$mtry[i] )
      hyper_grid.all$RMSE[i] <- sqrt(mean(RFmodel$mse)) # OOB RMSError
      pred_r2 <- predict(RFmodel, data[-1])
      hyper_grid.all$r2_train[i] <- cor(data$y, pred_r2)^2 # R2-train
      if (nrow(data_tt) > 0){
        pred_tt2 <- predict(RFmodel, data_tt[-1])
        hyper_grid.all$r2_test[i] <- cor(data_tt$y, pred_tt2)^2 } #R2-test
      if (length(data_presplit) > 2){
        pred_val2 <- predict(RFmodel, data_val[-1])
        hyper_grid.all$r2_val[i] <- cor(data_val$y, pred_val2)^2 }}
    # lists top 10 combinations with least Out-of-Bag RMSE
    if (length(data_presplit) > 2){
      top10.all = head( arrange( hyper_grid.all, desc(r2_val) ), 10) }
    else { top10.all = head( arrange( hyper_grid.all, desc(r2_test) ), 10)}  # desc(r2_test) or RMSE
    # Optimal model
    set.seed(opt$seed)
    opt_model <- randomForest(y ~., data = data, mtry = top10.all$mtry[1], ntree = top10.all$ntree[1], importance = TRUE)
    tr.rsq=double(1); tr.rmse=double(1); tt.rsq=double(1); tt.rmse=double(1); val.rsq=double(1); val.rmse=double(1)
    RF_predicted_tr <- predict(opt_model, data[-1])
    tr.rsq[1] = cor(data$y, RF_predicted_tr)^2 #R2-train pred
    tr.rmse[1] = rmse(data$y, RF_predicted_tr) #RMSE-train pred
    train.frame = tibble(RF_predicted_tr, data$y)
    cat('\n\no Optimal Random Forest model with SELECTED features:')
    cat('\n  R2-train = ', signif(tr.rsq[1],2),
        '\n   RMSE = ', signif(tr.rmse[1],2))
    if (nrow(data_tt) > 0){
      RF_predicted_tt <- predict(opt_model, data_tt[-1])
      tt.rsq[1] = cor(data_tt$y, RF_predicted_tt)^2 #R2-test pred
      tt.rmse[1] = rmse(data_tt$y, RF_predicted_tt) #RMSE-test pred
      test.frame = tibble(RF_predicted_tt, data_tt$y)
      cat('\n  R2-test = ', signif(tt.rsq[1],2),
          '\n   RMSE = ', signif(tt.rmse[1],2)) }
    if (length(data_presplit) > 2){
      RF_predicted_val <- predict(opt_model, data_val[-1])
      val.rsq[1] = cor(data_val$y, RF_predicted_val)^2 #R2-validation pred
      val.rmse[1] = rmse(data_val$y, RF_predicted_val) #RMSE-val pred
      val.frame = tibble(RF_predicted_val, data_val$y)
      cat('\n  R2-external = ', signif(val.rsq[1],2),
          '\n   RMSE = ', signif(val.rmse[1],2)) }
    if (opt$extdata != FALSE){
      RF_predicted_val <- predict(opt_model, data_extval[-1])
      val.rsq[1] = cor(data_extval$y, RF_predicted_val)^2 #R2-val pred
      val.rmse[1] = rmse(data_extval$y, RF_predicted_val) #RMSE-val pred
      val.frame = tibble(RF_predicted_val, data_extval$y)
      cat('\n  R2-external = ', signif(val.rsq[1],2),
          '\n   RMSE = ', signif(val.rmse[1],2), '\n\n') }}

    else if (opt$model == "rforest" && y_var == TRUE){
    print('RFOREST')
    set.seed(opt$seed)
    opt_model <- randomForest(y ~., data = data, importance = TRUE) #}

    tr.rsq=double(1); tr.rmse=double(1); tt.rsq=double(1); tt.rmse=double(1); val.rsq=double(1); val.rmse=double(1)
    RF_predicted_tr <- predict(opt_model, data[-1])
    tr.rsq[1] = cor(data$y, RF_predicted_tr)^2 #R2-train pred
    tr.rmse[1] = rmse(data$y, RF_predicted_tr) #RMSE-train pred
    train.frame = tibble(RF_predicted_tr, data$y)
    cat('\n\no Random Forest model with SELECTED features:')
    cat('\n  R2-train = ', signif(tr.rsq[1],2),
        '\n   RMSE = ', signif(tr.rmse[1],2))
    if (nrow(data_tt) > 0){
      RF_predicted_tt <- predict(opt_model, data_tt[-1])
      tt.rsq[1] = cor(data_tt$y, RF_predicted_tt)^2 #R2-test pred
      tt.rmse[1] = rmse(data_tt$y, RF_predicted_tt) #RMSE-test pred
      test.frame = tibble(RF_predicted_tt, data_tt$y)
      cat('\n  R2-test = ', signif(tt.rsq[1],2),
          '\n   RMSE = ', signif(tt.rmse[1],2)) }
    if (length(data_presplit) > 2){
      RF_predicted_val <- predict(opt_model, data_val[-1])
      val.rsq[1] = cor(data_val$y, RF_predicted_val)^2 #R2-val pred
      val.rmse[1] = rmse(data_val$y, RF_predicted_val) #RMSE-val pred
      val.frame = tibble(RF_predicted_val, data_val$y)
      cat('\n  R2-external = ', signif(val.rsq[1],2),
          '\n   RMSE = ', signif(val.rmse[1],2), '\n\n') }
    if (opt$extdata != FALSE){
      RF_predicted_val <- predict(opt_model, data_extval[-1])
      val.rsq[1] = cor(data_extval$y, RF_predicted_val)^2 #R2-val pred
      val.rmse[1] = rmse(data_extval$y, RF_predicted_val) #RMSE-val pred
      val.frame = tibble(RF_predicted_val, data_extval$y)
      cat('\n  R2-external = ', signif(val.rsq[1],2),
          '\n   RMSE = ', signif(val.rmse[1],2), '\n\n') }
    if (opt$exportdata == "predicted") {
      write.csv(train.frame, "train-RF-predicted.csv")
      write.csv(test.frame, "test-RF-predicted.csv") }
  } } ### END of Random Forest ###



# Type of Model:
# Forward Step-wise feature selection
if (opt$model != FALSE){
  data = select_if(data, is.numeric)
  if (opt$model == "stepwise" && y_var == TRUE) {
      null1=lm(y ~1, data=data)
      full1=lm(y ~ . , data=data)
      model=step(null1, scope=list(lower=null1, upper=full1), direction="forward", trace=0)
      summary_best1 = summary(model)
      features = summary_best1$coefficients[,0]
      data_tr = data_tr[ c( names( model$model)[ ]) ] # unscaled values
      model_uns <- lm(y~., data=data_tr)
      cat('\n\no Forward Step-wise features selected for linear regression: ',
          '\n  Number of features (including response): ', nrow(features), '\n\n')
      # formulas
      cat(' a) scaled coefficients:\n ', paste("y = ", round(coefficients(model)[1],2), " + ",
                                               paste( sprintf("%.2f * %s",coefficients(model)[-1],names(coefficients(model)[-1])), collapse=" + ") ), ' \n')
      cat('\n b) unscaled:\n ', paste( "y =", round(coef(model_uns)[1],2), "+"), paste(sprintf("%.2f * %s",coef(model_uns)[-1],names(coef(model_uns)[-1])), collapse=" + "))
      cat('\n\n  R2-train= ', signif( summary_best1$r.squared, 2),
          '\n  adj R2-train = ', signif( summary_best1$adj.r.squared, 2),
          '\n   RMSE-train = ', signif(rmse(data_tr$y, model$fitted.values),2),
          '\n\n')
  # dredge function feature selection
  } else if (opt$model == "dredge" && y_var == TRUE) {

    full.model <- lm(y ~ ., data=data)
    options(na.action = "na.fail")
    all.models <- dredge(full.model, extra = c("R^2", F = function(x)
      summary(x)$fstatistic[[1]]))
    desc_weights = importance(all.models)
    top_five = head(all.models, 5)
    model = get.models(all.models, 1)[[1]]
    summary_best1 = summary(model)
    features = summary_best1$coefficients[,0]
    data_tr = data_tr[ c( names( model$model)[ ]) ] # unscaled values
    model_uns <- lm(y~., data=data_tr)

    cat('\n\no Dredge function features selected for linear regression:',
        '\n  Number of features (including response): ', nrow(features), '\n\n')
    # formulas
    cat(' a) scaled coefficients:\n ', paste("y = ", round(coefficients(model)[1],2), " + ",
                                             paste( sprintf("%.2f * %s",coefficients(model)[-1],names(coefficients(model)[-1])), collapse=" + ") ), ' \n')
    cat('\n b) unscaled:\n ', paste( "y =", round(coef(model_uns)[1],2), "+"), paste(sprintf("%.2f * %s",coef(model_uns)[-1],names(coef(model_uns)[-1])), collapse=" + "))
    cat('\n\n  R2-train = ', signif( summary_best1$r.squared, 2),
        '\n  adj R2-train = ', signif( summary_best1$adj.r.squared, 2),
        '\n   RMSE-train = ', signif(rmse(data_tr$y, model$fitted.values),2),
        '\n\n')
  # linear regression model
  } else if (opt$model == "full" && y_var == TRUE) {
    model <- lm(y ~ ., data=data)
    model_uns <- lm(y ~., data=data_tr)  # unscaled values
    summary_best1 = summary(model)
    features = summary_best1$coefficients[,0]
    cat('\n\no Linear regression model with ALL variables: ',
        '\n  Number of features (including response): ', nrow(features), '\n\n')
    # formulas
    cat(' a) scaled coefficients:\n ', paste("y = ", round(coefficients(model)[1],2), " + ",
                                             paste( sprintf("%.2f * %s",coefficients(model)[-1],names(coefficients(model)[-1])), collapse=" + ") ), ' \n')
    cat('\n b) unscaled:\n ', paste( "y =", round(coef(model_uns)[1],2), "+"), paste(sprintf("%.2f * %s",coef(model_uns)[-1],names(coef(model_uns)[-1])), collapse=" + "))
    cat('\n\n  R2-train = ', signif( summary_best1$r.squared, 2),
        '\n  adj R2-train = ', signif( summary_best1$adj.r.squared, 2),
        '\n   RMSE-train = ', signif(rmse(data_tr$y, model$fitted.values),2),
        '\n\n')
    # reduced correlation-type model
    } else if (opt$model == "mincorr" && y_var == TRUE ) {
      if (opt$corrplot != FALSE){
        # computes the correlation among the x-vars based on cut-off value
        c_value = opt$corrplot
        comat = cor(data[,-1])
        hc = findCorrelation(comat, cutoff=c_value)
        hc = sort(hc) + 1
        data = data[,-c(hc)]

        model <- lm(y ~ ., data=data)
        summary_best1 = summary(model)
        features = summary_best1$coefficients[,0]
        data_tr = data_tr[ c( names( model$model)[ ]) ] # unscaled values
        model_uns <- lm(y~., data=data_tr)
        cat('\n\no Linear regression model with REMOVED highly-correlated variables: ',
            '\n  Number of features (including response): ', nrow(features), '\n\n')
        # formulas
        cat(' a) scaled coefficients:\n ', paste("y = ", round(coefficients(model)[1],2), " + ",
                                                 paste( sprintf("%.2f * %s",coefficients(model)[-1],names(coefficients(model)[-1])), collapse=" + ") ), ' \n')
        cat('\n b) unscaled:\n ', paste( "y =", round(coef(model_uns)[1],2), "+"), paste(sprintf("%.2f * %s",coef(model_uns)[-1],names(coef(model_uns)[-1])), collapse=" + "))
        cat('\n\n  R2-train = ', signif( summary_best1$r.squared, 2),
            '\n  adj R2-train = ', signif( summary_best1$adj.r.squared, 2),
            '\n   RMSE-train = ', signif(rmse(data_tr$y, model$fitted.values),2),
            '\n\n')
      } else { cat('\no Error: Need to define correlation cut off value (between 0 and 1) with -c\n')
      }} }


# Cross-Validation:
if (opt$crossvalidation != FALSE) {
  formula = as.formula( paste('y ~',
                    paste( sprintf("%s",  names(coefficients(model)[-1]) ) , collapse=" + ")))
  data_train = data[ c( names( model$model)[ ]) ]
  # Train validation - LOO-q2
  cat('\n\no Cross Validation for model ( k = ',opt$Kfoldvalue,'): \n')
  loo_q2 <- looq2(data_train, formula)
  cat('  LOO-q2 (train) = ', signif(loo_q2@result[["pred"]][["q2"]], 2) , '\n')
  # K-fold validation (default k=5)
  K = opt$Kfoldvalue
  iK<-list()
  for (i in 1:5){
    k_q2 = cvq2( data_train, formula, nFold = K)
    iK[[i]] <- signif(k_q2@result[["pred"]][["q2"]], 2)
    if (opt$verbose == TRUE) { cat('  Iteration ', i,': Kfold-q2 = ', iK[[i]],'\n') } }
  iK = unlist(iK, recursive = FALSE);
  iK_mean = signif(mean(iK),2)
  iK_sd = signif(sd(iK),1)
  cat('  Avg Kfold-q2 (train) = ', iK_mean , '( +/-', iK_sd,')\n')
  cat('   RMSE = ', signif(k_q2@result[["pred"]][["rmse"]], 2), '\n')
  # Test validation
  if ( nrow(data_tt) > 0) {
    data_test <- data_tt[ c(names(model$model)[ ])]
    tt_q2<- q2(data_train, data_test, formula ) # add extOut = TRUE (if want to print out details)
    cat('  q2 (test) = ', signif(tt_q2@result[["pred"]][["q2"]], 2), '\n')
    cat('   RMSE-test = ', signif(tt_q2@result[["pred"]][["rmse"]], 2), '\n') }
  # External validation: a) from initial data
  if (length(data_presplit) > 2){
    data_validation <- data_val[ c(names(model$model)[ ])]
    extval_q2<- q2(data_train, data_validation, formula ) # add extOut = TRUE (if want to print out details)
    cat('  q2 (external) = ', signif(extval_q2@result[["pred"]][["q2"]], 2), '\n')
    cat('   RMSE-ext = ', signif(extval_q2@result[["pred"]][["rmse"]], 2), '\n') } }


# External Data provided (Validation set)
if (opt$extdata != FALSE) {
  formula = as.formula( paste('y ~',
                              paste( sprintf("%s",  names(coefficients(model)[-1]) ) , collapse=" + ")))
  data_train = data[ c( names( model$model)[ ]) ]
  data_extval = data_extval[ c( names( model$model)[ ]) ]
  extval_q2 <- q2(data_train, data_extval, formula)
  cat('  q2 (external) = ', signif(extval_q2@result[["pred"]][["q2"]], 2), '\n')
  cat('   RMSE-ext = ', signif(extval_q2@result[["pred"]][["rmse"]], 2), '\n') }


# Diagnostics includes: F-value comparisons, Variance Inflation Factor, and Outlier Testing
if (opt$diagnostics != FALSE ) {
  full.model <- lm(y ~ ., data=data)
  full.model.sum = summary(full.model)
  cat('\n\no Diagnostic results:')
  # I. Inspired by ANOVA analysis (Principle of Conditional Error)
  fvalue_full = full.model.sum$fstatistic[1]; fvalue_full
  fvalue_red = summary_best1$fstatistic[1]
  # fdiff = fvalue_red - fvalue_full
  cat('\nI. Reduced model significance: ',
      "\n  F-value full = ", signif(fvalue_full,4),
      "\n  F-value reduced = ", signif(fvalue_red,4), "\n")
  # II. Predicition Power - Test criteria
  cat('\nII. QSAR model criteria for Test data set:')
  if ( nrow(data_tt) == 0 ) { cat('\n Error: Missing a Test data set. Need to define or random sample.\n') }
  data_tt$y_hat <- predict(model, data_tt)
  # Test R2
  r2_test <- round( cor(data_tt$y_hat, data_tt$y)^2, 3)
  # K-slopes
  k <- round( sum( (data_tt$y)*(data_tt$y_hat) ) / sum( (data_tt$y_hat)^2 ) , 2)
  k_pred <- round( sum( (data_tt$y)*(data_tt$y_hat) ) / sum( (data_tt$y)^2 ) , 2)
  # R2 ratio differences
  data_tt$y0 <- k*(data_tt$y_hat)
  data_tt$y0_pred <- k_pred*(data_tt$y)
  r2.0.actual <- 1 - ( sum((data_tt$y_hat - data_tt$y0)^2)/sum((data_tt$y_hat - mean(data_tt$y_hat))^2) )
  r2.0.predicted <- 1 - ( sum((data_tt$y - data_tt$y0_pred)^2)/sum((data_tt$y - mean(data_tt$y))^2) )
  r_act = round((r2_test - r2.0.actual)/ r2_test, 3)
  r_prd = round((r2_test - r2.0.predicted)/ r2_test, 3)
  cat("\n  Satisfactory conditions include: \n  R2-test > 0.6; R2-diff < 0.1; 0.85 <= k <= 1.15 or 0.85 <= k'<= 1.15; \n",
      '\n  R2 = ', r2_test,
      '\n  k = ', k,
      "\n  k' = ", k_pred,
      '\n  R2-diff (actual) = ', r_act,
      '\n  R2-diff (pred) = ', r_prd, '\n')
  # # III. Outlier testing
  # # CRITERIA FOR OUTLIER (Cook's distance >1 and standardized residuals > 2.5)
  # cat('\nIII. Outlier diagnostics for reduced model: \n Top 5 largest residuals: \n')
  # #ally_data = select_if(ally_data, is.numeric) ## EDIT TO REVIEW ALL RESIDUALS ##
  # Temp <- data.frame(Sample = row.names(data), y = data$y,
  #                    RawRes = residuals(model), # raw residuals
  #                    RStandard = rstandard(model), # standardized residuals
  #                    RStudent = rstudent(model), #rstudent residuals
  #                    CooksD = cooks.distance(model))
  # Influence_Sample <- arrange(Temp, desc(abs(RStudent))) # Reorder data by Rstudent
  # limit_inf = subset(Influence_Sample, abs(RStudent) < 2)
  # Out_Sample <- arrange(Temp, desc(abs(RStandard))) # Reorder by Rstandardized
  # limit_Temp1 = subset(Out_Sample, abs(RStandard) > 2.5)
  # limit_Temp = subset(limit_Temp1, abs(CooksD) > 1)
  # print(head(Influence_Sample, 5) )
  # # Standar deviation and mean of raw residuals based on Influence points
  # RawRes <- limit_inf["RawRes"]
  # ResMean <- apply(abs(RawRes),2,mean)
  # ResSd <- apply(RawRes,2,sd)
  # limit = 4*ResSd + ResMean
  # cat('\n Raw residuals summary \n Mean: ', ResMean, '\n SD: ', ResSd, '\n Max raw residual limit: ',limit)
  #
  # cat('\n\n Outlier criteria either: \n (a) Raw residual > four times standard deviation from mean of residulas. \n (b) or, Cooks distance > 1 AND standardized residuals > 2.5.\n')
  # for ( i in 1:nrow(Temp)){
  #   if ( abs(Temp[[3]][i]) > limit) { result = ""; cat('\n Possible outlier(s) per criteria in (a): ', as.character(Temp[[1]][i]), '(',Temp[[3]][i] ,')') }
  #   else { result = "\n Passed criteria in (a)."} }
  #
  # cat(result)
  # if ( nrow(limit_Temp) == 0 ){
  #   cat('\n Passed criteria in (b). \n')}
  # # Cooks distance
  # else { cat('\n Possible outlier(s) per criteria in (b): ', as.character(limit_Temp[[1]]), '\n')}

  # IV. VIF - collinearity diagnostics
  cat('\nIV. Collinearity diagnostics for reduced model: \n  Note: VIF > 4 or 10 indicates collinearity.\n  Variables with collinearity: \n')
  if ( length(names(coefficients(model)[-1])) != 1){
    vif_red = vif(model)
    t = double(length(vif_red))
    for (i in 1:length(vif_red)) {
      if ( vif_red[i] > 4) {
        t[i] = vif_red[i]
        cat('\n     ', names(vif_red[i]), '=', signif(vif_red[i], 4), '  ') }}
    cat('\n')
    if ( sum(t) == 0) { cat('\n  No collinearity found within model. VIF < 4 \n\n') }
  } else { cat('\n  VIF requires more than 1 coefficient in the model to calculate. \n')}
}#end of diagnostics


### ITEMS TO ADD in diagnostics ####
# # II. Y-Randomization
# cat('\nII. Y-Randomization for 1000 random runs: \n')
# subdata = model$model
# n = length(subdata)-1
# yscram = scramb(subdata,1000,n, cercle = TRUE)


# Plotting regression model
reggae_plot <- function(model) {
  options(warn=-1)
  # Training data set (or full dataframe)
  predicted = round( predict(model, data), 2)
  actual = data$y
  plot_df = tibble(predicted, actual)
  # MV Linear Plot
  q_plot = ggplot(plot_df, aes( x = actual , y = predicted, color="Train") ) +
    geom_point(alpha=0.7) +
    xlab(paste(opt$yresponse, "- actual")) +
    ylab(paste(opt$yresponse, "- predicted")) +
    labs( title = paste("MV Linear Regression Model: R-squared = ", signif(summary_best1$r.squared, 2)), # summary_best1[["terms"]][[2]], "~",
          #subtitle = paste("y = ", paste( sprintf("%s",names(coefficients(model)[-1])), collapse=" + ") ),
          caption = paste("y = ", round(coefficients(model)[1],2), " + ",
                          paste( sprintf("%.2f * %s",coefficients(model)[-1],names(coefficients(model)[-1])), collapse=" +\n")) )  +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
    stat_smooth(method = "lm", col = "black") +
    scale_color_manual(values=c("#E69F00", "#999999", "#56B4E9")); q_plot
  # Cross Validation Plot
  if ( opt$crossvalidation != FALSE ) {
    ypt = signif(min(actual))
    xpt = signif(max(actual))
    cpt = abs((ypt-xpt)/8)
    r_plot = q_plot + annotate("text", x = xpt-cpt, y = ypt+cpt,
                               label = paste("italic(q-Kfold)^ 2 ==", iK_mean, "(",iK_sd,")"), parse=TRUE); r_plot
    # External Validation data set (b)
    if (opt$extdata != FALSE) {
      v.predicted = round( predict(model, data_extval), 2)
      v.actual = data_extval$y
      v.plot_df = tibble(v.predicted, v.actual)
      t_plot <- r_plot + geom_point(data=v.plot_df, aes(x=v.actual, y=v.predicted, color="Val.External"), alpha=0.7) +
        theme(legend.title = element_blank()) +
        annotate("text", x = xpt-cpt, y = ypt,
                 label = paste("italic(q-external) ^ 2 ==", signif(extval_q2@result[["pred"]][["q2"]], 2)), parse=TRUE); t_plot}}
  if ( nrow(data_tt) > 0 ) {
    if ( opt$crossvalidation == FALSE ) { q_plot
    } else {
      # Testing data set
      t.predicted = round( predict(model, data_tt), 2)
      t.actual = data_tt$y
      t.plot_df = tibble(t.predicted, t.actual)
      s_plot <- r_plot + geom_point(data=t.plot_df, aes(x=t.actual, y=t.predicted, color="Test"), alpha=0.7) +
        theme(legend.title = element_blank()) +
        annotate("text", x = xpt-cpt, y = ypt,
                 label = paste("italic(q-Test) ^ 2 ==", signif(tt_q2@result[["pred"]][["q2"]], 2)), parse=TRUE); s_plot
      # External Validation data set (a)
      if (length(data_presplit) > 2){
        v.predicted = round( predict(model, data_val), 2)
        v.actual = data_val$y
        v.plot_df = tibble(v.predicted, v.actual)
        x_plot <- s_plot + geom_point(data=v.plot_df, aes(x=v.actual, y=v.predicted, color="Val.External"), alpha=0.7) +
          theme(legend.title = element_blank()) +
          annotate("text", x = xpt-cpt, y = ypt-cpt ,
                   label = paste("italic(q-external) ^ 2 ==", signif(extval_q2@result[["pred"]][["q2"]], 2)), parse=TRUE); x_plot }
      # External Validation data set (b)
      if (opt$extdata != FALSE) {
        v.predicted = round( predict(model, data_extval), 2)
        v.actual = data_extval$y
        v.plot_df = tibble(v.predicted, v.actual)
        x_plot <- s_plot + geom_point(data=v.plot_df, aes(x=v.actual, y=v.predicted, color="Val.External"), alpha=0.7) +
          theme(legend.title = element_blank()) +
          annotate("text", x = xpt-cpt, y = ypt-cpt ,
                   label = paste("italic(q-external) ^ 2 ==", signif(extval_q2@result[["pred"]][["q2"]], 2)), parse=TRUE); x_plot}} }

  # Additional Possible "outliers" plot:
  # if (opt$diagnostics != FALSE && nrow(limit_Temp) != 0 ){
    # print(head(limit_Temp, 5) )
    # top5outliers_samples = limit_Temp[1:5,1]
    # top5outliers_data = na.omit( data[ as.character( top5outliers_samples),] )
    # print(top5outliers_data)

    # outx_actual = top5outliers_data$y
    # outy_pred = round(predict(model, top5outliers_data))
    # out_df = tibble(outy_pred, outx_actual)
    # o_plot = q_plot + geom_point(data=out_df, aes(x=outx_actual, y=outy_pred, color="Outliers")) +
    #   theme(legend.title = element_blank())
    # if ( opt$crossvalidation != FALSE ){ o_plot = r_plot + geom_point(data=out_df, aes(x=outx_actual, y=outy_pred, color="Outliers"), alpha=0.7) +
    #   theme(legend.title = element_blank()) }
    # if ( nrow(data_tt) > 0 ) {
    #   if ( opt$crossvalidation == FALSE ) { o_plot
    #   } else { o_plot = s_plot + geom_point(data=out_df, aes(x=outx_actual, y=outy_pred, color="Outliers"), alpha=0.7) +
    #     theme(legend.title = element_blank()) }
    # o_plot }}
  # Export predicted-actual data:
  if (opt$exportdata == "predicted") { write.csv(plot_df, "train_predicted-actual_REGGAE.csv")
    if ( nrow(data_tt) != 0 ){ write.csv(t.plot_df, "test_predicted-actual_REGGAE.csv") }} } ### END Plottting ###

# Export scaled data sets:
if ( opt$exportdata != FALSE){
  if (opt$exportdata == "scaled") {
    write.csv(data, "scaled_traindata_REGGAE.csv")
    if ( nrow(data_tt) != 0 ) { write.csv(data_tt, "scaled_testdata_REGGAE.csv") } }
  if (opt$exportdata == "unscaled"){
    uns_datax <- t((t(datatr_scaled) * trainSd) + trainMean)
    uns_data <- cbind(data_tr["y"], uns_datax)
    write.csv(uns_data, "unscaled_traindata_REGGAE.csv")
    if ( nrow(data_tt) != 0 ) {
      uns_datattx <- t((t(datatt_scaled) * trainSd) + trainMean)
      uns_datatt <- cbind(data_tt["y"], uns_datattx)
      write.csv(uns_datatt, "unscaled_testdata_REGGAE.csv") }}
  # Export predicted-actual data:
  if (opt$exportdata == "predicted") { reggae_plot(model) }}

# verbose
if ( opt$verbose ){
  cat('\n\n********** DETAILED RESULTS **********\n\no Dataset preview ( y =', opt$yresponse, ')\n')
  print(head(data, 5));

  if (opt$buildmodel != FALSE){
    cat('\no Linear Regression model with SELECTED features: \n')
    print(summary_best1)
    reggae_plot(model)
    cat("\no Regression plot(s) saved in working file. \n")
    ggsave("REGGAE-MV-linear-plot.png",dpi=300, width = 9, height = 8)

    if (opt$model == "rforest") {
      cat('\no Random Forest model with SELECTED features:')
      # cat('\n\nFrom a total of ', nrow(hyper_grid.all),' RF models, the Top 10 optimal models: \n')
      # print(top10.all); cat('\n')
      print(opt_model)
      # Random Forest Plot
      rforest_plot = ggplot(train.frame, aes( x = data$y, y = RF_predicted_tr, color="Train") ) +
        geom_point(alpha=0.7) +
        xlab(paste(opt$yresponse, "- actual")) +
        ylab(paste(opt$yresponse, "- predicted")) +
        labs( title = paste("Random Forest Model "),
              caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                              "\nNumber of Samples = ", (nrow(df)) )) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
        stat_smooth(method = "lm", col = "black") +
        scale_color_manual(values=c("#E69F00", "#999999", "#56B4E9"))
      if (opt$extdata != FALSE){
        rforest_plot_val <- rforest_plot + geom_point(data=val.frame, aes(x=data_extval$y , y=RF_predicted_val, color="Val.External"), alpha=0.7) +
          stat_smooth(method = "lm", col = "black") +
          labs( title = paste("Random Forest Model "),
                caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                                "\nR2-val = ", signif(val.rsq[1], 2), "\nRMSE-val = ", signif(val.rmse[1], 2), "\nNumber of Samples = ", (nrow(df)) )); rforest_plot_val }
      if ( nrow(data_tt) > 0 ){
        rforest_plot_tt <- rforest_plot + geom_point(data=test.frame, aes(x=data_tt$y , y=RF_predicted_tt, color="Test"), alpha=0.7) +
          stat_smooth(method = "lm", col = "black"); rforest_plot_tt
        if (length(data_presplit) > 2){
          rforest_plot_val <- rforest_plot_tt + geom_point(data=val.frame, aes(x=data_val$y , y=RF_predicted_val, color="Val.External"), alpha=0.7) +
            stat_smooth(method = "lm", col = "black") +
            labs( title = paste("Random Forest Model "),
                  caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                                  "\nR2-test = ", signif(tt.rsq[1], 2), "\nRMSE-test = ", signif(tt.rmse[1], 2),
                                  "\nR2-val = ", signif(val.rsq[1], 2), "\nRMSE-val = ", signif(val.rmse[1], 2), "\nNumber of Samples = ", (nrow(df)) )); rforest_plot_val }
        if (opt$extdata != FALSE){
          rforest_plot_val <- rforest_plot_tt + geom_point(data=val.frame, aes(x=data_extval$y , y=RF_predicted_val, color="Val.External"), alpha=0.7) +
            stat_smooth(method = "lm", col = "black") +
            labs( title = paste("Random Forest Model "),
                  caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                                  "\nR2-test = ", signif(tt.rsq[1], 2), "\nRMSE-test = ", signif(tt.rmse[1], 2),
                                  "\nR2-val = ", signif(val.rsq[1], 2), "\nRMSE-val = ", signif(val.rmse[1], 2), "\nNumber of Samples = ", (nrow(df)) )); rforest_plot_val }
        } else { rforest_plot }
      ggsave("REGGAE-RF-plot.png",dpi=300, width = 9, height = 8)}

    if (opt$model == "opt-rforest") {
      cat('\no Optimal Random Forest model with SELECTED features:')
      # cat('\n\nFrom a total of ', nrow(hyper_grid.all),' RF models, the Top 10 optimal models: \n')
      # print(top10.all); cat('\n')
      print(opt_model)
      # Random Forest Plot
      rforest_plot = ggplot(train.frame, aes( x = data$y, y = RF_predicted_tr, color="Train") ) +
        geom_point(alpha=0.7) +
        xlab(paste(opt$yresponse, "- actual")) +
        ylab(paste(opt$yresponse, "- predicted")) +
        labs( title = paste("Random Forest Model "),
              caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                              "\nNumber of Samples = ", (nrow(df)) )) +
        theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
        stat_smooth(method = "lm", col = "black") +
        scale_color_manual(values=c("#E69F00", "#999999", "#56B4E9"))
      if (opt$extdata != FALSE){
        rforest_plot_val <- rforest_plot + geom_point(data=val.frame, aes(x=data_extval$y , y=RF_predicted_val, color="Val.External"), alpha=0.7) +
          stat_smooth(method = "lm", col = "black") +
          labs( title = paste("Random Forest Model "),
                caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                                "\nR2-val = ", signif(val.rsq[1], 2), "\nRMSE-val = ", signif(val.rmse[1], 2), "\nNumber of Samples = ", (nrow(df)) )); rforest_plot_val}
      if ( nrow(data_tt) > 0 ){
        rforest_plot_tt <- rforest_plot + geom_point(data=test.frame, aes(x=data_tt$y , y=RF_predicted_tt, color="Test"), alpha=0.7) +
          stat_smooth(method = "lm", col = "black"); rforest_plot_tt
        if (length(data_presplit) > 2){
          rforest_plot_val <- rforest_plot_tt + geom_point(data=val.frame, aes(x=data_val$y , y=RF_predicted_val, color="Val.External"), alpha=0.7) +
            stat_smooth(method = "lm", col = "black") +
            labs( title = paste("Random Forest Model "),
                  caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                                  "\nR2-test = ", signif(tt.rsq[1], 2), "\nRMSE-test = ", signif(tt.rmse[1], 2),
                                  "\nR2-val = ", signif(val.rsq[1], 2), "\nRMSE-val = ", signif(val.rmse[1], 2), "\nNumber of Samples = ", (nrow(df)) )); rforest_plot_val }
        if (opt$extdata != FALSE){
          rforest_plot_val <- rforest_plot_tt + geom_point(data=val.frame, aes(x=data_extval$y , y=RF_predicted_val, color="Val.External"), alpha=0.7) +
            stat_smooth(method = "lm", col = "black") +
            labs( title = paste("Random Forest Model "),
                  caption = paste("mtry = ", signif(opt_model$mtry) , "\nntree = ", signif(opt_model$ntree) , "\n R2-train = ", signif(tr.rsq[1],2), "\nRMSE-train = ", signif(tr.rmse[1], 2),
                                  "\nR2-test = ", signif(tt.rsq[1], 2), "\nRMSE-test = ", signif(tt.rmse[1], 2),
                                  "\nR2-val = ", signif(val.rsq[1], 2), "\nRMSE-val = ", signif(val.rmse[1], 2), "\nNumber of Samples = ", (nrow(df)) )); rforest_plot_val }
        } else { rforest_plot }
      ggsave("REGGAE-RF-plot.png",dpi=300, width = 9, height = 8)} }

  if ( opt$model != FALSE && opt$model != "rforest"){
    cat('\no Linear regression model: \n')
    print(summary_best1)
    reggae_plot(model)
    cat("\no Regression plot saved in working file. \n")
    ggsave("REGGAE-MV-linear-plot.png",dpi=300) # , width = 9, height = 8
    if (exists("top_five")) {
      cat('\no DREDGE top 5 results: \n')
      print(top_five)
      #cat('\n Variable importance based on All possible models: ', desc_weights)
    }}

  if ( opt$diagnostics != FALSE){
    png('Diagnostics-plot-REGGAE.png')
    par(mfrow=c(2,2))
    plot(model, which=c(1:2,4:5) , width = 300, height = 400, pointsize = 12, units='mm', res = 300)
    dev.off()  }

  if (opt$pca != FALSE){
    cat('\no PC Analysis: \n')
    print(pca_sum)
    print(loadings(pca_sum))

    png('PCA-scree-components-plot-REGGAE.png')
    plot(pca, type='l', main="PCs Analysis") # verbose
    dev.off()

    png('PCA-clusters-REGGAE.png')
    palette(alpha(brewer.pal(9,'Set1'), 0.5))
    # plot(comp, col=km$clust, pch=16) #verbose show all PCs requested
    plot(pc_only$x[,1:2], col=km$clust, pch=16) #verbose only show PC1 vs PC2
    dev.off()

    png('PCA-scree-clusters-plot-REGGAE.png')
    plot(1:length(pca_sum[["sdev"]]), ssw, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares", main="Cluster Analysis")
    dev.off()}

  if ( opt$crossvalidation != FALSE ){
    cat('\no I. Leave-one-out Cross validation: \n')
    print(loo_q2)
    cat('\no II. K-fold Cross validation ( k =',opt$Kfoldvalue,'): \n')
    print(k_q2)
    if ( nrow(data_tt) > 0) {
      cat('\no III. Test set validation: \n')
      print(tt_q2)} }

  if ( opt$corrplot != FALSE ){
    cat('\no Correlation table for all variables: \n')
    print(corr)
    cat("\no Correlation plot saved in working file. \n")
    png(file = "REGGEA-correlation-plot.png",width = 200, height = 200, pointsize = 18, units='mm', res = 300)
    corrplot <- corrplot(corr, p.mat = res$p, sig.level = c(.001, .01, .05), pch.cex = .7,
                         insig = "label_sig",
                         pch.col = "white",
                         type = "upper",
                         order = "hclust",
                         tl.col = "black", # mycolors
                         tl.srt = 45, # (0 = horizontal, 45 = angled)
                         method="square",
                         cl.align = "l" ,
                         tl.cex = .5) # for large amounts of descriptors use 0.5
    dev.off() }
  # if ( opt$diagnostics != FALSE ){
  #   if ( length(names(coefficients(model)[-1])) != 1){
  #   cat('\no Collinearity diagnostics for reduced model: \n')
  #   print(vif_red) } }
  cat('******************END*****************\n\n') }

# Stop writing to the file
sink()
#write out.txt results
# Append to the file
sink(filename, append=TRUE)
