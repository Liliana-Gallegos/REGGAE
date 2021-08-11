#!/usr/local/bin/Rscript

##################################################
#### REGGAE - Regression Generator & Analyzer ####
#### SETUP code                               ####
#### AUTHOR: Liliana C. Gallegos              ####
#### EMAIL: lilianac.gallegos@colostate.edu   ####
##################################################


InsPack <- function(pack) { 
  for (i in 1:length(pack)) {
    if (!pack[i] %in% installed.packages()) { 
      cat("\nInstalling required package: \n", pack[i], "\n") 
      install.packages(pack[i], repos="https://cran.revolutionanalytics.com/")
      if ( !pack[i] == "car"){
        install.packages("car",dependencies=TRUE)
      }
    } else  { cat('\n Package already installed: ', pack[i], '\n') } 
  }
}
pack <- c("optparse", "corrplot", "bindr", "MuMIn", "cvq2", "dplyr","car", "ggplot2", "caret", "Metrics")  # "R1magic", "DEMOVA",  "magic" 
InsPack(pack)
