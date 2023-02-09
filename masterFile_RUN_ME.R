
# This is the master file for the replication of Chadefaux (2020)'s contribution to ViEWS. 
# NB: calculating d istances will be extremely time consuming, esp. for PGM (days on a macbook pro). In the future, this could fairly easily be sent to a cluster to run in  about one hour.
# NB: most of the work is done in 1 and 2 below.

rm(list=ls())
set.seed(0)
#packrat::init() # see https://rstudio.github.io/packrat/walkthrough.html

# Load functions
source('CODE/Functions/tadda.R')  
source('CODE/load.data.R')

# Dependency management
#packrat::init("~/Dropbox/Github/conflictlab/Papers/Chadefaux_ViEWS_paper/VIEWS_Replication_Chadefaux/")
#packrat::snapshot()

#------------------ Load Packages ----------------------------------------------

library(arrow) # useful to import parquet files from Python
library(TSclust) # may need to reinstall rgl and knitr
library(zoo)
library(readr)
library(dplyr) 
library(tidyr)
library(speedglm)
library(ggplot2)


#------ Define some parameters -----------------

# Important: Choose pgm or cm below
#this.unit <- 'cm'
this.unit <- 'pgm'

# Do I want updates on MSE progression?
printUpdates <- FALSE

#----- Analysis starts here ------------
for(this.unit in c('cm', 'pgm')){
  
  # Loop through the "steps". The steps are specific to the ViEWS competition. They refer to how far ahead I want to predict. Step 2 =2 months ahead, etc.
  for(step in c(2:7)){ 
    print('')
    print('')
    print(paste('STEP', step))
    
    # 1. Cut data into sequences
    source('CODE/prepSequences.R')
    
    # 2. calculate distance between sequences (for each step), 
    # Saves predictions to predictions.csv
    source('CODE/calcDistances.R')
  }
  
  # 3. Import the predictions, calibrate using a simple lm on past data, and predict future data. Output a file of form 'predictionsChadefaux_cm_s5_forecastSet.csv'.
  # Also output a Predictions/results.csv, which reports various metrics such as MSE
  source('CODE/distancesToPred.R')
  
  
  # NOTE: the below is only relevant for the ViEWS competition.
  
  # 4. At this stage, all the important steps have been completed.
  # The below is just a bit of tidying up
  # First, merge all `prediction' files into a single csv file, with each step as one column, as per ViEWS instructions
  source('CODE/mergeFiles.R')
  
  # 5. just a bit of tidying up the files now
  source('CODE/cleanUp.R')

}

# 6. illustrative graphs (needs cleaning)
source('CODE/AnalysisForPaperGraphsEtc.R')

# Not run:
# 7. Identify dangerous shapes
# source('CODE/identifyDangerousShapes.R')



