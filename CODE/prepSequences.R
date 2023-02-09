#------------------ IMPORT DATA ----------------------------------------------


# Define the STEP to train, predict and evaluate for. 

  print('')
  print(paste('UNIT = ', this.unit, '; STEP = ', step))
  print('')
  
  print('... Loading data')
  df1 <- load.data(step = step, this.unit = this.unit)
  
  
  #------------- Create all short sequences of n periods -------------------
  print('... Gather sequences')
  
  n <- 12 # parameter of number of periods to use. 12 month period.
  
  # Only keep a subset of the data columns, don't need all these columns
  df3 <- df1[, c('output', 'future', 'year', 'month', 'unit', 'month_id')]
  gc()
  
  df3 <- df3[order(df3$unit, df3$month_id), ]
  
  
  # create a cutting index that will be used later to split data into chunks
  maxLengthForACountry <- (max(df3$year) - min(df3$year) + 1)*12
  cutCode <- NULL
  for(i in 1:(maxLengthForACountry/n)){ 
    cutCode <- c(cutCode, rep(i, n))
  }
  
  
  # initialize variables
  mat <- sequencesFutures <- sequencesPasts <-timesOfsequencesPasts <-   NULL
  
  # create sequences : [1, n], [2, n+1], ..., [n-1, n+11]
  for(rep1 in 1:(n)){
    df3 <- df3[order(df3$unit, df3$year, df3$month), ]
    
    # Cut each country time series into chunks of size n
    #nb: # shift cut by rep1 to cover everything, so remove first rep1 obs from every country
    df3$cut <- NA
    df3$cut <- unlist(tapply(X = df3$cut, INDEX = list(df3$unit), 
                             FUN = function(x) c(rep(NA, rep1), cutCode[1:(length(x)-rep1)] )))
    df3$cutn <- paste(df3$unit, df3$cut, sep='_') # assigns a name (e..g, Ghana_19) to each sequence
    df3$countryYM <- paste(df3$country_name,
                           'Cut', df3$cut,
                           'Year', df3$year,
                           'Month', df3$month,
                           'ID', df3$unit, sep='_')
    
    # Get the sequences
    sequencesPast <- split(df3$output, df3$cutn)
    # get the time associated with the sequences (the last obs defines the "time")
    timesOfsequencesPast <- split(df3$countryYM, df3$cutn)
    # get the last time of each sequence:
    timesOfsequencesPast <- lapply(timesOfsequencesPast, tail, n = 1L)
    
    #--- Get the next observation associated with each sequence (i.e., the "future")
    # First, get the non-lagged sequences (i.e., 'future' sequences)
    sequencesFuture <- split(df3$future, df3$cutn)
    
    # Now extract the last element of these "future" list, which is the "future" of the main list (sequencesPast)
    sequencesFuture <- lapply(sequencesFuture, tail, n = 1L)
    
    # Remove obs for which there is no "future" obs. IMPORTANT: this needs to be removed from all the sequences and associated dates, etc
    #toRm <- which(is.na(sequencesFuture))
    #sequencesFuture <- sequencesFuture[-toRm]
    #sequencesPast <- sequencesPast[-toRm]
    #timesOfsequencesPast <- timesOfsequencesPast[-toRm]
    
    # Now that we collected all sequences [rep1, n+rep1] for each country, we add them to the pile of all sequences
    sequencesFutures <- c(sequencesFutures, sequencesFuture)
    sequencesPasts <- c(sequencesPasts, sequencesPast)
    timesOfsequencesPasts <- c(timesOfsequencesPasts, timesOfsequencesPast)
    
    # End of loop; time to collect the next set of sequences, i.e., [rep1+1, n+rep1+1]
  }
  
  
  # remove the sequences that do not have 12 obs. These occur at the end of the data
  l1 <- lapply(X = sequencesPasts, FUN = length)
  l1 <- unlist(l1)
  seqNot12Long <- which(l1!=12)
  sequencesPasts <- sequencesPasts[-seqNot12Long]
  timesOfsequencesPasts <- timesOfsequencesPasts[-seqNot12Long]
  sequencesFutures <- sequencesFutures[-seqNot12Long]
  
  # just to check that they are all of length 12 (this is critical, so better make sure!)
  if(1==2){
    l1 <- lapply(X = sequencesPasts, FUN = length)
    l1 <- unlist(l1)
    seqNot12Long <- which(l1!=12)
    head(seqNot12Long) # should be none this time
  }
  
  