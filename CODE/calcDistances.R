#------------------ SETUP ----------------------------------------------

# gather all sequences into a giant matrix. Will be convenient later for similarity analysis
mat <- matrix(unlist(sequencesPasts), nrow = 12, byrow = FALSE)

calcDissMatrix <- function(my.method, these.obs){
  
  #--- gather relevant data for this bloc
  nobs <- these.obs # these are the observations obtained from d1[[k]]. Each refers to a sequence number
  this.mat <- mat[, nobs] # From the mat of all sequences, take only the columns for this bloc
  this.sequencesPast <- sequencesPasts[nobs] # technically this is redundant since these are in the mat, but just a check...
  this.sequencesFuture <- sequencesFutures[nobs] # these are not in the mat and should be retained
  this.timesOfsequencesPasts <- timesOfsequencesPasts[nobs] # not in the mat either. These are the dates associated with the sequences (more specifically, a string with the country name, year and month of the LAST OBSERVATION in the sequence. I..e, that's the time at which we make the forecast)
  # print(paste('this.timesOfsequencesPasts = ', head(this.timesOfsequencesPasts)))
  
  # if it's a sequence of zeros, just predict a 0. No need to waste time comparing it...
  zeros <- NULL
  for(k in 1:length(this.sequencesPast)){
    if(length( which(this.sequencesPast[[k]] ==0 | is.na(this.sequencesPast[[k]]))) == n) zeros <- c(zeros, k)
  }
  
  predictions <- data.frame(datePredMade = date(), 
                            method = my.method,
                            # currentValue =   unlist(lapply(this.sequencesPast[zeros], tail, n = 1L)), # this one is just a check...
                            prediction =  0,
                            countryYM =  unlist(this.timesOfsequencesPasts[zeros]))
  write.table(predictions, file = paste('Results/RawPredictions/predictions_', this.unit,'_s', step, '.csv', sep=''), append = T, sep=',' , row.names = F, col.names = F)
  
  # now remove them
  this.sequencesPast <- this.sequencesPast[-zeros] # technically this is redundant since these are in the mat, but just a check...
  this.sequencesFuture <- this.sequencesFuture[-zeros] # these are not in the mat and should be retained
  this.timesOfsequencesPasts <- this.timesOfsequencesPasts[-zeros] # not in the mat either. These are the dates associated with the sequences (more specifically, a string with the country name, year and month of the LAST OBSERVATION in the sequence. I..e, that's the time at which we make the forecast)
  this.mat <- this.mat[,-zeros]
  
  
  
  #--- Clean the mat
  # First, go through each column, fill missing obs, and normalize each column. Note that normalization is absolutely necessary. Otherwise sequences match on their sheer values, not their shape etc.
  colsToRm <- NULL
  for(i in 1:ncol(this.mat)){
    # Fill NAs, backwards and forwards
    this.mat[,i] <- na.locf(this.mat[,i], na.rm=F, fromLast=T)
    this.mat[,i] <- na.locf(this.mat[,i], na.rm=F, fromLast=F)
    if(sd(this.mat[,i])!=0 & !is.na(sd(this.mat[,i]))) this.mat[,i] <- (this.mat[,i] - mean(this.mat[,i]))/sd(this.mat[,i])
    # Remove the column if it only has NAs. Take note of the col number and remove it in the next step
    if(length(which(is.na(this.mat[,i])))==n) colsToRm <- c(colsToRm, i)
  }
  
  # remove NA columns
  if(length(colsToRm)>0){
    this.mat <- this.mat[, -colsToRm]
    this.sequencesPast <- this.sequencesPast[-colsToRm]
    this.sequencesFuture <- this.sequencesFuture[-colsToRm]
    this.timesOfsequencesPasts <- this.timesOfsequencesPasts[-colsToRm]
    
  }
  
  
  #--- for each method, calculate a similarity matrix of the sequences. This is the core
  # First, define a function apply that does not stop if there is an error. Note that I use pbapply here, but it's just apply with a progress bar
  
  lapply_with_error <- function(X,FUN,...){    
    lapply(X, function(x, ...) tryCatch(FUN(x, ...),
                                        error=function(e) NULL))
  }
  #this.mat <- as.matrix(this.mat)
  
  # This is the core line. For method 'my.method', apply a distance funtion; results are in the form of a distance matrix for all sequences in the bloc. I.e., entry i, j gives the distance between sequence i and sequence j
  D.list <- lapply_with_error(my.method,  FUN= function(x) diss(t(this.mat), x))
  
  # Replace empty entries with NA
  D.list[sapply(D.list, is.null)] <- NA
  
  # Define weights as the inverse of the distance^2. I.e., the more 'distant' (i.e., different) a sequence is, the less weight I put on it. The square is a meta-param that may or may not help. The idea is to really punish dissimilar sequences
  weights <- 1/(as.matrix(D.list[[1]])^2)
  
  # Sequences that are exactly similar will receive a weight=Infinity. This causes problems so replace it with a very large number
  weights[weights==Inf] <- 1000
  
  # Don't assign a weight to own sequence -> remove diag
  diag(weights) <- NA
  
  # Give a name to each row/col in the weight matrix
  colnames(weights) <- this.timesOfsequencesPasts
  row.names(weights) <- this.timesOfsequencesPasts
  
  
  # remove weights of future obs. Note that it was wasteful to compute them in the first place. Future code to be improved there...
  toNA <- NULL
  rn <- row.names(weights)
  rn <- strsplit(rn, split = '_')
  cn <- colnames(weights)
  cn <- strsplit(cn, split = '_')
  for(i in 1:nrow(weights)){
    countryi <- rn[[i]][9] # ninth entry in the name is the country
    timeIndexi <- rn[[i]][5] # fifth entry in the name is the year
    for(j in 1:ncol(weights)){
      countryj <- cn[[j]][9]
      timeIndexj <- cn[[j]][5]
      if(countryi == countryj | timeIndexi <= timeIndexj){ # don't use sequences from same unit, else might get almost perfect overlap between sequences 1:10 and 2:11, and don't use future sequences
        weights[i,j] <- 0
      }
    }
  }
  
  this.sequencesFuture[is.na(this.sequencesFuture)] <- 0
  # normalize the weights by row
  for(i in 1:nrow(weights)){
    weights[i,] <- weights[i,]/sum(weights[i,], na.rm=T)
  }
  weights[is.na(weights)] <- 0
  
  
  
  # So now I need to know what happens in t = n + 1 for these countries. that is stored in sequencesFuture
  #w1 <- weights[, !is.na(this.sequencesFuture)]
  #f1 <-unlist(this.sequencesFuture)[!is.na(this.sequencesFuture)]
  preds <-  weights%*%unlist(this.sequencesFuture)  # Note: this.sequencesFuture because I want to use the other obs. futures (but they happened in the past, don't worry!)
  
  predictions <- data.frame(datePredMade = date(), 
                            method = my.method,
                            #currentValue =   unlist(lapply(this.sequencesPast, tail, n = 1L)), # this one is just a check...
                            prediction =  as.numeric(preds),
                            countryYM =  unlist(this.timesOfsequencesPasts)[!is.na(this.sequencesFuture)])
  write.table(predictions, file = paste('Results/RawPredictions/predictions_', this.unit,'_s', step, '.csv', sep=''), append = T, sep=',' , row.names = F, col.names = F)
  #print(summary(predictions$prediction))
  
  # The below is unnecessary, but gives me an update on how well this iteration is doing
  df4 <- merge(df3, predictions, by='countryYM')
  df4$constantPred <-  mean(df4$output, na.rm=T)
  print('')
  if(printUpdates == T){
    print(paste('time of prediction:', Sys.time()))
    print(paste(my.method, 'MSE =', round(mean( (df4$prediction- df4$future)^2, na.rm=T),3)))
    print(paste('Constant MSE =', round(mean( (df4$constantPred- df4$future)^2, na.rm=T),3)))
  }
  cat(paste('unit = ', this.unit, Sys.time(), 'this.d=', this.d, 'nd1=', length(d1), 'step=', step),
      file = paste('Logs/log_s_', step,'_', this.unit, '.txt', sep=''), append=T)
  cat('\n', file = paste('Logs/log_s_',step,'_', this.unit, '.txt', sep=''), append=T) 
  
}




# Now we have gathered all possible sequences of n observations at the country level. I.e., we have Zimbabwe_month_1-month_n, Zimbabwe_month2_month+n+1, etc
# Ideally we'd like to throw all of these into the pot and compare them all to each other. But this is computationally too demanding.
# So we define a size of a partition of observations to compare to each other
if(this.unit=='pgm') size.of.bloc <-  10000  #ncol(mat) # The larger the better: the more sequences to compare to, the better the match, but computational cost. Note that a lot of these are discarded because full of 0s.
if(this.unit=='cm') size.of.bloc <-  10000# ncol(mat)/10 # fewer sequences are 'discarded', so need to choose a smaller, more manageable bloc
# Randomize the order in which sequences appear in the set
sequenceID <- sample(1:length(sequencesPasts),
                     length(sequencesPasts), replace=F)
# Now group these sequences into blocks for comparison purposes. E.g., if I have 10000 sequences and I want to include them into (i.e., compare them to) blocks of 1000 sequences only, I'll need to cut them into N sequences / size of block =10000/1000 = 10 partitions
cutoffFactor <- ceiling( seq(sequenceID) / size.of.bloc )
# Now d1 is a list of "blocs" of sequences. For example, d1[[1]] gives me a list of size.of.bloc sequences, which I'll compare to each other.  
d1 <- split(sequenceID, cutoffFactor)

# Choose methods to compare sequences. Here only DTWARP was used
my.methods <- c('DTWARP')

print('... Calculate sequence similarity')

# For a given method, calculate a distance matrix for each block of sequences. calcDissMatrix saves its predictions to predictions.csv
for(my.method in my.methods){
  print(my.method)
  for(this.d in 1:length(d1)){
    gc()
    print(paste('Analyzing sequence bloc', this.d,'out of ', length(d1)))
    these.obs <- d1[[this.d]]
    try(calcDissMatrix(my.method, these.obs))
  }
}

