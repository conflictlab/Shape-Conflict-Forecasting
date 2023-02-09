for(file in list.files('Predictions/', pattern='chadefaux')){
  print(file)
  
  t1 <- read.csv(paste('Results/PostPredictions/',file, sep=''))
  # remove entries with no predictions. Ugly way of removing months out of requested set
  del <- which(is.na(t1$chadefaux_s2)&
                 is.na(t1$chadefaux_s3)&
                 is.na(t1$chadefaux_s4)&
                 is.na(t1$chadefaux_s5)&
                 is.na(t1$chadefaux_s6)&
                 is.na(t1$chadefaux_s7))
  t1 <- t1[-del,]
  t1$X <- NULL
  
  dup <- which(duplicated(t1))
  if(length(dup) >0) t1 <- t1[-dup, ]
  
  write.csv(t1, file = paste('Results/PredictionsInViewsFormat/',file, '_clean.csv', sep=''))
}
