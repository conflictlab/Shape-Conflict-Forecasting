
# -------------- ANALYZE RESULTS ----------------------

for(this.step in c(2:7)){
  
  # Import the prediction file
  preds <- read_csv(paste('Results/RawPredictions/predictions_', this.unit,'_s', this.step, '.csv', sep=''))
  preds <- as.data.frame(preds)
  
  # name and format columns
  colnames(preds) <- c('datePredMade' ,
                       'method',
                       'prediction',
                       'unitYM'  # date of last obs in the seq for that unit
  )
  
  preds$datePredMade <- NULL # remove this, was useful to track the time the file was changed, but now is just taking up precious memory
  
  preds$unitYM <- as.character(preds$unitYM)
  preds$method <- as.character(preds$method)
  
  
  # split unitYM, a character vector, into its components
  mypreds <- preds %>%
    separate(unitYM, c("country_name", "cutDel", 'cutnDel', 'yearDel', 'year', 
                       'MonthDel', 'month', 'IDdel', 'unit'), "_", remove=FALSE)
  mypreds$cutDel <- mypreds$yearDel <- mypreds$MonthDel <- mypreds$cutnDel <- mypreds$IDdel <- NULL
  
  
  mypreds$unitYM <- NULL
  mypreds$country_name <- NULL
  mypreds$year <- as.numeric(mypreds$year)
  mypreds$month <- as.numeric(mypreds$month)
  mypreds$unit <- as.numeric(mypreds$unit)
  mypreds <- mypreds[order(mypreds$unit, mypreds$year, mypreds$month),]
  
  
  # Merge it back into main data, df1, to get info about the "future"
  df1 <- load.data(step = this.step, this.unit = this.unit)
  dfa <- df1[,c( 'unit', 'month_id', 'future', 'year', 'month', 'in_africa')]
  dfa$unit <- as.numeric(dfa$unit)
  dfa$year <- as.numeric(dfa$year)
  dfa$month <- as.numeric(dfa$month)
  df4 <- merge(mypreds, 
               dfa,
               by=c('unit', 'year', 'month'),
               all.x=T)
  
  
  
  # remove the dots in variable names; clean a bit
  names(df4) <- gsub("\\.", "_", names(df4))
  df4 <- df4[!is.na(df4$unit) & !is.na(df4$prediction),]
  df4 <- df4[order(df4$unit, df4$month_id),]
  
  
  # Define learning and testing samples, as per Views instructions
  forecast.set1 <- c(408, 444) - this.step
  forecast.set2 <- c(444, 480) - this.step
  forecast.set3 <- c(487, 487 + this.step + 1) - this.step
  
  for(forecast.set in list(forecast.set1, forecast.set2, forecast.set3)){
    print(forecast.set)
    
    df5 <- df4 # legacy
    
    
    df5$learn <- 0
    df5$learn[df5$month_id >= 121& df5$month_id <= forecast.set[1]] <- 1
    df5$test <- 0
    df5$test[df5$month_id > forecast.set[1] & df5$month_id <= forecast.set[2]] <- 1
    
    
    # Estimate a model with only a constant: y ~ alpha. This is just to compare our predictions (below) to a basic benchmark.
    lm.cons <- speedlm(future ~ 1, data=df5[df5$learn==1,], na.action = na.exclude)
    
    # estimate y ~ alpha + yhat, where yhat is the estimate based on the DTW analysis. 
    # This step is useful to calibrate our predictions from the previous step
    formula.shape <- as.formula(future ~ prediction)
    lm.shape <- speedlm(formula.shape, data = df5[df5$learn==1,], na.action = na.exclude)
    
    
    
    df5$pred.shape <-df5$pred.cons <- NA
    # predict each model on the testing set
    df5$pred.shape[df5$test==1] <- predict(lm.shape, newdata = df5[df5$test==1,])
    df5$pred.cons[df5$test==1] <- predict(lm.cons, newdata = df5[df5$test==1,])
    
    
    # report Out-of-sample performance metrics
    # First, MSE:
    with(df5[!is.na(df5$pred.shape) & !is.na(df5$pred.cons),], 
         print(paste('shape: ', mean((df5$future - df5$pred.shape)^2, na.rm=T))))
    
    with(df5[!is.na(df5$pred.shape) & !is.na(df5$pred.cons),], 
         print(paste('shape (africa): ', mean((df5$future[df5$in_africa==1] - df5$pred.shape[df5$in_africa==1])^2, na.rm=T))))
    
    with(df5[!is.na(df5$pred.shape) & !is.na(df5$pred.cons),], 
         print(paste('constant: ', mean((df5$future - df5$pred.cons)^2, na.rm=T))))
    
    with(df5[!is.na(df5$pred.shape) & !is.na(df5$pred.cons),], 
         print(paste('constant (Africa): ', mean((df5$future[df5$in_africa==1] - df5$pred.cons[df5$in_africa==1])^2, na.rm=T))))
    
    # Second, TADDA:
    with(df5[!is.na(df5$pred.shape)  & !is.na(df5$pred.cons) ,], 
         print(paste('tadda ens: ', tadda(pred = df5$pred.shape, obs = df5$future, epsilon = 0.048))))
    with(df5[!is.na(df5$pred.shape)  & !is.na(df5$pred.cons) ,],
         print(paste('tadda constant:', tadda(pred = df5$pred.cons, obs = df5$future, epsilon = 0.048))))
    
    # Export the data to transfer to ViEWS team
    df.toexport <- data.frame(unit_id =  df5$unit,
                              unit_type = this.unit,
                              step = this.step,
                              month_id = df5$month_id + this.step,
                              last_month_id_of_data_used = df5$month_id,
                              prediction = df5$pred.shape
    )
    df.toexport <- df.toexport[!is.na(df.toexport$prediction), ]
    colnames(df.toexport)[colnames(df.toexport)=='prediction'] <- paste('chadefaux_s',this.step,sep='' )
    
    write.csv(df.toexport, file = paste('Results/PostPredictions/predictionsChadefaux_',this.unit,'_s', this.step,'_', forecast.set[1]+this.step,'.csv', sep=''))
    
    # Calculate and export various metrics
    
    for(booti in 1:100){
      samp1 <- sample(1:nrow(df5), size = nrow(df5), replace = T)
      df6 <- df5[samp1, ]
      
      results <- data.frame(unit = this.unit,
                            this.step = this.step,
                            forecastSet = forecast.set[1],
                            model = c('shape', 'cons'),
                            mse = c( mean((df6$future - df6$pred.shape)^2, na.rm=T), 
                                     mean((df6$future - df6$pred.cons)^2, na.rm=T))  ,
                            mse.africa = c( mean((df6$future[df6$in_africa==1] - df6$pred.shape[df6$in_africa==1])^2, na.rm=T), 
                                            mean((df6$future[df6$in_africa==1] - df6$pred.cons[df6$in_africa==1])^2, na.rm=T))  ,
                            
                            tadda = c(tadda(pred = df6$pred.shape, obs = df6$future, epsilon = 0.048),
                                      tadda(pred = df6$pred.cons, obs = df6$future, epsilon = 0.048)),
                            tadda.africa = c(tadda(pred = df6$pred.shape[df6$in_africa==1], obs = df6$future[df6$in_africa==1], epsilon = 0.048),
                                             tadda(pred = df6$pred.cons, obs = df6$future, epsilon = 0.048))
      )
      
      
      write.table(results, file = 'Results/Metrics/metrics_bootstrapped.csv', append = T, sep=',' , row.names = F, col.names = F)
    }
  }
}



boots <- read.csv('Results/Metrics/metrics_bootstrapped.csv')
colnames(boots) <- c('unit', 'this.step', 'forecastSet', 'model', 'mse', 'mse.africa', 'tadda', 'tadda.africa')

library(reshape)

boots <- melt(boots, id=c( "forecastSet","model", 'this.step', 'unit'))

# This needs to be added manually because I dont have their bootstrap
mseForBench <- data.frame(
  forecastSet=rep(440, 12),
  model = rep('Benchmark', 12),
  this.step  =c(2:7, 2:7),
  unit=c(rep('cm',6), rep('pgm', 6)),
  variable = c(rep('mse.africa',6), rep('mse', 6)),
  value = c(0.778, 0.861, 0.914, 1.050, 1.07, 1.108,
            0.043526, 0.046589, 0.046589, 0.047856, 0.047827, 0.047342))
boots <- rbind(boots, mseForBench)

taddaForBench <- data.frame(
  forecastSet=rep(440, 12),
  model = rep('Benchmark', 12),
  this.step  =c(2:7, 2:7),
  unit=c(rep('cm',6), rep('pgm', 6)),
  variable = c(rep('tadda',6), rep('tadda', 6)),
  value = c(0.4608, 0.4950, 0.5180, 0.5745, 0.5875, 0.5868,
            0.13661, 0.14150, 0.14301, 0.14617, 0.14329, 0.14256))
boots <- rbind(boots, taddaForBench)


this.metric <- 'mse.africa'
for(my.unit in c('pgm', 'cm')){
  print(paste('Creating MSE and Tadda plots for ', my.unit))
  for(this.metric in c('mse.africa', 'tadda')){
    if(my.unit=='pgm' & this.metric=='mse.africa') this.metric <- 'mse' # that's because "in_africa" is coded differently in pgm...
    pdf(paste('Results/Figs/', gsub(pattern = '\\.', replacement = '', x = this.metric), '_', my.unit, '_boxplots.pdf', sep=''))
    {
      modelNames <- c("Benchmark", "Constant", "Shapes")
      modelnumbers <- 2:7
      names1 <- apply(expand.grid(modelNames, modelnumbers), 1, paste, collapse=" ")
      #      boots$model <- factor(boots$model, levels = c("Benchmark", "shape", "cons"))
      
      par(mar=c(5,8,1,2))
      plotdat <- boots[boots$unit==my.unit &
                         boots$variable==this.metric &
                         #boots$model!='Benchmark'&
                         boots$forecastSet>420&
                         boots$forecastSet<460,]
      plotdat$model <- factor(plotdat$model, levels = c('Benchmark', "cons", "shape"))
      
      boxplot( plotdat$value ~ plotdat$model*plotdat$this.step ,
               col=c('black', 'darkgrey', 'lightgrey'),
               outline=F, las=2, names=NULL,
               xaxt='n', xlab = '', ylab='', cex.axis=2,
               #at=c(1,2,
               #      4,5,
               #      7,8,
               #      10,11,
               #      13,14,
               #      16, 17
               #)
      )
      legend('topleft',
             legend = c('Benchmark', 'Constant', 'Shape'),
             fill = c('black', 'darkgrey', 'lightgrey'), cex=1.7)
      #library(fields)
      #xline(5, lty=2)
      this.side <- 2
      mtext(side = this.side, text = 2:7, at=seq(1.5,17, 3),
            cex=2, line=1)
      mtext(side = 1, text = 'Step',  cex=2.5, line=3)
      if(this.metric=='mse.africa') this.metric <- 'MSE'
      mtext(side = this.side, text = toupper(this.metric),  cex=2.5, line=6)
      #segments(x0 = 1, y0 = 0.22, x1 = 2, y1 = 0.22)
    }
    dev.off()
  }
}


for(myunit in c('cm', 'pgm')){
  for(mystep in 2:7){
    t1 <- t.test(boots$value[boots$unit==myunit & boots$model=='cons' & boots$this.step==mystep], boots$value[boots$unit==myunit & boots$model=='shape' & boots$this.step==mystep])
    print(t1$p.value)
    
    t1 <- wilcox.test(boots$value[boots$unit==myunit & boots$model=='cons' & boots$this.step==mystep], boots$value[boots$unit==myunit & boots$model=='shape' & boots$this.step==mystep])
    print(t1$p.value)
    print('')
    
  }
}

# t-tests
for(s in 2:7){
  print(t.test(boots$value[boots$unit=='cm' & boots$model=='cons' & boots$this.step==s], boots$value[boots$unit=='cm' & boots$model=='shape' & boots$this.step==s]))
  t.test(boots$value[boots$unit=='pgm' & boots$model=='cons' & boots$this.step==s], boots$value[boots$unit=='pgm' & boots$model=='shape' & boots$this.step==s])
}

